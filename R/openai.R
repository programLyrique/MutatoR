# nocov start
#' Identify equivalent mutants using OpenAI API
#'
#' Analyzes survived mutants to determine if they are functionally equivalent
#' to the original code using OpenAI's language models.
#'
#' @param src_file Path to the original source file
#' @param survived_mutants List of mutants that survived test execution
#' @param api_config Optional API configuration (will be loaded if NULL)
#'
#' @return Updated list of survived mutants with equivalence information
#'
#' @examples
#' src <- tempfile(fileext = ".R")
#' writeLines("add <- function(x, y) x + y", src)
#' survived <- list(mutant_001 = list(mutation_info = "x + y -> x - y"))
#' suppressWarnings(identify_equivalent_mutants(
#'     src,
#'     survived,
#'     api_config = list(api_key = "", model = "gpt-4")
#' ))
#'
#' @export
identify_equivalent_mutants <- function(src_file, survived_mutants, api_config = NULL) {
    # Load API configuration if not provided
    if (is.null(api_config)) {
        api_config <- get_openai_config()
    }

    # If no API key is available, return early
    if (is.null(api_config$api_key) || api_config$api_key == "") {
        warning("OpenAI API key not found. Skipping equivalent mutant detection.")
        return(survived_mutants)
    }

    # Read original source code
    orig_code <- paste(readLines(src_file), collapse = "\n")

    # Every mutant passed in is compared against this single `src_file`, so they
    # form one group. Keying by `basename(src_file)` avoids recovering the file
    # name from the mutant ID (which is unreliable: filenames contain '_').
    mutants_by_file <- list()
    mutants_by_file[[basename(src_file)]] <- survived_mutants

    # Track counts for each category
    equiv_count <- 0
    not_equiv_count <- 0
    unknown_count <- 0

    # Process each source file
    for (file_name in names(mutants_by_file)) {
        file_mutants <- mutants_by_file[[file_name]]

        # Prepare mutant information for the prompt. When the mutated source
        # file is available, include its full contents so the model reasons
        # about the actual code rather than inferring it from the change note.
        mutant_details <- lapply(names(file_mutants), function(mid) {
            m <- file_mutants[[mid]]
            mutated_code <- NULL
            if (!is.null(m$mutant_file) && file.exists(m$mutant_file)) {
                mutated_code <- tryCatch(
                    paste(readLines(m$mutant_file, warn = FALSE), collapse = "\n"),
                    error = function(e) NULL
                )
            }
            list(
                id = mid,
                mutation_info = m$mutation_info,
                mutated_code = mutated_code
            )
        })

        # Create the prompt
        prompt <- create_equivalent_mutant_prompt(orig_code, mutant_details)

        cat("\nAnalyzing mutants with OpenAI API...\n")
        cat("Prompt being sent to OpenAI:\n")
        cat("----------------------------------------\n")
        cat(prompt)
        cat("\n----------------------------------------\n\n")

        # Call OpenAI API
        response <- call_openai_api(prompt, api_config)

        # Process response
        if (!is.null(response)) {
            parsed <- response

            cat("Answer received from OpenAI API\n")
            cat("----------------------------------------\n")
            cat(parsed$choices[[1]]$message$content)
            cat("\n----------------------------------------\n\n")

            if (!is.null(parsed$choices) && length(parsed$choices) > 0) {
                # Extract the model's answer and parse it as structured JSON.
                equivalent_analysis <- parsed$choices[[1]]$message$content

                verdicts <- parse_equivalence_verdicts(equivalent_analysis)
                if (is.null(verdicts)) {
                    # The model did not return usable JSON; fall back to a strict
                    # line-by-line scan that matches each id literally and only
                    # accepts a verdict found on the *same* line. This avoids the
                    # cross-mutant "bleed" a greedy regex over the whole response
                    # would cause.
                    verdicts <- fallback_line_verdicts(
                        equivalent_analysis, names(file_mutants)
                    )
                }

                for (mid in names(file_mutants)) {
                    raw <- if (mid %in% names(verdicts)) verdicts[[mid]] else NA_character_
                    cls <- classify_equivalence_verdict(raw)

                    survived_mutants[[mid]]$equivalent <- cls$equivalent
                    survived_mutants[[mid]]$equivalence_status <- cls$status

                    if (isTRUE(cls$equivalent)) {
                        equiv_count <- equiv_count + 1
                    } else if (isFALSE(cls$equivalent)) {
                        not_equiv_count <- not_equiv_count + 1
                    } else {
                        unknown_count <- unknown_count + 1
                    }
                    cat(sprintf("Mutant %s: %s\n", mid, cls$status))
                }
            }
        }
    }

    cat("\nEquivalence Analysis Summary:\n")
    cat(sprintf("  Equivalent:     %d\n", equiv_count))
    cat(sprintf("  Not Equivalent: %d\n", not_equiv_count))
    cat(sprintf("  Uncertain:      %d\n", unknown_count))
    cat("\n")

    return(survived_mutants)
}

#' Create a prompt for equivalent mutant detection
#'
#' Generates a well-formatted prompt for the OpenAI API to analyze
#' if mutants are equivalent to the original code.
#'
#' @param original_code String containing the original source code
#' @param mutant_details List of mutant details including IDs and mutation info
#'
#' @return A formatted prompt string for the OpenAI API
create_equivalent_mutant_prompt <- function(original_code, mutant_details) {
    ids <- vapply(mutant_details, function(m) as.character(m$id), character(1))

    mutant_info <- paste(vapply(mutant_details, function(m) {
        block <- paste0("- id: \"", m$id, "\"\n  change: ", m$mutation_info)
        if (!is.null(m$mutated_code) && nzchar(m$mutated_code)) {
            block <- paste0(block, "\n  mutated code:\n```r\n", m$mutated_code, "\n```")
        }
        block
    }, character(1)), collapse = "\n\n")

    prompt <- paste0(
        "You are given an original R function and a set of mutants of it. Each ",
        "mutant applies one small change (summarised under `change:`) to the ",
        "original code; the full mutated source is shown under `mutated code:` ",
        "when available -- reason about that code, using `change:` only to locate ",
        "the edit.\n\n",
        "A mutant is EQUIVALENT only if it produces the same observable behaviour ",
        "as the original for every possible input -- identical return value, and ",
        "the same errors, warnings and side effects. It is NOT_EQUIVALENT if there ",
        "exists any input for which behaviour differs. If you cannot establish ",
        "either with confidence, answer DONT_KNOW.\n\n",
        "Reason about R semantics specifically: vectorisation and recycling, ",
        "NA/NULL handling, integer vs double, coercion rules, lazy evaluation, and ",
        "the difference between `&`/`&&` and `|`/`||`.\n\n",
        "Respond with JSON only -- no prose, no markdown code fences -- as an ",
        "object of exactly this shape:\n",
        "{\"results\": [{\"id\": \"<mutant id>\", ",
        "\"verdict\": \"EQUIVALENT\" | \"NOT_EQUIVALENT\" | \"DONT_KNOW\"}]}\n",
        "Include exactly one entry for each of these ids: ",
        paste(sprintf("\"%s\"", ids), collapse = ", "), ".\n\n",
        "Original code:\n```r\n", original_code, "\n```\n\n",
        "Mutants:\n", mutant_info, "\n"
    )

    return(prompt)
}

#' Call OpenAI API
#'
#' Makes a POST request to the OpenAI Chat Completions API.
#'
#' @param prompt The prompt to send to the API
#' @param config API configuration with key and model information
#'
#' @return API response as text, or NULL if request failed
call_openai_api <- function(prompt, config) {
    tryCatch(
        {
            # Create the request body without temperature parameter
            request_body <- list(
                model = config$model,
                messages = list(
                    list(
                        role = "system",
                        content = paste0(
                            "You are a precise program-analysis assistant for the R ",
                            "language. You decide whether a mutant of an R function is ",
                            "semantically equivalent to the original across all possible ",
                            "inputs. You are conservative: when equivalence is not ",
                            "certain, you answer DONT_KNOW rather than guess. You reply ",
                            "with valid JSON only, with no prose or markdown."
                        )
                    ),
                    list(
                        role = "user",
                        content = prompt
                    )
                )
            )

            # Convert to JSON with proper settings
            json_body <- jsonlite::toJSON(request_body, auto_unbox = TRUE)

            # Make the API request
            response <- httr::POST(
                url = "https://api.openai.com/v1/chat/completions",
                httr::add_headers(
                    "Content-Type" = "application/json",
                    "Authorization" = paste("Bearer", config$api_key)
                ),
                body = json_body,
                encode = "json"
            )

            # Check for HTTP errors
            httr::stop_for_status(response)

            if (httr::status_code(response) == 200) {
                return(httr::content(response, as = "parsed", type = "application/json"))
            } else {
                warning(
                    "OpenAI API error: ",
                    httr::content(response, "text", encoding = "UTF-8")
                )
                return(NULL)
            }
        },
        error = function(e) {
            warning("Error calling OpenAI API: ", e$message)
            return(NULL)
        }
    )
}

#' Get OpenAI API configuration
#'
#' Retrieves API key and model configuration from environment variables
#' or a configuration file.
#'
#' @return List containing api_key and model values
#'
#' @examples
#' config <- get_openai_config()
#' names(config)
#'
#' @export
get_openai_config <- function() {
    api_key <- Sys.getenv("OPENAI_API_KEY", "")
    model <- Sys.getenv("OPENAI_MODEL", "gpt-4")

    if (api_key == "") {
        # candidates: .openai_config.R and .openai_config.R.template
        candidates <- c(".openai_config.R", ".openai_config.R.template")
        wd <- normalizePath(getwd())
        config_path <- NULL

        # walk up until we hit root
        repeat {
            for (f in candidates) {
                p <- file.path(wd, f)
                if (file.exists(p)) {
                    config_path <- p
                    break
                }
            }
            if (!is.null(config_path)) break
            parent <- dirname(wd)
            if (parent == wd) break
            wd <- parent
        }

        if (!is.null(config_path)) {
            config_env <- new.env()
            try(source(config_path, local = config_env), silent = TRUE)

            # pick up either lowercase or uppercase var names
            if (exists("api_key", envir = config_env)) api_key <- get("api_key", envir = config_env)
            if (exists("OPENAI_API_KEY", envir = config_env)) api_key <- get("OPENAI_API_KEY", envir = config_env)
            if (exists("model", envir = config_env)) model <- get("model", envir = config_env)
            if (exists("OPENAI_MODEL", envir = config_env)) model <- get("OPENAI_MODEL", envir = config_env)
        }
    }

    list(api_key = api_key, model = model)
}
# nocov end

# Map a raw verdict token (from the model) to an equivalence flag and a stable
# display status. Matching is on letters only, so "NOT_EQUIVALENT",
# "NOT EQUIVALENT" and "not-equivalent" are treated identically, and anything
# unrecognised (including NA) is conservatively reported as uncertain.
classify_equivalence_verdict <- function(verdict) {
    token <- toupper(gsub("[^A-Za-z]", "", as.character(verdict)[1]))
    if (identical(token, "EQUIVALENT")) {
        list(equivalent = TRUE, status = "EQUIVALENT")
    } else if (identical(token, "NOTEQUIVALENT")) {
        list(equivalent = FALSE, status = "NOT EQUIVALENT")
    } else {
        list(equivalent = NA, status = "DONT KNOW")
    }
}

# Locate the outermost JSON array or object embedded in text, returning it as a
# string (or NULL if none is found). Whichever of '[' or '{' appears first wins.
extract_json_block <- function(text) {
    text <- as.character(text)[1]
    if (is.na(text)) {
        return(NULL)
    }
    arr <- regexpr("[", text, fixed = TRUE)
    obj <- regexpr("{", text, fixed = TRUE)

    if (arr > 0 && (obj < 0 || arr < obj)) {
        start <- arr
        close <- "]"
    } else if (obj > 0) {
        start <- obj
        close <- "}"
    } else {
        return(NULL)
    }

    hits <- gregexpr(close, text, fixed = TRUE)[[1]]
    end <- if (length(hits) == 1 && hits[1] == -1) -1L else max(hits)
    if (end > start) substr(text, start, end) else NULL
}

# Parse the model's response into a named character vector mapping mutant id ->
# raw verdict string. Returns NULL when no usable JSON can be recovered, so the
# caller can fall back to a line-based scan.
parse_equivalence_verdicts <- function(content) {
    if (is.null(content)) {
        return(NULL)
    }
    content <- as.character(content)[1]
    if (is.na(content) || !nzchar(content)) {
        return(NULL)
    }

    cleaned <- gsub("```[A-Za-z]*", "", content)
    cleaned <- gsub("```", "", cleaned)

    block <- extract_json_block(cleaned)
    if (is.null(block)) {
        return(NULL)
    }

    parsed <- tryCatch(
        jsonlite::fromJSON(block, simplifyDataFrame = TRUE),
        error = function(e) NULL
    )
    if (is.null(parsed)) {
        return(NULL)
    }

    records <- if (is.data.frame(parsed)) {
        parsed
    } else if (is.list(parsed) && is.data.frame(parsed$results)) {
        parsed$results
    } else if (is.list(parsed) && is.data.frame(parsed$mutants)) {
        parsed$mutants
    } else {
        NULL
    }

    if (is.null(records) || !all(c("id", "verdict") %in% names(records))) {
        return(NULL)
    }

    verdicts <- as.character(records$verdict)
    names(verdicts) <- as.character(records$id)
    verdicts
}

# Strict fallback when the response is not valid JSON: for each id, scan lines,
# match the id literally (fixed = TRUE), and accept only a verdict found on the
# same line. Returns a named character vector (NA where no verdict was found).
fallback_line_verdicts <- function(content, ids) {
    out <- rep(NA_character_, length(ids))
    names(out) <- ids
    if (is.null(content)) {
        return(out)
    }
    content <- as.character(content)[1]
    if (is.na(content) || !nzchar(content)) {
        return(out)
    }

    lines <- strsplit(content, "\n", fixed = TRUE)[[1]]
    for (id in ids) {
        for (ln in lines) {
            if (!grepl(id, ln, fixed = TRUE)) {
                next
            }
            up <- toupper(ln)
            # Check NOT EQUIVALENT before EQUIVALENT (substring of the former).
            verdict <- if (grepl("NOT", up, fixed = TRUE) && grepl("EQUIVALENT", up, fixed = TRUE)) {
                "NOT_EQUIVALENT"
            } else if (grepl("EQUIVALENT", up, fixed = TRUE)) {
                "EQUIVALENT"
            } else if (grepl("KNOW", up, fixed = TRUE)) {
                "DONT_KNOW"
            } else {
                NA_character_
            }
            if (!is.na(verdict)) {
                out[[id]] <- verdict
                break
            }
        }
    }
    out
}
