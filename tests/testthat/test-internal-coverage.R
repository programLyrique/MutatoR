resolve_mutator_fn <- function(name) {
    get0(name,
        mode = "function",
        inherits = TRUE,
        ifnotfound = get(name, envir = asNamespace("mutator"))
    )
}

test_that("delete_line_mutants creates indexed mutant files", {
    delete_line_mutants <- resolve_mutator_fn("delete_line_mutants")

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutations_")
    dir.create(out_dir)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines(c(
        "# comment",
        "",
        "x <- 1",
        "y <- 2"
    ), src)

    set.seed(1)
    mutants <- delete_line_mutants(
        src_file = src,
        out_dir = out_dir,
        file_base = "example.R",
        max_del = 2,
        start_idx = 10
    )

    expect_length(mutants, 2)
    expect_equal(basename(mutants[[1]]$path), "example.R_010.R")
    expect_equal(basename(mutants[[2]]$path), "example.R_011.R")
    expect_true(all(vapply(mutants, function(m) file.exists(m$path), logical(1))))
    expect_true(all(vapply(mutants, function(m) is.list(m$info), logical(1))))
    expect_true(all(vapply(mutants, function(m) identical(m$info$mutation_type, "line_deletion"), logical(1))))
    expect_true(all(vapply(mutants, function(m) !is.null(m$info$deleted_line), logical(1))))
})

test_that("delete_line_mutants returns empty list when no valid lines", {
    delete_line_mutants <- resolve_mutator_fn("delete_line_mutants")

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutations_")
    dir.create(out_dir)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines(c("", "# only comment"), src)

    mutants <- NULL
    expect_warning(
        mutants <- delete_line_mutants(src, out_dir = out_dir, max_del = 3),
        "No valid lines to delete"
    )

    expect_equal(mutants, list())
})

test_that("create_equivalent_mutant_prompt includes required sections", {
    create_equivalent_mutant_prompt <- resolve_mutator_fn("create_equivalent_mutant_prompt")

    prompt <- create_equivalent_mutant_prompt(
        original_code = "x <- 1",
        mutant_details = list(
            list(id = "file_001", mutation_info = "x <- 0"),
            list(id = "file_002", mutation_info = "x <- 2")
        )
    )

    expect_match(prompt, "Original code", fixed = TRUE)
    expect_match(prompt, "file_001", fixed = TRUE)
    expect_match(prompt, "file_002", fixed = TRUE)
    expect_match(prompt, "DONT_KNOW", fixed = TRUE)
    expect_match(prompt, "JSON", fixed = TRUE)
})

test_that("create_equivalent_mutant_prompt embeds mutated code when provided", {
    create_equivalent_mutant_prompt <- resolve_mutator_fn("create_equivalent_mutant_prompt")

    prompt <- create_equivalent_mutant_prompt(
        original_code = "f <- function(x) x + 1",
        mutant_details = list(
            list(id = "f_001", mutation_info = "'+' -> '-'", mutated_code = "f <- function(x) x - 1")
        )
    )

    # The indented marker only appears in a per-mutant block, not the intro text.
    expect_match(prompt, "\n  mutated code:\n", fixed = TRUE)
    expect_match(prompt, "f <- function(x) x - 1", fixed = TRUE)

    # Without mutated_code the per-mutant section is omitted (e.g. the exported
    # entry point called with only a change description).
    prompt2 <- create_equivalent_mutant_prompt(
        original_code = "f <- function(x) x + 1",
        mutant_details = list(list(id = "f_001", mutation_info = "'+' -> '-'"))
    )
    expect_no_match(prompt2, "\n  mutated code:\n", fixed = TRUE)
})

test_that("parse_equivalence_verdicts reads JSON object, array and fenced forms", {
    parse_equivalence_verdicts <- resolve_mutator_fn("parse_equivalence_verdicts")

    obj <- '{"results":[{"id":"a_001","verdict":"EQUIVALENT"},{"id":"b_002","verdict":"NOT_EQUIVALENT"}]}'
    v <- parse_equivalence_verdicts(obj)
    expect_equal(v[["a_001"]], "EQUIVALENT")
    expect_equal(v[["b_002"]], "NOT_EQUIVALENT")

    arr <- '[{"id":"a_001","verdict":"DONT_KNOW"}]'
    expect_equal(parse_equivalence_verdicts(arr)[["a_001"]], "DONT_KNOW")

    fenced <- "```json\n{\"results\":[{\"id\":\"a_001\",\"verdict\":\"EQUIVALENT\"}]}\n```"
    expect_equal(parse_equivalence_verdicts(fenced)[["a_001"]], "EQUIVALENT")

    # Surrounding prose is tolerated; non-JSON returns NULL so the caller falls back.
    pre <- "Here is my answer:\n{\"results\":[{\"id\":\"a_001\",\"verdict\":\"EQUIVALENT\"}]}"
    expect_equal(parse_equivalence_verdicts(pre)[["a_001"]], "EQUIVALENT")
    expect_null(parse_equivalence_verdicts("no json here"))
    expect_null(parse_equivalence_verdicts(NULL))
})

test_that("classify_equivalence_verdict normalises tokens", {
    classify_equivalence_verdict <- resolve_mutator_fn("classify_equivalence_verdict")

    expect_true(classify_equivalence_verdict("EQUIVALENT")$equivalent)
    expect_false(classify_equivalence_verdict("not equivalent")$equivalent)
    expect_equal(classify_equivalence_verdict("NOT_EQUIVALENT")$status, "NOT EQUIVALENT")
    expect_true(is.na(classify_equivalence_verdict("DONT_KNOW")$equivalent))
    expect_true(is.na(classify_equivalence_verdict(NA)$equivalent))
    expect_true(is.na(classify_equivalence_verdict("gibberish")$equivalent))
})

test_that("fallback_line_verdicts does not bleed verdicts across mutants", {
    fallback_line_verdicts <- resolve_mutator_fn("fallback_line_verdicts")

    # m1 has no verdict on its own line; the EQUIVALENT belongs to m2. The old
    # greedy `mid.*EQUIVALENT` regex would have matched m1 against m2's verdict.
    content <- paste(
        "Mutant a.R_a.R_001.R: analysis pending",
        "Mutant a.R_a.R_002.R: EQUIVALENT",
        sep = "\n"
    )
    v <- fallback_line_verdicts(content, c("a.R_a.R_001.R", "a.R_a.R_002.R"))
    expect_true(is.na(v[["a.R_a.R_001.R"]]))
    expect_equal(v[["a.R_a.R_002.R"]], "EQUIVALENT")

    # "NOT EQUIVALENT" must not be read as EQUIVALENT.
    expect_equal(fallback_line_verdicts("Mutant x: NOT EQUIVALENT", "x")[["x"]], "NOT_EQUIVALENT")
})

test_that("get_openai_config prefers environment variables", {
    get_openai_config <- resolve_mutator_fn("get_openai_config")

    old_key <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
    old_model <- Sys.getenv("OPENAI_MODEL", unset = NA_character_)
    on.exit(
        {
            if (is.na(old_key)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = old_key)
            if (is.na(old_model)) Sys.unsetenv("OPENAI_MODEL") else Sys.setenv(OPENAI_MODEL = old_model)
        },
        add = TRUE
    )

    Sys.setenv(OPENAI_API_KEY = "env-key", OPENAI_MODEL = "env-model")
    cfg <- get_openai_config()

    expect_equal(cfg$api_key, "env-key")
    expect_equal(cfg$model, "env-model")
})

test_that("get_openai_config searches parent folders for config file", {
    get_openai_config <- resolve_mutator_fn("get_openai_config")

    old_key <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
    old_model <- Sys.getenv("OPENAI_MODEL", unset = NA_character_)
    old_wd <- getwd()
    on.exit(
        {
            setwd(old_wd)
            if (is.na(old_key)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = old_key)
            if (is.na(old_model)) Sys.unsetenv("OPENAI_MODEL") else Sys.setenv(OPENAI_MODEL = old_model)
        },
        add = TRUE
    )

    Sys.unsetenv("OPENAI_API_KEY")
    Sys.unsetenv("OPENAI_MODEL")

    root <- tempfile("cfg_root_")
    child <- file.path(root, "a", "b")
    dir.create(child, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)

    writeLines(
        c(
            "api_key <- 'file-key'",
            "model <- 'file-model'"
        ),
        file.path(root, ".openai_config.R")
    )

    setwd(child)
    cfg <- get_openai_config()

    expect_equal(cfg$api_key, "file-key")
    expect_equal(cfg$model, "file-model")
})

test_that("C_mutate_file validates input types and srcref", {
    expect_error(
        .Call("C_mutate_file", 1L, PACKAGE = "mutator"),
        "EXPRSXP"
    )

    exprs <- expression(a + b)
    attr(exprs, "srcref") <- list(1:3)

    expect_error(
        .Call("C_mutate_file", exprs, PACKAGE = "mutator"),
        "length 4"
    )
})

test_that("mutate_package supports a user-provided mutation_dir", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    skip_if_not_installed("pkgload")
    skip_if_not_installed("furrr")
    skip_if_not_installed("future")

    pkg_info <- create_test_package("testMutatoRCustomDir")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    custom_mutation_dir <- tempfile("mutations_keep_")
    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = custom_mutation_dir,
        isFullLog = TRUE
    )
    on.exit(unlink(custom_mutation_dir, recursive = TRUE), add = TRUE)

    expect_true(is.list(result))
    expect_true(dir.exists(custom_mutation_dir))

    mutated_files <- list.files(custom_mutation_dir, pattern = "\\.R$", full.names = TRUE)
    expect_true(length(mutated_files) > 0)
})

test_that("mutate_file falls back to line-deletion mutants when C call fails", {
    mutate_file <- resolve_mutator_fn("mutate_file")

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutate_file_out_")
    dir.create(out_dir, recursive = TRUE)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines(c("f <- function(x) x + 1", "f(1)"), src)

    mutants <- mutate_file(src, out_dir = out_dir)

    expect_true(length(mutants) >= 1)
    expect_true(any(vapply(mutants, function(m) grepl("deleted line", m$info, fixed = TRUE), logical(1))))
    expect_true(all(vapply(mutants, function(m) file.exists(m$path), logical(1))))
})

test_that("mutate_file restores keep.source option", {
    mutate_file <- resolve_mutator_fn("mutate_file")

    old_options <- options(keep.source = FALSE)
    on.exit(options(old_options), add = TRUE)

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutate_file_options_")
    dir.create(out_dir, recursive = TRUE)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines("f <- function(x) x + 1", src)
    mutate_file(src, out_dir = out_dir)

    expect_false(getOption("keep.source"))
})

test_that("mutate_package handles empty test results as killed mutants", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatoREmptyResults")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_mock_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(.x, .f, ...) {
            out <- lapply(.x, function(...) list())
            names(out) <- names(.x)
            out
        },
        furrr_options = function(...) NULL,
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        isFullLog = TRUE,
        mutation_dir = mutation_dir
    )

    expect_true(is.list(result))
    expect_true(length(result$test_results) >= 1)
    expect_true(all(vapply(result$test_results, function(x) identical(x, "KILLED"), logical(1))))
})

test_that("mutate_package restores previous future plan on errors", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatoRFuturePlanRestore")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_future_plan_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    plan_calls <- list()
    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(...) {
            stop("future_map failed")
        },
        furrr_options = function(...) {
            list(...)
        },
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) {
            args <- list(...)
            plan_calls[[length(plan_calls) + 1]] <<- args
            if (length(args) == 0) "previous-plan" else invisible(NULL)
        },
        .package = "future"
    )

    expect_error(
        mutate_package(
            pkg_dir = pkg_info$pkg_dir,
            cores = 1,
            mutation_dir = mutation_dir
        ),
        "future_map failed"
    )

    expect_true(any(vapply(
        plan_calls,
        function(args) length(args) == 1 && identical(args[[1]], "previous-plan"),
        logical(1)
    )))
})

test_that("mutate_package marks timed-out mutants as HANG", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatoRTimeoutHang")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_hang_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(.x, .f, ...) {
            out <- lapply(.x, function(...) "HANG")
            names(out) <- names(.x)
            out
        },
        furrr_options = function(...) NULL,
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = mutation_dir
    )

    expect_equal(unname(unlist(result$test_results)), "HANG")
    expect_equal(result$package_mutants[[1]]$status, "HANG")
})

test_that("mutate_package passes explicit timeout to inline worker execution", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatoRExplicitTimeout")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_timeout_override_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    observed_timeout <- NA_real_

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(.x, .f, ...) {
            dots <- list(...)
            if (!is.null(dots$.options) && !is.null(dots$.options$globals)) {
                observed_timeout <<- dots$.options$globals$effective_timeout_seconds
            }
            out <- lapply(.x, .f)
            names(out) <- names(.x)
            out
        },
        furrr_options = function(...) {
            list(...)
        },
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = mutation_dir,
        timeout_seconds = 12.5
    )

    expect_true(is.list(result))
    expect_equal(observed_timeout, 12.5)
})

test_that("mutate_package computes equivalent mutant summary when enabled", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatoREquivSummary")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_eq_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_paths <- c(
        file.path(mutation_dir, "my_abs.R_001.R"),
        file.path(mutation_dir, "my_abs.R_002.R"),
        file.path(mutation_dir, "my_abs.R_003.R")
    )
    original_impl <- c(
        "my_abs <- function(x) {",
        "  if (x < 0) {",
        "    return(-x)",
        "  }",
        "  return(x)",
        "}"
    )
    for (p in mut_paths) writeLines(original_impl, p)

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(
                list(path = mut_paths[[1]], info = "m1"),
                list(path = mut_paths[[2]], info = "m2"),
                list(path = mut_paths[[3]], info = "m3")
            )
        },
        identify_equivalent_mutants = function(src_file, file_mutants, api_config = NULL) {
            ids <- names(file_mutants)
            if (length(ids) >= 1) {
                file_mutants[[ids[[1]]]]$equivalent <- TRUE
                file_mutants[[ids[[1]]]]$equivalence_status <- "EQUIVALENT"
            }
            if (length(ids) >= 2) {
                file_mutants[[ids[[2]]]]$equivalent <- FALSE
                file_mutants[[ids[[2]]]]$equivalence_status <- "NOT EQUIVALENT"
            }
            if (length(ids) >= 3) {
                file_mutants[[ids[[3]]]]$equivalent <- NA
                file_mutants[[ids[[3]]]]$equivalence_status <- "DONT KNOW"
            }
            file_mutants
        },
        .package = "mutator"
    )

    output <- capture.output({
        result <- mutate_package(
            pkg_dir = pkg_info$pkg_dir,
            cores = 1,
            isFullLog = TRUE,
            detectEqMutants = TRUE,
            mutation_dir = mutation_dir
        )
    })

    expect_true(any(grepl("Equivalent:", output)))
    expect_true(any(grepl("Not Equivalent:", output)))
    expect_true(any(grepl("Uncertain:", output)))
    expect_true(is.list(result))
    expect_true(length(result$package_mutants) >= 1)
})

test_that("max_mutants validation rejects invalid values", {
    mutate_file <- resolve_mutator_fn("mutate_file")
    mutate_package <- resolve_mutator_fn("mutate_package")

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutations_validation_")
    dir.create(out_dir, recursive = TRUE)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)
    writeLines("x <- 1", src)

    expect_error(mutate_file(src, out_dir = out_dir, max_mutants = -1), "max_mutants")
    expect_error(mutate_file(src, out_dir = out_dir, max_mutants = c(1, 2)), "single finite")
    expect_error(mutate_file(src, out_dir = out_dir, max_mutants = 1.5), "whole number")

    expect_error(mutate_package(tempdir(), max_mutants = -1), "max_mutants")
    expect_error(mutate_package(tempdir(), max_mutants = c(1, 2)), "single finite")
    expect_error(mutate_package(tempdir(), max_mutants = 1.5), "whole number")
})

test_that("timeout parameter validation rejects invalid values", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    expect_error(mutate_package(tempdir(), timeout_seconds = 0), "timeout_seconds")
    expect_error(mutate_package(tempdir(), timeout_seconds = c(1, 2)), "single finite")
})

test_that("mutate_package caps total mutants with max_mutants", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    skip_if_not_installed("pkgload")
    skip_if_not_installed("furrr")
    skip_if_not_installed("future")

    pkg_info <- create_test_package("testMutatoRMaxMutantsCap")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_cap_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_paths <- c(
        file.path(mutation_dir, "my_abs.R_001.R"),
        file.path(mutation_dir, "my_abs.R_002.R"),
        file.path(mutation_dir, "my_abs.R_003.R")
    )
    for (p in mut_paths) writeLines("my_abs <- function(x) x", p)

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(
                list(path = mut_paths[[1]], info = "m1"),
                list(path = mut_paths[[2]], info = "m2"),
                list(path = mut_paths[[3]], info = "m3")
            )
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(.x, .f, ...) {
            out <- lapply(.x, function(...) TRUE)
            names(out) <- names(.x)
            out
        },
        furrr_options = function(...) NULL,
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = mutation_dir,
        max_mutants = 2
    )

    expect_true(is.list(result))
    expect_length(result$test_results, 2)
    expect_length(result$package_mutants, 2)
})

test_that("mutate_package supports max_mutants set to zero", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    skip_if_not_installed("pkgload")
    skip_if_not_installed("furrr")
    skip_if_not_installed("future")

    pkg_info <- create_test_package("testMutatoRZeroMutants")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_zero_cap_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "m1"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = mutation_dir,
        max_mutants = 0
    )

    expect_true(is.list(result))
    expect_equal(result$test_results, list())
    expect_equal(result$package_mutants, list())
})

test_that("detectEqMutants runs the real equivalence workflow for underscore filenames", {
    # Regression test: the source file backing a survived mutant used to be
    # recovered with strsplit(id, "_")[[1]][1], which mangles any name
    # containing '_' (e.g. my_abs.R -> "my"). identify_equivalent_mutants then
    # tried to readLines("<pkg>/R/my") and errored. Here we exercise the real
    # workflow (only the network call is stubbed) to lock in the fix.
    mutate_package <- resolve_mutator_fn("mutate_package")

    skip_if_not_installed("pkgload")
    skip_if_not_installed("furrr")
    skip_if_not_installed("future")

    pkg_info <- create_test_package("testMutatoREquivReal") # ships R/my_abs.R
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_eq_real_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    # A mutant identical to the original survives the test suite.
    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines(c(
        "my_abs <- function(x) {",
        "  if (x < 0) {",
        "    return(-x)",
        "  }",
        "  return(x)",
        "}"
    ), mut_path)

    # The mutant id mirrors how mutate_package builds it: <src>_<mutant file>.
    expected_id <- paste("my_abs.R", basename(mut_path), sep = "_")

    # Make get_openai_config() return a non-empty key so the workflow proceeds
    # past its early return, without touching the real environment/config files.
    old_key <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
    on.exit(
        if (is.na(old_key)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = old_key),
        add = TRUE
    )
    Sys.setenv(OPENAI_API_KEY = "test-key")

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "identity mutant"))
        },
        # Stub only the network call; identify_equivalent_mutants (incl. the
        # readLines(src_file) that used to crash) runs for real.
        call_openai_api = function(prompt, config) {
            list(choices = list(list(message = list(
                content = paste0("Mutant ", expected_id, ": EQUIVALENT")
            ))))
        },
        .package = "mutator"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        detectEqMutants = TRUE,
        mutation_dir = mutation_dir
    )

    # Completing at all is the core regression assertion.
    expect_true(is.list(result))
    expect_true(expected_id %in% names(result$package_mutants))

    mutant <- result$package_mutants[[expected_id]]
    expect_equal(mutant$status, "SURVIVED")
    # The equivalence verdict was attached, proving the correct source file was
    # read and the survived mutant was matched back to it.
    expect_true(isTRUE(mutant$equivalent))
    expect_equal(mutant$equivalence_status, "EQUIVALENT")
})
