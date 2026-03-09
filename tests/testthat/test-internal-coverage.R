test_that("delete_line_mutants creates indexed mutant files", {
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
    mutants <- MutatoR:::delete_line_mutants(
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
    expect_true(all(vapply(mutants, function(m) grepl("deleted line", m$info), logical(1))))
})

test_that("delete_line_mutants returns empty list when no valid lines", {
    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutations_")
    dir.create(out_dir)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines(c("", "# only comment"), src)

    mutants <- NULL
    expect_warning(
        mutants <- MutatoR:::delete_line_mutants(src, out_dir = out_dir, max_del = 3),
        "No valid lines to delete"
    )

    expect_equal(mutants, list())
})

test_that("create_equivalent_mutant_prompt includes required sections", {
    prompt <- MutatoR:::create_equivalent_mutant_prompt(
        original_code = "x <- 1",
        mutant_details = list(
            list(id = "file_001", mutation_info = "x <- 0"),
            list(id = "file_002", mutation_info = "x <- 2")
        )
    )

    expect_match(prompt, "Original code", fixed = TRUE)
    expect_match(prompt, "Survived mutants", fixed = TRUE)
    expect_match(prompt, "Mutant ID: file_001", fixed = TRUE)
    expect_match(prompt, "Mutant ID: file_002", fixed = TRUE)
    expect_match(prompt, "DONT KNOW", fixed = TRUE)
})

test_that("get_openai_config prefers environment variables", {
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
    cfg <- MutatoR:::get_openai_config()

    expect_equal(cfg$api_key, "env-key")
    expect_equal(cfg$model, "env-model")
})

test_that("get_openai_config searches parent folders for config file", {
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
    cfg <- MutatoR:::get_openai_config()

    expect_equal(cfg$api_key, "file-key")
    expect_equal(cfg$model, "file-model")
})

test_that("C_mutate_file validates input types and srcref", {
    expect_error(
        .Call("C_mutate_file", 1L, PACKAGE = "MutatoR"),
        "EXPRSXP"
    )

    exprs <- expression(a + b)
    attr(exprs, "srcref") <- list(1:3)

    expect_error(
        .Call("C_mutate_file", exprs, PACKAGE = "MutatoR"),
        "length 4"
    )
})

test_that("mutate_package supports a user-provided mutation_dir", {
    skip_if_not_installed("devtools")
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
