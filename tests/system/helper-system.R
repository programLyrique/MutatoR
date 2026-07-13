SYSTEM_ROOT <- normalizePath(file.path(getwd(), "../.."), mustWork = TRUE)
source("fixtures.R")

system_profile <- function() {
  profile <- Sys.getenv("MUTATOR_SYSTEM_PROFILE", unset = "smoke")
  if (!profile %in% names(SYSTEM_PROFILES)) {
    stop(sprintf("Unknown system-test profile '%s'.", profile))
  }
  SYSTEM_PROFILES[[profile]]
}

system_selected_packages <- function() {
  requested <- Sys.getenv("MUTATOR_SYSTEM_PACKAGES", unset = "")
  packages <- system_profile()$packages
  if (nzchar(requested)) {
    packages <- intersect(packages, strsplit(requested, ",", fixed = TRUE)[[1]])
  }
  packages
}

relative_fixture_path <- function(path, fixture_dir) {
  if (is.null(path) || !length(path) || is.na(path)) return(path)
  path <- normalizePath(path, mustWork = FALSE)
  fixture_dir <- paste0(normalizePath(fixture_dir, mustWork = FALSE), .Platform$file.sep)
  if (startsWith(path, fixture_dir)) substring(path, nchar(fixture_dir) + 1L) else path
}

normalise_fixture_value <- function(value, fixture_dir) {
  fixture_dir <- normalizePath(fixture_dir, mustWork = FALSE)
  if (is.character(value)) {
    value <- gsub(fixture_dir, ".", value, fixed = TRUE)
    return(gsub("/tmp/Rtmp[^/]+/R_LIBS[^/]+", "<test-library>", value))
  }
  if (is.list(value)) {
    return(lapply(value, normalise_fixture_value, fixture_dir = fixture_dir))
  }
  value
}

normalise_mutation_result <- function(result, fixture_dir) {
  if (inherits(result, "error")) {
    return(list(
      outcome = "ERROR",
      error = sub("\\n.*", "", normalise_fixture_value(conditionMessage(result), fixture_dir))
    ))
  }
  mutants <- vapply(names(result$package_mutants), function(name) {
    mutant <- result$package_mutants[[name]]
    location <- mutant$mutation_loc
    paste(
      name,
      relative_fixture_path(mutant$src, fixture_dir),
      mutant$status,
      location$start_line,
      location$start_col,
      location$end_line,
      location$end_col,
      location$details,
      sep = " | "
    )
  }, character(1))
  summary <- paste(vapply(names(result$summary), function(name) {
    sprintf("%s=%s", name, paste(result$summary[[name]], collapse = ","))
  }, character(1)), collapse = " | ")
  list(outcome = "OK", summary = summary, mutants = paste(mutants, collapse = "\n"))
}

run_system_fixture <- function(package) {
  fixture_dir <- file.path(SYSTEM_ROOT, "packages", "system", package)
  profile <- system_profile()
  coverage_guided <- dir.exists(file.path(fixture_dir, "tests", "testthat"))
  set.seed(SYSTEM_SEED)
  result <- suppressMessages(tryCatch(
    mutate_package(
      fixture_dir,
      cores = 4,
      max_mutants = profile$max_mutants,
      timeout_seconds = SYSTEM_TIMEOUT_SECONDS,
      coverage_guided = coverage_guided,
      max_show = 0
    ),
    error = function(e) e
  )
  )
  normalise_mutation_result(result, fixture_dir)
}
