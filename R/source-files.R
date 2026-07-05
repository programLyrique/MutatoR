# Drop R source files whose base name matches any of `exclude_files`. Patterns
# are shell-style globs (e.g. "import-standalone-*"); an exact name also works.
# `exclude_files` of NULL or empty is a no-op. Matching is on the base name
# because R source discovery is non-recursive (flat `R/`).
filter_excluded_files <- function(r_files, exclude_files) {
  if (is.null(exclude_files) || length(exclude_files) == 0) {
    return(r_files)
  }
  if (!is.character(exclude_files)) {
    stop("`exclude_files` must be NULL or a character vector of file patterns.",
      call. = FALSE
    )
  }
  patterns <- utils::glob2rx(exclude_files)
  bases <- basename(r_files)
  excluded <- Reduce(`|`, lapply(patterns, function(p) grepl(p, bases)),
    accumulate = FALSE
  )
  r_files[!excluded]
}

# Drop R source files listed in the package's covr `.covrignore`, so files
# already excluded from coverage are not mutation-tested either. Mirrors covr's
# own parsing: each non-empty line is a glob expanded (relative to the package
# root) with Sys.glob(), and a matched directory expands to the files under it.
# No `.covrignore`, or one matching nothing, is a no-op.
covrignore_excluded_files <- function(r_files, pkg_dir) {
  if (length(r_files) == 0) {
    return(r_files)
  }
  ignore_file <- file.path(pkg_dir, ".covrignore")
  if (!file.exists(ignore_file)) {
    return(r_files)
  }
  lines <- tryCatch(readLines(ignore_file, warn = FALSE), error = function(e) character(0))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) {
    return(r_files)
  }
  paths <- Sys.glob(file.path(pkg_dir, lines), dirmark = TRUE)
  excluded <- unlist(
    lapply(paths, function(x) {
      if (dir.exists(x)) {
        list.files(x, recursive = TRUE, all.files = TRUE, full.names = TRUE)
      } else {
        x
      }
    }),
    use.names = FALSE
  )
  if (length(excluded) == 0) {
    return(r_files)
  }
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  r_files[!(norm(r_files) %in% norm(excluded))]
}

