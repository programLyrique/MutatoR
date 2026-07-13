#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(getwd(), mustWork = TRUE)
source(file.path(root, "tests", "system", "fixtures.R"))

requested <- sub("^--packages=", "", args[grepl("^--packages=", args)])
requested <- if (length(requested)) strsplit(requested[[1]], ",", fixed = TRUE)[[1]] else names(SYSTEM_FIXTURES)
install_deps <- "--install-deps" %in% args
packages_dir <- file.path(root, "packages", "system")
dir.create(packages_dir, recursive = TRUE, showWarnings = FALSE)

download_fixture <- function(package, version) {
  destination <- file.path(packages_dir, package)
  if (dir.exists(destination)) {
    description <- tryCatch(read.dcf(file.path(destination, "DESCRIPTION")), error = function(e) NULL)
    if (!is.null(description) && identical(unname(description[1, "Version"]), version)) {
      message(sprintf("%s %s already present.", package, version))
      return(destination)
    }
    stop(sprintf("%s exists but is not pinned version %s; remove it and retry.", package, version))
  }

  archive_name <- sprintf("%s_%s.tar.gz", package, version)
  urls <- c(
    sprintf("https://cran.r-project.org/src/contrib/%s", archive_name),
    sprintf("https://cran.r-project.org/src/contrib/Archive/%s/%s", package, archive_name)
  )
  tarball <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tarball), add = TRUE)
  downloaded <- FALSE
  for (url in urls) {
    downloaded <- tryCatch({
      utils::download.file(url, tarball, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) FALSE)
    if (downloaded) break
  }
  if (!downloaded) {
    stop(sprintf("Could not download pinned CRAN fixture %s %s.", package, version))
  }
  utils::untar(tarball, exdir = packages_dir)
  if (!dir.exists(destination)) {
    stop(sprintf("CRAN archive for %s did not extract to the expected directory.", package))
  }
  destination
}

fixture_dirs <- vapply(requested, function(package) {
  version <- SYSTEM_FIXTURES[[package]]
  if (is.null(version)) stop(sprintf("Unknown system fixture '%s'.", package))
  download_fixture(package, version)
}, character(1))

if (install_deps) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("Install 'remotes' first, or let CI install it.")
  }
  for (fixture_dir in fixture_dirs) {
    remotes::install_deps(fixture_dir, dependencies = TRUE, upgrade = "never", quiet = TRUE)
  }
}
