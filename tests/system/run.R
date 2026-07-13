#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(getwd(), mustWork = TRUE)
profile <- sub("^--profile=", "", args[grepl("^--profile=", args)])
profile <- if (length(profile)) profile[[1]] else "smoke"
packages <- sub("^--packages=", "", args[grepl("^--packages=", args)])
no_bootstrap <- "--no-bootstrap" %in% args

if (!no_bootstrap) {
  bootstrap_args <- c(file.path("tests", "system", "bootstrap.R"), "--install-deps")
  if (length(packages)) bootstrap_args <- c(bootstrap_args, paste0("--packages=", packages[[1]]))
  status <- system2(file.path(R.home("bin"), "Rscript"), bootstrap_args)
  if (!identical(status, 0L)) quit(status = status)
}

pkgload::load_all(root, quiet = TRUE, export_all = TRUE)
Sys.setenv(NOT_CRAN = "true")
Sys.setenv(MUTATOR_SYSTEM_PROFILE = profile)
if (length(packages)) Sys.setenv(MUTATOR_SYSTEM_PACKAGES = packages[[1]])
testthat::test_dir(file.path(root, "tests", "system"), load_package = "none")
