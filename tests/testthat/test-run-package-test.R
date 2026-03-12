test_that("mutate_package returns results for a valid package", {
  # Skip test if dependencies are not available
  skip_if_not_installed("devtools")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  # Mock a small package directory structure
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create a simple package structure
  pkg_dir <- file.path(temp_dir, "testpkg")
  dir.create(pkg_dir)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)

  # Add basic package files
  writeLines("Package: testpkg
Version: 0.1.0
Title: Test Package
Description: A test package.
Author: Test Author
License: MIT
RoxygenNote: 7.1.1", file.path(pkg_dir, "DESCRIPTION"))

  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))

  # Create a simple R function
  writeLines("#' Add two numbers
#' @param a First number
#' @param b Second number
#' @return Sum of a and b
#' @export
add <- function(a, b) {
  return(a + b)
}", file.path(pkg_dir, "R", "add.R"))

  # Create a test for that function
  writeLines("library(testthat)
library(testpkg)

test_check(\"testpkg\")", file.path(pkg_dir, "tests", "testthat.R"))

  writeLines("test_that(\"addition works\", {
  expect_equal(add(2, 2), 4)
})", file.path(pkg_dir, "tests", "testthat", "test-add.R"))

  result <- mutate_package(pkg_dir, cores = 1)

  expect_true(is.list(result))
  expect_true("package_mutants" %in% names(result))
  expect_true("test_results" %in% names(result))
  expect_true(length(result$test_results) > 0)
})

test_that("mutate_package marks mutants as killed when mutations break tests", {
  # Skip test if dependencies are not available
  skip_if_not_installed("devtools")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  # Create a package whose baseline passes but mutations cause failures
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  pkg_dir <- file.path(temp_dir, "killpkg")
  dir.create(pkg_dir)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)

  writeLines("Package: killpkg
Version: 0.1.0
Title: Kill Package
Description: A package where mutations get killed.
Author: Test Author
License: MIT
RoxygenNote: 7.1.1", file.path(pkg_dir, "DESCRIPTION"))

  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))

  # A function whose + operator can be mutated to -
  writeLines("inc <- function(x) {
  x + 1
}", file.path(pkg_dir, "R", "inc.R"))

  writeLines("library(testthat)
library(killpkg)

test_check(\"killpkg\")", file.path(pkg_dir, "tests", "testthat.R"))

  # This test passes on the original but fails when + is mutated to -
  writeLines("test_that(\"inc adds one\", {
  expect_equal(inc(1), 2)
})", file.path(pkg_dir, "tests", "testthat", "test-inc.R"))

  result <- mutate_package(pkg_dir, cores = 1)

  expect_true(is.list(result))
  expect_true(length(result$test_results) > 0)
  # At least one mutant should be killed
  expect_true(any(vapply(result$test_results, isFALSE, logical(1))))
})
