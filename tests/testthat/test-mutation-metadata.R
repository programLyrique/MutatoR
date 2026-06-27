# Tests that pin the exact outputs of the mutation-metadata / srcref helpers.
# These functions are exercised by other tests but their *outputs* were not
# asserted, so mutations like `<expr> -> 42` and `return/<- -> <deleted>` survived
# (see the self-mutation-testing run). Asserting exact values kills those.

test_that("format_mutation_info renders the expected strings", {
  fmi <- mutator:::format_mutation_info

  # Operator mutation: file (from raw_info), range, and 'old -> new' details.
  expect_identical(
    fmi("ignored.R", list(file_path = "M.R", start_line = 3, start_col = 5,
                          end_line = 3, end_col = 10,
                          original_symbol = "+", new_symbol = "-")),
    "File: M.R\nRange: 3:5-3:10\nDetails: '+' -> '-'"
  )
  # new_symbol = NA renders as "<deleted>"; a real new_symbol must NOT (kills
  # `is.na(new_symbol) -> 42`, which would always print "<deleted>").
  expect_identical(
    fmi("ignored.R", list(file_path = "M.R", original_symbol = "x", new_symbol = NA)),
    "File: M.R\nDetails: 'x' -> '<deleted>'"
  )
  # original_symbol = NA renders as "<unknown>" (kills the "<unknown>" deletion
  # and `is.na(original_symbol) -> !is.na(original_symbol)`).
  expect_identical(
    fmi("ignored.R", list(file_path = "M.R", original_symbol = NA, new_symbol = "z")),
    "File: M.R\nDetails: '<unknown>' -> 'z'"
  )
  # line_deletion branch (kills the "line_deletion" literal comparison).
  expect_identical(
    fmi("ignored.R", list(file_path = "M.R", mutation_type = "line_deletion",
                          deleted_line = 7)),
    "File: M.R\nDetails: deleted line 7"
  )
  # The "File:" prefix and the use of raw_info$file_path (kills the parts<-c(...)
  # and `raw_info$file_path -> 42` mutants).
  expect_true(startsWith(fmi("ignored.R", list(file_path = "ZZZ.R")), "File: ZZZ.R"))
  # When raw_info has no file_path, the source file is used (kills the condition
  # `is.list(raw_info) && !is.null(raw_info$file_path) && ... -> 42`).
  expect_match(
    fmi("foo.R", list(start_line = 1, start_col = 1, end_line = 1, end_col = 2)),
    "foo\\.R"
  )
})

test_that("mutation_location returns the expected coordinates", {
  ml <- mutator:::mutation_location

  res <- ml("ignored.R", list(file_path = "X.R", start_line = 3, end_line = 7))
  expect_identical(res$file_path, "X.R")
  expect_identical(res$start_line, 3L)   # kills `as.integer(start_line[1]) -> 42`
  expect_identical(res$end_line, 7L)     # kills `!is.null(end_line) -> is.null(...)`

  # Missing coordinates default to NA_integer_ (kills `NA_integer_ -> NULL`).
  res2 <- ml("ignored.R", NULL)
  expect_identical(res2$start_line, NA_integer_)
  expect_identical(res2$end_line, NA_integer_)
})

test_that("is_excluded_range overlaps correctly at the boundaries", {
  ier <- mutator:::is_excluded_range

  expect_true(ier(15, 18, list(c(10L, 20L))))    # contained -> overlap
  expect_true(ier(5, 12, list(c(10L, 20L))))     # straddles the start
  expect_false(ier(30, 35, list(c(10L, 20L))))   # entirely after  (kills `r[2] -> 42`)
  expect_false(ier(1, 5, list(c(10L, 20L))))     # entirely before
  # An overlapping query must stay TRUE (kills `r[1] -> 42`, which would make
  # `42 <= e` false and flip it to FALSE).
  expect_true(ier(12, 14, list(c(10L, 20L))))
  # NA / empty coordinates are guarded with `||`; flipping to `&&` would fall
  # through to `if (NA <= ...)` and error, so a clean FALSE kills `|| -> &&`.
  expect_false(ier(NA_integer_, NA_integer_, list(c(1L, 5L))))
  expect_false(ier(integer(0), integer(0), list(c(1L, 5L))))
  expect_false(ier(3, 4, list()))                # no ranges
})

test_that("filter_excluded_files honours basename glob patterns", {
  fef <- mutator:::filter_excluded_files

  files <- c("pkg/R/foo.R", "pkg/R/bar.R", "pkg/R/compat-x.R")
  # Exact basename (kills `basename(r_files) -> 42` and `glob2rx(...) -> 42`,
  # both of which would exclude nothing and return all three files).
  expect_identical(fef(files, "bar.R"), c("pkg/R/foo.R", "pkg/R/compat-x.R"))
  # Glob pattern across basenames.
  expect_identical(fef(files, "compat-*.R"), c("pkg/R/foo.R", "pkg/R/bar.R"))
  # No exclusions -> unchanged.
  expect_identical(fef(files, NULL), files)
})
