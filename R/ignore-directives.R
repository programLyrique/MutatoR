# Parse code-exclusion directives from a file's lines and return the regions to
# exclude from mutation. Both mutator's own directives and covr's `# nocov`
# coverage-exclusion annotations are honoured, so code already marked as
# untested-by-design (defensive branches, unreachable stubs) is not mutated.
# Returns a list with:
#   $whole_file : TRUE if a `# mutator:ignore-file` directive is present anywhere
#   $ranges     : list of integer c(start, end) line ranges to exclude, from
#                 region markers (`# mutator:ignore-start`/`-end`, `# nocov
#                 start`/`end`) and single-line `# nocov` comments.
# An unmatched region start runs to the end of the file. Multiple non-nested
# regions are supported (a single open marker is tracked linearly). Region
# starts/ends from either convention are interchangeable in practice but are not
# expected to be mixed. The directive lines themselves are comments, so they are
# never mutated regardless.
#
# covr conventions (mirrored here): `# nocov start` / `# nocov end` delimit a
# block; a bare `# nocov` excludes its own line and may be a trailing comment
# (e.g. `stop("unreachable") # nocov`). mutator's own markers must be full-line
# comments. Note that, as for mutator's region directives, an excluded single
# line still drops a whole function's operator mutants when their location could
# only be resolved to the enclosing function (see `is_excluded_range`).
ignore_directive_ranges <- function(lines) {
  result <- list(whole_file = FALSE, ranges = list())
  if (length(lines) == 0) {
    return(result)
  }

  file_re  <- "^\\s*#\\s*mutator:ignore-file\\b"
  # Region start/end: mutator's full-line markers, or covr's `# nocov start` /
  # `# nocov end` (which may trail code, hence not anchored to line start).
  start_re <- "^\\s*#\\s*mutator:ignore-start\\b|#\\s*nocov\\s*start"
  end_re   <- "^\\s*#\\s*mutator:ignore-end\\b|#\\s*nocov\\s*end"
  # A bare covr `# nocov` (not a start/end marker) excludes just its own line.
  nocov_line_re <- "#\\s*nocov"

  if (any(grepl(file_re, lines, perl = TRUE))) {
    result$whole_file <- TRUE
    return(result)
  }

  open <- NA_integer_
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    if (grepl(start_re, line, perl = TRUE)) {
      if (is.na(open)) open <- i
    } else if (grepl(end_re, line, perl = TRUE)) {
      if (!is.na(open)) {
        result$ranges[[length(result$ranges) + 1L]] <- c(open, i)
        open <- NA_integer_
      }
    } else if (grepl(nocov_line_re, line, perl = TRUE)) {
      # Single-line `# nocov`: exclude this line only. Redundant (and so
      # skipped) when already inside an open region.
      if (is.na(open)) {
        result$ranges[[length(result$ranges) + 1L]] <- c(i, i)
      }
    }
  }
  # Unmatched region start: exclude through the end of the file.
  if (!is.na(open)) {
    result$ranges[[length(result$ranges) + 1L]] <- c(open, length(lines))
  }

  result
}

# TRUE if the inclusive line span [start_line, end_line] overlaps any excluded
# range. Used to drop mutants whose reported source span falls inside a
# `# mutator:ignore-start`/`-end` region. Note that operator mutants report
# their enclosing top-level expression's bounds (see src/ASTHandler.cpp), so in
# practice this matches at function granularity for them.
is_excluded_range <- function(start_line, end_line, ranges) {
  if (length(ranges) == 0) {
    return(FALSE)
  }
  if (length(start_line) == 0 || length(end_line) == 0 ||
    is.na(start_line[1]) || is.na(end_line[1])) {
    return(FALSE)
  }
  s <- as.integer(start_line[1])
  e <- as.integer(end_line[1])
  for (r in ranges) {
    if (s <= r[2] && r[1] <= e) {
      return(TRUE)
    }
  }
  FALSE
}

