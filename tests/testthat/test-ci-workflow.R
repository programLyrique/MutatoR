# The reusable workflow falls back to github::PRL-PRG/mutator@<mutator-ref>
# when mutator cannot be installed from CRAN or r-universe. That default has to
# name a released tag, otherwise callers pinned to a release silently fall back
# to some other version of the package.

# Reads the `default:` of a workflow_call input, without taking a YAML
# dependency: inputs are indented by six spaces and their keys by eight.
workflow_input_default <- function(lines, input) {
  start <- grep(sprintf("^      %s:\\s*$", input), lines)
  if (length(start) != 1) {
    stop(sprintf("expected exactly one '%s' input, found %d", input, length(start)))
  }

  rest <- lines[seq.int(start + 1L, length(lines))]
  next_input <- grep("^      [A-Za-z0-9-]+:", rest)
  if (length(next_input)) rest <- rest[seq_len(next_input[1] - 1L)]

  default <- grep("^        default:", rest, value = TRUE)
  if (length(default) != 1) {
    stop(sprintf("expected exactly one default for '%s', found %d", input, length(default)))
  }

  gsub('^"|"$', "", trimws(sub("^        default:", "", default)))
}

test_that("the workflow's mutator-ref default points at the current release tag", {
  workflow <- test_path("..", "..", ".github", "workflows", "mutation-testing.yaml")
  description <- test_path("..", "..", "DESCRIPTION")
  skip_if_not(
    file.exists(workflow) && file.exists(description),
    "only available in the source tree"
  )

  version <- unname(read.dcf(description, fields = "Version")[1, 1])
  # A development version such as 0.2.2.9000 still falls back to the 0.2.2 tag,
  # since no tag exists for the unreleased state.
  release <- paste(utils::head(strsplit(version, "[.-]")[[1]], 3L), collapse = ".")

  expect_equal(
    workflow_input_default(readLines(workflow), "mutator-ref"),
    paste0("v", release)
  )
})
