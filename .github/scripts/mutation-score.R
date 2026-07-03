pkg_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)

cores <- parallel::detectCores(logical = TRUE)
if (is.na(cores) || cores < 1L) {
  cores <- 1L
}

runner_temp <- Sys.getenv("RUNNER_TEMP", unset = tempdir())
mutation_dir <- file.path(runner_temp, "mutator-self-mutations")
dir.create(mutation_dir, recursive = TRUE, showWarnings = FALSE)

out_dir <- "mutation-results"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_badge <- function(message, color) {
  badge <- list(
    schemaVersion = 1L,
    label = "mutation score",
    message = message,
    color = color
  )
  jsonlite::write_json(
    badge,
    file.path(out_dir, "mutation-score.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}

message(sprintf("Running self-mutation with %d core(s).", cores))
message("Using isolate = TRUE so parallel tests do not share tests/ or src/ state.")

result <- tryCatch(
  mutator::mutate_package(
    pkg_dir,
    cores = cores,
    isolate = TRUE,
    mutation_dir = mutation_dir,
    max_show = 25L
  ),
  error = function(err) {
    write_badge("failing tests", "red")
    message("Self-mutation could not run; wrote failing-tests badge data.")
    stop(conditionMessage(err), call. = FALSE)
  }
)

summary <- result$summary
score <- summary$mutation_score

score_label <- sprintf("%.1f%%", score)
color <- if (score >= 90) {
  "brightgreen"
} else if (score >= 80) {
  "green"
} else if (score >= 60) {
  "yellow"
} else if (score >= 40) {
  "orange"
} else {
  "red"
}

write_badge(score_label, color)

message(sprintf("Mutation score: %s", score_label))
