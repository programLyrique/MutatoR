#!/usr/bin/env Rscript
# measure_baselines.R -- measure each package's PLAIN (uninstrumented, no covr)
# test-suite run time, the honest denominator for "× baseline" cost ratios.
#
# For every package present in results/benchmark_results*.csv (or those passed as
# args), times one run of its framework-appropriate suite via test_command()
# (load_all + test_dir / tinytest / rtests -- exactly how the tools run it, but
# once and without coverage instrumentation). Takes the MIN of 2 runs to drop
# first-run compilation noise. Writes results/baselines.csv (package, baseline_s).

Sys.setenv(BENCH_ROOT = "/home/pierre/Documents/RLanguage/MutatoR/benchmarks")
suppressWarnings(suppressMessages(pkgload::load_all("/home/pierre/Documents/RLanguage/MutatoR", quiet = TRUE)))
source(file.path(Sys.getenv("BENCH_ROOT"), "lib", "common.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  pkgs <- args
} else {
  csvs <- Sys.glob(file.path(RESULTS_DIR, "benchmark_results*.csv"))
  pkgs <- unique(unlist(lapply(csvs, function(f)
    read.csv(f, stringsAsFactors = FALSE)$package)))
}

time_once <- function(work) {
  cmd <- test_command(work)
  t0 <- Sys.time()
  system2("bash", c("-c", shQuote(cmd)), stdout = FALSE, stderr = FALSE)
  as.numeric(difftime(Sys.time(), t0, units = "secs"))
}

rows <- lapply(pkgs, function(p) {
  pd <- ensure_package_source(p)
  if (is.null(pd)) return(NULL)
  work <- copy_pkg(pd, "base")
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
  t <- min(time_once(work), time_once(work))   # min of 2: warm run, no compile noise
  cat(sprintf("%-13s plain baseline = %.1fs\n", p, t))
  data.frame(package = p, baseline_s = round(t, 1), stringsAsFactors = FALSE)
})
out <- do.call(rbind, Filter(Negate(is.null), rows))
utils::write.csv(out, file.path(RESULTS_DIR, "baselines.csv"), row.names = FALSE)
cat(sprintf("\nWrote %s\n", file.path(RESULTS_DIR, "baselines.csv")))
