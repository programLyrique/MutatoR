# Mutation system tests

These are snapshot-based end-to-end checks for mutation testing against pinned
CRAN source fixtures. They are intentionally outside `tests/testthat/`, so they
run only through this runner or the dedicated CI workflow.

```sh
Rscript tests/system/run.R
Rscript tests/system/run.R --packages=lumberjack
Rscript tests/system/run.R --profile=full
```

`bootstrap.R` downloads the exact fixture versions from `fixtures.R` into the
ignored `packages/system/` directory and, by default, installs their dependencies.
Snapshots intentionally exclude timing and temporary paths.
