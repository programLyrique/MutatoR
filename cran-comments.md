## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

* The note "Suggests or Enhances not in mainstream repositories: imputesrcref"
  is expected. 'imputesrcref' is an optional enhancement available only from
  GitHub (<https://github.com/PRL-PRG/imputesrcref>); it is used to refine
  reported mutation source locations when present. It is listed under Enhances
  (not Imports/Suggests) and is accessed strictly conditionally via
  requireNamespace(); the package is fully functional and all tests pass when it
  is not installed.

## Reverse dependencies

There are currently no downstream dependencies for this package.

## References

There are no external references describing the implementation methods for this
initial submission.
