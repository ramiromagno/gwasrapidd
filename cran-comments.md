## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

- The NOTE results from the elapsed time of the examples ran in
`identifier_mapping` documentation, which is greater than 10s. Because these are
REST API requests I don't think I can reduce the time without skipping them.
