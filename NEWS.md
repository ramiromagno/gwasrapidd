## gwasrapidd 0.0.0.9010

- Changed type of column `association_id` in the S4 class `associations` from
`integer()` to `character()`. This saves us from unnecessary coercions from 
integer to character when searching by association identifers.
- The cheatsheet is also changed to reflect this update.

## gwasrapidd 0.0.0.9009

- Added function `exists_variant()`

## gwasrapidd 0.0.0.9008

- Improved the Getting Started vignette.

## gwasrapidd 0.0.0.9007

- Added two new functions: `open_in_dbsnp()` and `open_in_gtex()`.

## gwasrapidd 0.0.0.9006

- Added a cheatsheet.

## gwasrapidd 0.0.0.9005

- Improved test coverage significantly.

## gwasrapidd 0.0.0.9004

- Reorganized mock fixtures. Now mock file paths are all under 100 chars. No
more R CMD check whining.

## gwasrapidd 0.0.0.9003

- Reference documentation is organized in the website.
