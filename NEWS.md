## gwasrapidd 0.99.11
- Fixed issues #8, #14, #16 and #17.
- Added identifier mapping functions: `study_to_association()`, `study_to_variant()`, `study_to_trait()`, `association_to_study()`, `association_to_variant()`, `association_to_trait()`, `variant_to_study()`, `variant_to_association()`, `variant_to_trait()`, `trait_to_study()`, `trait_to_association()`, and `trait_to_variant()`.

## gwasrapidd 0.99.10
- Fixed issue #13.

## gwasrapidd 0.99.9
- gwasrapidd depends now on tidyr (> 0.8.99).

## gwasrapidd 0.99.8
- `genomic_contexts` of object `variants` includes now the column `is_mapped_gene`.

## gwasrapidd 0.99.7
- Addressing remaining warnings in Windows build, namely those related to broken doc links: [https://github.com/Bioconductor/Contributions/issues/1124#issuecomment-499088435](https://github.com/Bioconductor/Contributions/issues/1124#issuecomment-499088435)

## gwasrapidd 0.99.6
- Addressing issue: [https://github.com/Bioconductor/Contributions/issues/1124#issuecomment-499046949](https://github.com/Bioconductor/Contributions/issues/1124#issuecomment-499046949)

## gwasrapidd 0.99.5
- Just a version bump to troubleshoot bioc build on Windows: [https://github.com/Bioconductor/Contributions/issues/1124#issuecomment-497415961](https://github.com/Bioconductor/Contributions/issues/1124#issuecomment-497415961).

## gwasrapidd 0.99.4
- Added function `open_in_gwas_catalog()`.

## gwasrapidd 0.0.1
- Updated gwasrapidd packaging to comply with Bioconductor guidelines (`BiocCheck::BiocCheck()`).

## gwasrapidd 0.0.0.9015
- Fixed typo in cheatsheet.

## gwasrapidd 0.0.0.9014
- `get_variants()` can now use the `cytogenetic_band` criterion.

## gwasrapidd 0.0.0.9013
- Added the dataset `cytogenetic_bands`.

## gwasrapidd 0.0.0.9012
- Updated README installation command to use the new `remotes` package.
- Devel version badge is now hardcoded directly instead of using a call to `badger`.

## gwasrapidd 0.0.0.9011
- `get_variants()` now accepts a new parameter: `std_chromosomes_only`. This allows to select whether  variants mapped to scaffolds other than the standard chromosomes are returned. By default, now, `get_variants()` only returns variants mapped to chromosomes 1 thru 22, X, Y, or MT. This behaviour is now in line with results returned by the Web UI.
- Fixed bug with `multiple_snp_haplotype` and `snp_interaction`. NA values in columns `multiple_snp_haplotype` and `snp_interaction` of associations tibble in associations S4 object were being mapped to character type instead of logical. This is now fixed.
- Constructors for GWAS Catalog S4 objects ensure no NAs are in primary keys.
- Increased test coverage.

## gwasrapidd 0.0.0.9010

- Changed type of column `association_id` in the S4 class `associations` from
`integer()` to `character()`. This saves us from unnecessary coercions from 
integer to character when searching by association identifiers.
- The cheatsheet is also changed to reflect this update.

## gwasrapidd 0.0.0.9009

- Added function `exists_variant()`.

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
