
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gwasrapidd <img src="man/figures/logo.svg" align="right" height=140/>

[![Travis build
status](https://travis-ci.org/ramiromagno/gwasrapidd.svg?branch=master)](https://travis-ci.org/ramiromagno/gwasrapidd)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ramiromagno/gwasrapidd?branch=master&svg=true)](https://ci.appveyor.com/project/ramiromagno/gwasrapidd)
[![BioC
status](http://www.bioconductor.org/shields/build/release/bioc/gwasrapidd.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/gwasrapidd)
[![codecov](https://codecov.io/gh/ramiromagno/gwasrapidd/branch/master/graph/badge.svg)](https://codecov.io/gh/ramiromagno/gwasrapidd)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![](https://img.shields.io/badge/devel%20version-0.0.1-blue.svg)](https://github.com/ramiromagno/gwasrapidd)

The goal of gwasrapidd is to provide programmatic access to the
[NHGRI-EBI Catalog](https://www.ebi.ac.uk/gwas) of published genome-wide
association studies.

Get started by reading the
[documentation](https://rmagno.eu/gwasrapidd/articles/gwasrapidd.html).

## Installation

You can install the current, experimental version of gwasrapidd with:

``` r
# install.packages("remotes")
remotes::install_github("ramiromagno/gwasrapidd")
```

## Cheatsheet

<a href="https://github.com/ramiromagno/gwasrapidd/raw/master/inst/cheatsheet/gwasrapidd_cheatsheet.pdf"><img src="https://raw.githubusercontent.com/ramiromagno/gwasrapidd/master/inst/cheatsheet/gwasrapidd_cheatsheet_thumbs.png" width="420" height="300"/></a>

## Example

Get the study related to triple-negative breast cancer:

``` r
library(gwasrapidd)
studies <- get_studies(efo_trait = 'triple-negative breast cancer')
studies@studies[1:4]
## # A tibble: 1 x 4
##   study_id  reported_trait        initial_sample_size  replication_sample_…
##   <chr>     <chr>                 <chr>                <chr>               
## 1 GCST0023… Breast cancer (estro… 1,529 European ance… 2,148 European ance…
```

Find associated variants:

``` r
variants <- get_variants(study_id = 'GCST002305')
variants@variants[c('variant_id', 'functional_class')]
## # A tibble: 5 x 2
##   variant_id functional_class   
##   <chr>      <chr>              
## 1 rs4245739  3_prime_UTR_variant
## 2 rs2363956  missense_variant   
## 3 rs10069690 intron_variant     
## 4 rs3757318  intron_variant     
## 5 rs10771399 intergenic_variant
```

## Contributors

Please note that the gwasrapidd project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
