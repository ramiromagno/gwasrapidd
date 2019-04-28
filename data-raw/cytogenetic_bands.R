# Title: Human cytogenetic bands and their genomic coordinates.
# Genome assembly version: GRCh38.
# Source: Ensembl's REST API (https://rest.ensembl.org/info/assembly/homo_sapiens?content-type=application/json&bands=1)
# Last download date: 28 April 2019.
# How to run: just source this file.


# Output: This script generates a tibble named 'cytogenetic_bands' of 7 columns:
#   - cytogenetic_band
#   - chromosome
#   - start
#   - end
#   - assembly
#   - stain
#   - last_download_date: time stamp of the date this script was last run.
#
# The 'cytogenetic_bands' tibble is saved to two files:
#   - data-raw/cytogenetic_bands.csv
#   - data/cytogenetic_bands.rda

library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(lubridate)

ensembl_json <- httr::GET(url = 'https://rest.ensembl.org/info/assembly/homo_sapiens?content-type=application/json&bands=1')
response_code <- httr::status_code(ensembl_json)

if(!identical(response_code, 200L))
  stop('Could not get cytogenetic band information from Ensembl.')

content_type <- httr::http_type(ensembl_json)

if (!identical(content_type, "application/json"))
  stop('Response from Ensembl is not JSON.')

content <- jsonlite::fromJSON(httr::content(ensembl_json, "text"), flatten = TRUE)

is_chr <- content$top_level_region$coord_system == 'chromosome'

cytogenetic_bands <-
  dplyr::bind_rows(content$top_level_region$bands[is_chr]) %>%
  tibble::as_tibble() %>%
  dplyr::rename(chromosome = seq_region_name) %>%
  dplyr::mutate_at(vars(id), ~paste0(chromosome, id)) %>%
  dplyr::rename(cytogenetic_band = id, assembly = assembly_name) %>%
  dplyr::select(cytogenetic_band, chromosome, start, end, assembly, stain) %>%
  dplyr::arrange(match(chromosome, c(1:22, 'X', 'Y', 'MT')), start, end) %>%
  dplyr::mutate(last_download_date = lubridate::date())

# Note: although I include the mitochondrial chromosome in the sorting
# (dplyr::arrange command), it does not make till the end because it has no
# bands assigned to it. So it gets lost already at the dplyr::bind_rows()
# command level.

readr::write_csv(cytogenetic_bands, "data-raw/cytogenetic_bands.csv")
usethis::use_data(cytogenetic_bands, compress = "xz", overwrite = TRUE)
