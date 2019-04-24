#' Browse dbSNP from SNP identifiers.
#'
#' This function launches the web browser and opens a tab for each SNP
#' identifier.
#'
#' @param variant_id A variant identifier, a character vector.
#'
#' @export
open_in_dbsnp <- function(variant_id) {
  if (!(rlang::is_character(variant_id) ))
    stop("variant_id must be a character vector.")

  # if (any(!is_pubmed_id(pubmed_id2)))
  #   stop("These are not valid PubMed IDs: ",
  #        concatenate::cc_and(pubmed_id[!is_pubmed_id(pubmed_id2)], oxford = TRUE),
  #        ".")

  urls <-
    glue::glue("https://www.ncbi.nlm.nih.gov/snp/{variant_id}")

  purrr::walk(urls, utils::browseURL)

  return(invisible(TRUE))
}

#' Browse GTEx from SNP identifiers.
#'
#' This function launches the web browser and opens a tab for each SNP
#' identifier on GTEx portal.
#'
#' @param variant_id A variant identifier, a character vector.
#'
#' @export
open_in_gtex <- function(variant_id) {
  if (!(rlang::is_character(variant_id) ))
    stop("variant_id must be a character vector.")

  # if (any(!is_pubmed_id(pubmed_id2)))
  #   stop("These are not valid PubMed IDs: ",
  #        concatenate::cc_and(pubmed_id[!is_pubmed_id(pubmed_id2)], oxford = TRUE),
  #        ".")

  urls <-
    glue::glue("https://gtexportal.org/home/snp/{variant_id}")

  purrr::walk(urls, utils::browseURL)

  return(invisible(TRUE))
}
