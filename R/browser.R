#' Browse GWAS Catalog entities from the GWAS Web Graphical User Interface
#'
#' This function launches the web browser and opens a tab for each identifier on
#' the GWAS web graphical user interface: \url{https://www.ebi.ac.uk/gwas}.
#'
#' @param identifier A vector of identifiers. The identifiers can be: study
#'   accession identifiers, variant identifiers, EFO trait identifiers, gene
#'   symbol names, cytogenetic regions, or PubMed identifiers.
#' @param gwas_catalog_entity Either \code{'study'} (default), \code{'variant'},
#'   \code{'trait'}, \code{'gene'}, \code{'region'} or \code{'publication'}, a
#'   scalar character. This argument indicates the type of the identifiers
#'   passed in \code{identifier}.
#'
#' @return Returns \code{TRUE} if successful, or \code{FALSE} otherwise. But
#'   note that this function is run for its side effect.
#' @examples
#' # Open studies in GWAS Web Graphical User Interface
#' open_in_gwas_catalog(c('GCST000016', 'GCST001115'))
#'
#' # Open variants
#' open_in_gwas_catalog(c('rs146992477', 'rs56261590'),
#'   gwas_catalog_entity = 'variant')
#'
#' # Open EFO traits
#' open_in_gwas_catalog(c('EFO_0004884', 'EFO_0004343'),
#'   gwas_catalog_entity = 'trait')
#'
#' # Open genes
#' open_in_gwas_catalog(c('DPP6', 'MCCC2'),
#'   gwas_catalog_entity = 'gene')
#'
#' # Open cytogenetic regions
#' open_in_gwas_catalog(c('2q37.1', '1p36.11'),
#'   gwas_catalog_entity = 'region')
#'
#' # Open publications
#' open_in_gwas_catalog(c('25533513', '24376627'),
#'   gwas_catalog_entity = 'publication')
#'
#' @export
open_in_gwas_catalog <- function(identifier,
                                 gwas_catalog_entity = c(
                                   'study',
                                   'variant',
                                   'trait',
                                   'gene',
                                   'region',
                                   'publication')
                                 ) {

  if (!(rlang::is_character(identifier)))
    stop("`identifier` must be a character vector.")

  if (any(rlang::are_na(identifier)))
    stop("The following positions of `identifier` are NAs: ",
         cc_and(which(rlang::are_na(identifier)), oxford = TRUE),
         ".")

  gwas_catalog_entity <- rlang::arg_match(gwas_catalog_entity)

  if (interactive()) {
    msg <- 'You are about to open {length(identifier)} pages in your browser.'
    question <- glue::glue(msg)
    if (length(identifier) > 3L)
      if (!sure(before_question = question)) return(invisible(FALSE))

    url_basename <- "https://www.ebi.ac.uk/gwas"
    entity2url <- c(
      study = "{url_basename}/studies/{identifier}",
      variant = "{url_basename}/variants/{identifier}",
      trait = "{url_basename}/efotraits/{identifier}",
      gene = "{url_basename}/genes/{identifier}",
      region = "{url_basename}/regions/{identifier}",
      publication = "{url_basename}/publications/{identifier}"
    )

    urls <- glue::glue(entity2url[gwas_catalog_entity])
    purrr::walk(urls, browse_url)

    return(invisible(TRUE))
  } else {
    return(invisible(TRUE))
  }
}
