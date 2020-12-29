#' Get GWAS Catalog associations by study identifiers
#'
#' Gets associations by GWAS Catalog internal study identifiers.
#'
#' @param study_id A \code{character} vector of GWAS Catalog study accession
#'   identifiers.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{associations} object.
#' @keywords internal
get_associations_by_study_id <- function(study_id = NULL,
                                         verbose = FALSE,
                                         warnings = TRUE,
                                         page_size = 20L) {

  if(rlang::is_null(study_id))
    return(associations())

  assertthat::assert_that(
    is.character(study_id),
    length(study_id) > 0,
    assertthat::noNA(study_id),
    all(is_study_id(study_id)))

  resource_urls <- sprintf("/%s/%s/%s", "studies", study_id, "associations")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty associations object.
  if(rlang::is_empty(responses_ok)) return(associations())

  obj <- plst_left_join(responses_ok)
  my_associations <- a_obj_to_associations(obj)

  return(my_associations)
}

#' Get GWAS Catalog associations by their association identifiers
#'
#' Gets associations by GWAS Catalog internal association identifiers.
#'
#' @param association_id A \code{character} vector of GWAS Catalog association
#'   identifiers.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{associations} object.
#' @keywords internal
get_associations_by_association_id <- function(association_id = NULL,
                                               verbose = FALSE,
                                               warnings = TRUE,
                                               page_size = 20L) {

  if(rlang::is_null(association_id))
    return(associations())

  assertthat::assert_that(
    is.character(association_id),
    length(association_id) > 0,
    assertthat::noNA(association_id),
    all(is_association_id(association_id)))

  resource_urls <- sprintf("/%s/%s/%s", "associations", association_id, "")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty associations object.
  if(rlang::is_empty(responses_ok)) return(associations())

  obj <- plst_left_join(responses_ok)

  my_associations <- a_obj_to_associations(obj)

  return(my_associations)
}

#' Get GWAS Catalog associations by variant identifiers
#'
#' Gets associations by variant identifiers.
#'
#' @param variant_id A \code{character} vector of GWAS Catalog variant identifiers.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{associations} object.
#' @keywords internal
get_associations_by_variant_id <- function(variant_id = NULL,
                                           verbose = FALSE,
                                           warnings = TRUE,
                                           page_size = 20L) {

  if(rlang::is_null(variant_id))
    return(associations())

  assertthat::assert_that(
    is.character(variant_id),
    length(variant_id) > 0,
    assertthat::noNA(variant_id))

  resource_urls <- sprintf("/%s/%s/%s", "singleNucleotidePolymorphisms",
                           urltools::url_encode(variant_id),
                           "associations")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty associations object.
  if(rlang::is_empty(responses_ok)) return(associations())

  obj <- plst_left_join(responses_ok)

  my_associations <- a_obj_to_associations(obj)

  return(my_associations)
}

#' Get GWAS Catalog associations by EFO identifier
#'
#' Gets associations whose phenotypic trait is matched by EFO identifiers.
#'
#' @param efo_id A character vector of \href{https://www.ebi.ac.uk/efo/}{EFO}
#'   identifiers.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{associations} object.
#' @keywords internal
get_associations_by_efo_id <- function(efo_id = NULL,
                                       verbose = FALSE,
                                       warnings = TRUE,
                                       page_size = 20L) {

  if(rlang::is_null(efo_id))
    return(associations())

  assertthat::assert_that(
    is.character(efo_id),
    length(efo_id) > 0,
    assertthat::noNA(efo_id),
    all(is_efo_id2(efo_id)))

  resource_urls <- sprintf("/%s/%s/%s", "efoTraits",
                           urltools::url_encode(efo_id),
                           "associations")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty associations object.
  if(rlang::is_empty(responses_ok)) return(associations())

  obj <- plst_left_join(responses_ok)

  my_associations <- a_obj_to_associations(obj)

  return(my_associations)
}

#' Get GWAS Catalog associations by PubMed identifiers
#'
#' Gets associations whose associated publications match
#'  \href{https://en.wikipedia.org/wiki/PubMed}{PubMed} identifiers.
#'
#' @param pubmed_id An \code{integer} vector of
#'   \href{https://en.wikipedia.org/wiki/PubMed}{PubMed} identifiers.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{associations} object.
#' @keywords internal
get_associations_by_pubmed_id <- function(pubmed_id = NULL,
                                          verbose = FALSE,
                                          warnings = TRUE,
                                          page_size = 20L) {

  if(rlang::is_null(pubmed_id))
    return(associations())

  assertthat::assert_that(
    is.character(pubmed_id),
    length(pubmed_id) > 0,
    assertthat::noNA(pubmed_id))

  resource_urls <- sprintf("/%s%s", "associations/search/findByPubmedId?pubmedId=",
                           urltools::url_encode(pubmed_id))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(associations())

  obj <- plst_left_join(responses_ok)

  my_associations <- a_obj_to_associations(obj)

  return(my_associations)
}

#' Get GWAS Catalog associations by EFO traits
#'
#' Gets associations that match \href{https://www.ebi.ac.uk/efo/}{EFO} trait
#' description.
#'
#' @param efo_trait A \code{character} vector of
#'   \href{https://www.ebi.ac.uk/efo/}{EFO} trait descriptions, e.g.,
#'   \code{'uric acid measurement'}.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{associations} object.
#' @keywords internal
get_associations_by_efo_trait <- function(efo_trait = NULL,
                                          verbose = FALSE,
                                          warnings = TRUE,
                                          page_size = 20L) {

  if(rlang::is_null(efo_trait))
    return(associations())

  assertthat::assert_that(
    is.character(efo_trait),
    length(efo_trait) > 0,
    assertthat::noNA(efo_trait))

  resource_urls <- sprintf("/%s%s", "associations/search/findByEfoTrait?efoTrait=",
                           urltools::url_encode(efo_trait))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty associations object.
  if(rlang::is_empty(responses_ok)) return(associations())

  obj <- plst_left_join(responses_ok)

  my_associations <- a_obj_to_associations(obj)

  return(my_associations)
}

#' Get all GWAS Catalog associations
#'
#' Gets all associations. Beware this can take a few hours!
#'
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{associations} object.
#' @keywords internal
get_associations_all <- function(verbose = FALSE,
                                 warnings = TRUE,
                                 page_size = 20L) {

  resource_urls <- "/associations"

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status ==
  # "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty associations
  # object.
  if(rlang::is_empty(responses_ok)) return(associations())

  obj <- plst_left_join(responses_ok)

  my_associations <- a_obj_to_associations(obj)

  return(my_associations)
}


#' Get GWAS Catalog associations
#'
#' Retrieves associations via the NHGRI-EBI GWAS Catalog REST API. The REST
#' API is queried multiple times with the criteria passed as arguments (see
#' below). By default all associations that match the criteria supplied in the
#' arguments are retrieved: this corresponds to the default option
#' \code{set_operation} set to \code{'union'}. If you rather have only the
#' associations that match simultaneously all criteria provided, then set
#' \code{set_operation} to \code{'intersection'}.
#'
#' Please note that all search criteria are vectorised, thus allowing for batch
#' mode search, e.g., one can search by multiple variant identifiers at once by
#' passing a vector of identifiers to \code{variant_id}.
#'
#' @param study_id A \code{character} vector of GWAS Catalog study accession
#'   identifiers.
#' @param association_id A \code{character} vector of GWAS Catalog association
#'   identifiers.
#' @param variant_id A \code{character} vector of GWAS Catalog variant
#'   identifiers.
#' @param efo_id A character vector of \href{https://www.ebi.ac.uk/efo/}{EFO}
#'   identifiers.
#' @param pubmed_id An \code{integer} vector of
#'   \href{https://en.wikipedia.org/wiki/PubMed}{PubMed} identifiers.
#' @param efo_trait A \code{character} vector of
#'   \href{https://www.ebi.ac.uk/efo/}{EFO} trait descriptions, e.g.,
#'   \code{'uric acid measurement'}.
#' @param set_operation Either \code{'union'} or \code{'intersection'}. This
#'   tells how associations retrieved by different criteria  should be combined:
#'   \code{'union'} binds together all results removing duplicates and
#'   \code{'intersection'} only keeps same associations found with different
#'   criteria.
#' @param interactive A logical. If all associations are requested, whether to ask
#'   interactively if we really want to proceed.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#'
#' @return An \linkS4class{associations} object.
#' @examples
#' # Get an association by study identifier
#' get_associations(study_id = 'GCST001085')
#'
#' # Get an association by association identifier
#' get_associations(association_id = '25389945')
#'
#' # Get associations by variant identifier
#' get_associations(variant_id = 'rs3798440')
#'
#' # Get associations by EFO trait identifier
#' get_associations(efo_id = 'EFO_0005537')
#'
#' @export
get_associations <- function(study_id = NULL,
                        association_id = NULL,
                        variant_id = NULL,
                        efo_id = NULL,
                        pubmed_id = NULL,
                        efo_trait = NULL,
                        set_operation = 'union',
                        interactive = TRUE,
                        verbose = FALSE,
                        warnings = TRUE) {

  if(!(rlang::is_scalar_character(set_operation) && set_operation %in% c('union', 'intersection')))
    stop("set_operation must be either 'union' or 'intersection'")

  if(!(rlang::is_scalar_logical(verbose) && verbose %in% c(TRUE, FALSE)))
    stop("verbose must be either TRUE or FALSE")

  if(!(rlang::is_scalar_logical(warnings) && warnings %in% c(TRUE, FALSE)))
    stop("warnings must be either TRUE or FALSE")

  list_of_associations = list()

  if (!rlang::is_null(study_id))
    list_of_associations[['associations_by_study_id']] <-
    get_associations_by_study_id(study_id = study_id,
                            verbose = verbose,
                            warnings = warnings)

  if (!rlang::is_null(association_id))
    list_of_associations[['associations_by_association_id']] <-
    get_associations_by_association_id(association_id = association_id,
                                  verbose = verbose,
                                  warnings = warnings)

  if (!rlang::is_null(variant_id))
    list_of_associations[['associations_by_variant_id']] <-
    get_associations_by_variant_id(variant_id = variant_id,
                              verbose = verbose,
                              warnings = warnings)

  if (!rlang::is_null(efo_id))
    list_of_associations[['associations_by_efo_id']] <-
    get_associations_by_efo_id(efo_id = efo_id,
                          verbose = verbose,
                          warnings = warnings)

  if (!rlang::is_null(pubmed_id))
    list_of_associations[['associations_by_pubmed_id']] <-
    get_associations_by_pubmed_id(pubmed_id = pubmed_id,
                             verbose = verbose,
                             warnings = warnings)

  if (!rlang::is_null(efo_trait))
    list_of_associations[['associations_by_efo_trait']] <-
    get_associations_by_efo_trait(efo_trait = efo_trait,
                             verbose = verbose,
                             warnings = warnings)

  # If no criteria have been passed, i.e. all are NULL then got fetch all
  # associations.
  if (rlang::is_empty(list_of_associations)) {
    msg1 <- "You are about to download all associations from the GWAS Catalog.\nThis might take a few hours."
    msg2 <- 'Returning an empty associations object!'
    msg3 <- 'OK! Getting all associations then. This is going to take a while...'
    if (interactive)
      default_answer = NULL  # i.e., use interactive mode.
    else
      default_answer = 'y'
    if (sure(before_question = msg1, after_saying_no = msg2, after_saying_yes = msg3, default_answer = default_answer))
      return(get_associations_all(verbose = verbose, warnings = warnings))
    else
      return(associations())
  } else {

    if (identical(set_operation, "union")) {
      return(purrr::reduce(list_of_associations, union))
    }

    if (identical(set_operation, "intersection")) {
      return(purrr::reduce(list_of_associations, intersect))
    }
  }

}
