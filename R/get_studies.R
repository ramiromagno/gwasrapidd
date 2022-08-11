#' Get GWAS Catalog studies by study identifiers
#'
#' Gets studies by GWAS Catalog internal study identifiers.
#'
#' @param study_id A character vector of GWAS Catalog study accession
#'   identifiers.
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{studies} object.
#' @keywords internal
get_studies_by_study_id <- function(study_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(study_id))
    return(studies())

  assertthat::assert_that(
              is.character(study_id),
              length(study_id) > 0,
              assertthat::noNA(study_id),
              all(is_study_id(study_id)))

  endpoint <- "/studies"
  resource_urls <- sprintf("%s/%s", endpoint, study_id)

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(studies())

  obj <- plst_left_join(responses_ok)
  my_studies <- obj_to_studies(obj)

  return(my_studies)
}

#' Get GWAS Catalog studies by association identifiers
#'
#' Gets studies by GWAS Catalog internal association identifiers.
#'
#' @param association_id A character vector of GWAS Catalog association
#'   identifiers.
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{studies} object.
#' @keywords internal
get_studies_by_association_id <- function(association_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(association_id))
    return(studies())

  assertthat::assert_that(
                          is.character(association_id),
                          length(association_id) > 0,
                          assertthat::noNA(association_id),
                          all(is_association_id(association_id)))

  resource_urls <- sprintf("/%s/%s/%s", "associations", association_id, "study")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(studies())

  obj <- plst_left_join(responses_ok)

  my_studies <- obj_to_studies(obj)

  return(my_studies)
}

#' Get GWAS Catalog studies by variant identifiers
#'
#' Gets studies by variant identifiers.
#'
#' @param variant_id A character vector of GWAS Catalog variant identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{studies} object.
#' @keywords internal
get_studies_by_variant_id <- function(variant_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(variant_id))
    return(studies())

  assertthat::assert_that(
                          is.character(variant_id),
                          length(variant_id) > 0,
                          assertthat::noNA(variant_id))

  resource_urls <- sprintf("/%s/%s/%s", "singleNucleotidePolymorphisms",
                           urltools::url_encode(variant_id),
                           "studies")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(studies())

  obj <- plst_left_join(responses_ok)

  my_studies <- obj_to_studies(obj)

  return(my_studies)
}

#' Get GWAS Catalog studies by EFO identifier
#'
#' Gets studies whose phenotypic trait is matched by EFO identifiers.
#'
#' @param efo_id A character vector of \href{https://www.ebi.ac.uk/efo/}{EFO}
#'   identifiers.
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{studies} object.
#' @keywords internal
get_studies_by_efo_id <- function(efo_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(efo_id))
    return(studies())

  assertthat::assert_that(
                          is.character(efo_id),
                          length(efo_id) > 0,
                          assertthat::noNA(efo_id),
                          all(is_efo_id2(efo_id)))

  resource_urls <- sprintf("/%s/%s/%s", "efoTraits",
                           urltools::url_encode(efo_id),
                           "studies")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(studies())

  obj <- plst_left_join(responses_ok)

  my_studies <- obj_to_studies(obj)

  return(my_studies)
}

#' Get GWAS Catalog studies by PubMed identifiers
#'
#' Gets studies whose associated publications match
#'  \href{https://pubmed.ncbi.nlm.nih.gov/}{PubMed} identifiers.
#'
#' @param pubmed_id An integer vector of
#'   \href{https://pubmed.ncbi.nlm.nih.gov/}{PubMed} identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{studies} object.
#' @keywords internal
get_studies_by_pubmed_id <- function(pubmed_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(pubmed_id))
    return(studies())

  assertthat::assert_that(
                          is.character(pubmed_id),
                          length(pubmed_id) > 0,
                          assertthat::noNA(pubmed_id))

  resource_urls <- sprintf("/%s%s", "studies/search/findByPublicationIdPubmedId?pubmedId=",
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
  if(rlang::is_empty(responses_ok)) return(studies())

  obj <- plst_left_join(responses_ok)

  my_studies <- obj_to_studies(obj)

  return(my_studies)
}

#' Get GWAS Catalog studies that have been requested by users or not
#'
#' Gets studies that have either been requested by users of the Catalog
#' or studies that have not been explicitly requested by users.
#'
#' @param user_requested A \code{logical} (scalar!) indicating to retrieve
#'   either studies requested by users of the Catalog (\code{TRUE}) or otherwise
#'   (\code{FALSE}).
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{studies} object.
#' @keywords internal
get_studies_by_user_requested <- function(
  user_requested = NULL,
  verbose = FALSE,
  warnings = TRUE,
  page_size = 20L) {

  if(rlang::is_null(user_requested))
    return(studies())

  assertthat::assert_that(
                          rlang::is_scalar_logical(user_requested),
                          assertthat::noNA(user_requested))

  user_requested_str <- ifelse(user_requested, 'true', 'false')
  resource_urls <- sprintf("/%s%s", "studies/search/findByUserRequested?userRequested=",
                           user_requested_str)

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  # If response is paginated then it tests if all responses (of each page) had status == 'OK'
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(studies())

  obj <- plst_left_join(responses_ok)

  my_studies <- obj_to_studies(obj)

  return(my_studies)
}

#' Get GWAS Catalog studies by full summary statistics criterion
#'
#' Gets studies that either have full summary statistics or studies that do not
#' have it.
#'
#' @param full_pvalue_set A \code{logical} (scalar!) indicating to retrieve
#'   studies with full summary statistics (\code{TRUE}) or studies without it
#'   (\code{FALSE}).
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{studies} object.
#' @keywords internal
get_studies_by_full_pvalue_set <- function(
  full_pvalue_set = NULL,
  verbose = FALSE,
  warnings = TRUE,
  page_size = 20L) {

  if(rlang::is_null(full_pvalue_set))
    return(studies())

  assertthat::assert_that(
                          rlang::is_scalar_logical(full_pvalue_set),
                          assertthat::noNA(full_pvalue_set))

  full_pvalue_set_str <- ifelse(full_pvalue_set, 'true', 'false')
  resource_urls <- sprintf("/%s%s", "studies/search/findByFullPvalueSet?fullPvalueSet=",
                           full_pvalue_set_str)

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  # If response is paginated then it tests if all responses (of each page) had status == 'OK'
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(studies())

  obj <- plst_left_join(responses_ok)

  my_studies <- obj_to_studies(obj)

  return(my_studies)
}

#' Get GWAS Catalog studies by EFO URIs
#'
#' Gets studies that match \href{https://www.ebi.ac.uk/efo/}{EFO} URI.
#'
#' @param efo_uri A character vector of
#'   \href{https://www.ebi.ac.uk/efo/}{EFO} URIs.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{studies} object.
#' @keywords internal
get_studies_by_efo_uri <- function(efo_uri = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(efo_uri))
    return(studies())

  assertthat::assert_that(
                          is.character(efo_uri),
                          length(efo_uri) > 0,
                          assertthat::noNA(efo_uri))

  # I really want to use URLencode here and not urltools::url_encode
  # as I often use elsewhere.
  resource_urls <- sprintf("/%s%s", "studies/search/findByEfoUri?uri=",
                           utils::URLencode(efo_uri, reserved = TRUE))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(studies())

  obj <- plst_left_join(responses_ok)

  my_studies <- obj_to_studies(obj)

  return(my_studies)
}

#' Get GWAS Catalog studies by EFO traits
#'
#' Gets studies that match \href{https://www.ebi.ac.uk/efo/}{EFO} trait
#' description.
#'
#' @param efo_trait A \code{character} vector of
#'   \href{https://www.ebi.ac.uk/efo/}{EFO} trait descriptions, e.g.,
#'   \code{'uric acid measurement'}.
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{studies} object.
#' @keywords internal
get_studies_by_efo_trait <- function(efo_trait = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(efo_trait))
    return(studies())

  assertthat::assert_that(
                          is.character(efo_trait),
                          length(efo_trait) > 0,
                          assertthat::noNA(efo_trait))

  resource_urls <- sprintf("/%s%s", "studies/search/findByEfoTrait?efoTrait=",
                           urltools::url_encode(efo_trait))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(studies())

  obj <- plst_left_join(responses_ok)

  my_studies <- obj_to_studies(obj)

  return(my_studies)
}

#' Get GWAS Catalog studies by reported traits
#'
#' Gets studies that match the reported traits, as reported by the original
#' authors' of the study.
#'
#' @param reported_trait A character vector of phenotypic traits as
#'   reported by the original authors' the study.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{studies} object.
#' @keywords internal
get_studies_by_reported_trait <- function(reported_trait = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(reported_trait))
    return(studies())

  assertthat::assert_that(
                          is.character(reported_trait),
                          length(reported_trait) > 0,
                          assertthat::noNA(reported_trait))

  resource_urls <- sprintf("/%s%s", "studies/search/findByDiseaseTrait?diseaseTrait=",
                           urltools::url_encode(reported_trait))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(studies())

  obj <- plst_left_join(responses_ok)

  my_studies <- obj_to_studies(obj)

  return(my_studies)
}

#' Get all GWAS Catalog studies
#'
#' Gets all studies. Beware this can take several minutes!
#'
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{studies} object.
#' @keywords internal
get_studies_all <- function(verbose = FALSE, warnings = TRUE, page_size = 20L) {

  resource_urls <- "/studies"

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(studies())

  obj <- plst_left_join(responses_ok)

  my_studies <- obj_to_studies(obj)

  return(my_studies)
}

#' Get GWAS Catalog studies
#'
#' Retrieves studies via the NHGRI-EBI GWAS Catalog REST API. The REST
#' API is queried multiple times with the criteria passed as arguments (see
#' below). By default all studies that match the criteria supplied in the
#' arguments are retrieved: this corresponds to the default option
#' \code{set_operation} set to \code{'union'}. If you rather have only the
#' studies that match simultaneously all criteria provided, then set
#' \code{set_operation} to \code{'intersection'}.
#'
#' Please note that all search criteria are vectorised, thus allowing for batch
#' mode search, e.g., one can search by multiple variant identifiers at once by
#' passing a vector of identifiers to \code{variant_id}.
#'
#' @param study_id A character vector of GWAS Catalog study accession
#'   identifiers.
#' @param association_id A character vector of GWAS Catalog association
#'   identifiers.
#' @param variant_id A character vector of GWAS Catalog variant identifiers.
#' @param efo_id A character vector of \href{https://www.ebi.ac.uk/efo/}{EFO}
#'   identifiers.
#' @param pubmed_id An integer vector of
#'   \href{https://en.wikipedia.org/wiki/PubMed}{PubMed} identifiers.
#' @param user_requested A \code{logical} (scalar!) indicating to retrieve either studies
#'   requested by users of the Catalog (\code{TRUE}) or otherwise
#'   (\code{FALSE}).
#' @param full_pvalue_set A \code{logical} (scalar!) indicating to retrieve studies with full
#'   summary statistics (\code{TRUE}) or studies without it (\code{FALSE}).
#' @param efo_uri A character vector of \href{https://www.ebi.ac.uk/efo/}{EFO} URIs.
#' @param efo_trait A character vector of
#'   \href{https://www.ebi.ac.uk/efo/}{EFO} trait descriptions, e.g.,
#'   \code{'uric acid measurement'}.
#' @param reported_trait A character vector of phenotypic traits as
#'   reported by the original authors of the study.
#' @param set_operation Either \code{'union'} or \code{'intersection'}. This
#'   tells how studies retrieved by different criteria  should be combined:
#'   \code{'union'} binds together all results removing duplicates and
#'   \code{'intersection'} only keeps same studies found with different
#'   criteria.
#' @param interactive A logical. If all studies are requested, whether to ask
#'   interactively if we really want to proceed.
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A \linkS4class{studies} object.
#' @examples
#' \dontrun{
#' # Get a study by its accession identifier
#' get_studies(study_id = 'GCST001085', warnings = FALSE)
#'
#' # Get a study by association identifier
#' get_studies(association_id = '25389945', warnings = FALSE)
#'
#' # Get studies by variant identifier
#' get_studies(variant_id = 'rs3798440', warnings = FALSE)
#'
#' # Get studies by EFO trait identifier
#' get_studies(efo_id = 'EFO_0005537', warnings = FALSE)
#' }
#'
#' @export
get_studies <- function(study_id = NULL,
                        association_id = NULL,
                        variant_id = NULL,
                        efo_id = NULL,
                        pubmed_id = NULL,
                        user_requested = NULL,
                        full_pvalue_set = NULL,
                        efo_uri = NULL,
                        efo_trait = NULL,
                        reported_trait = NULL,
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

  list_of_studies = list()

  if (!rlang::is_null(study_id))
    list_of_studies[['studies_by_study_id']] <-
      get_studies_by_study_id(study_id = study_id,
                              verbose = verbose,
                              warnings = warnings)

  if (!rlang::is_null(association_id))
    list_of_studies[['studies_by_association_id']] <-
      get_studies_by_association_id(association_id = association_id,
                                    verbose = verbose,
                                    warnings = warnings)

  if (!rlang::is_null(variant_id))
    list_of_studies[['studies_by_variant_id']] <-
      get_studies_by_variant_id(variant_id = variant_id,
                                verbose = verbose,
                                warnings = warnings)

  if (!rlang::is_null(efo_id))
    list_of_studies[['studies_by_efo_id']] <-
      get_studies_by_efo_id(efo_id = efo_id,
                            verbose = verbose,
                            warnings = warnings)

  if (!rlang::is_null(pubmed_id))
    list_of_studies[['studies_by_pubmed_id']] <-
      get_studies_by_pubmed_id(pubmed_id = pubmed_id,
                               verbose = verbose,
                               warnings = warnings)

  if (!rlang::is_null(user_requested))
    list_of_studies[['studies_by_user_requested']] <-
      get_studies_by_user_requested(user_requested = user_requested,
                                    verbose = verbose,
                                    warnings = warnings)

  if (!rlang::is_null(full_pvalue_set))
    list_of_studies[['studies_by_full_pvalue_set']] <-
      get_studies_by_full_pvalue_set(full_pvalue_set = full_pvalue_set,
                                     verbose = verbose,
                                     warnings = warnings)

  if (!rlang::is_null(efo_uri))
    list_of_studies[['studies_by_efo_uri']] <-
      get_studies_by_efo_uri(efo_uri = efo_uri,
                             verbose = verbose,
                             warnings = warnings)

  if (!rlang::is_null(efo_trait))
    list_of_studies[['studies_by_efo_trait']] <-
      get_studies_by_efo_trait(efo_trait = efo_trait,
                               verbose = verbose,
                               warnings = warnings)

  if (!rlang::is_null(reported_trait))
    list_of_studies[['studies_by_reported_trait']] <-
      get_studies_by_reported_trait(reported_trait = reported_trait,
                                    verbose = verbose,
                                    warnings = warnings)

  # If no criteria have been passed, i.e. all are NULL then got fetch all
  # studies.
  if(rlang::is_empty(list_of_studies)) {
    msg1 <- "You are about to download all studies from the GWAS Catalog.\nThis might take several minutes."
    msg2 <- 'Returning an empty studies object!'
    msg3 <- 'OK! Getting all studies then. This is going to take a while...'
    if(interactive)
      default_answer = NULL  # i.e., use interactive mode.
    else
      default_answer = 'y'
    if(sure(before_question = msg1, after_saying_no = msg2, after_saying_yes = msg3, default_answer = default_answer))
      return(get_studies_all(verbose = verbose, warnings = warnings))
    else
      return(studies())
  } else {

    if (identical(set_operation, "union")) {
      return(purrr::reduce(list_of_studies, union))
    }

    if (identical(set_operation, "intersection")) {
      return(purrr::reduce(list_of_studies, intersect))
    }
  }

}
