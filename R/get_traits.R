#' Get GWAS Catalog traits by study identifiers
#'
#' Gets traits by GWAS Catalog internal study identifiers.
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
#' @return A \linkS4class{traits} object.
#' @keywords internal
get_traits_by_study_id <- function(study_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(study_id))
    return(traits())

  assertthat::assert_that(
    is.character(study_id),
    length(study_id) > 0,
    assertthat::noNA(study_id),
    all(is_study_id(study_id)))

  resource_urls <- sprintf("/%s/%s/%s", "studies", study_id, "efoTraits")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty traits object.
  if(rlang::is_empty(responses_ok)) return(traits())

  obj <- plst_left_join(responses_ok)
  my_traits <- e_obj_to_traits(obj)

  return(my_traits)
}


#' Get GWAS Catalog traits by association identifiers
#'
#' Gets traits by GWAS Catalog internal association identifiers.
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
#' @return A \linkS4class{traits} object.
#' @keywords internal
get_traits_by_association_id <- function(association_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(association_id))
    return(traits())

  assertthat::assert_that(
    is.character(association_id),
    length(association_id) > 0,
    assertthat::noNA(association_id),
    all(is_association_id(association_id)))

  resource_urls <- sprintf("/%s/%s/%s", "associations", association_id, "efoTraits")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty traits object.
  if(rlang::is_empty(responses_ok)) return(traits())

  obj <- plst_left_join(responses_ok)

  my_traits <- e_obj_to_traits(obj)

  return(my_traits)
}

#' Get GWAS Catalog traits by EFO identifier
#'
#' Gets traits whose phenotypic trait is matched by EFO identifiers.
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
#' @return A \linkS4class{traits} object.
#' @keywords internal
get_traits_by_efo_id <- function(efo_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(efo_id))
    return(traits())

  assertthat::assert_that(
    is.character(efo_id),
    length(efo_id) > 0,
    assertthat::noNA(efo_id),
    all(is_efo_id2(efo_id)))

  resource_urls <- sprintf("%s/%s", "/efoTraits", urltools::url_encode(efo_id))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty traits object.
  if(rlang::is_empty(responses_ok)) return(traits())

  obj <- plst_left_join(responses_ok)

  my_traits <- e_obj_to_traits(obj)

  return(my_traits)
}

#' Get GWAS Catalog traits by PubMed identifiers
#'
#' Gets traits whose associated publications match
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
#' @return A \linkS4class{traits} object.
#' @keywords internal
get_traits_by_pubmed_id <- function(pubmed_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(pubmed_id))
    return(traits())

  assertthat::assert_that(
    is.character(pubmed_id),
    length(pubmed_id) > 0,
    assertthat::noNA(pubmed_id))

  resource_urls <- sprintf("%s%s", "/efoTraits/search/findByPubmedId?pubmedId=",
                           urltools::url_encode(pubmed_id))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty traits object.
  if(rlang::is_empty(responses_ok)) return(traits())

  obj <- plst_left_join(responses_ok)

  my_traits <- e_obj_to_traits(obj)

  return(my_traits)
}

#' Get GWAS Catalog traits by EFO URIs
#'
#' Gets traits that match \href{https://www.ebi.ac.uk/efo/}{EFO} URI.
#'
#' @param efo_uri A \code{character} vector of
#'   \href{https://www.ebi.ac.uk/efo/}{EFO} URIs.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{traits} object.
#' @keywords internal
get_traits_by_efo_uri <- function(efo_uri = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(efo_uri))
    return(traits())

  assertthat::assert_that(
    is.character(efo_uri),
    length(efo_uri) > 0,
    assertthat::noNA(efo_uri))

  # I really want to use URLencode here and not urltools::url_encode
  # as I often use elsewhere.
  resource_urls <- sprintf("%s%s", "/efoTraits/search/findByEfoUri?uri=",
                           utils::URLencode(efo_uri, reserved = TRUE))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty traits object.
  if(rlang::is_empty(responses_ok)) return(traits())

  obj <- plst_left_join(responses_ok)

  my_traits <- e_obj_to_traits(obj)

  return(my_traits)
}

#' Get GWAS Catalog traits by EFO traits
#'
#' Gets traits that match \href{https://www.ebi.ac.uk/efo/}{EFO} trait
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
#' @return A \linkS4class{traits} object.
#' @keywords internal
get_traits_by_efo_trait <- function(efo_trait = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(efo_trait))
    return(traits())

  assertthat::assert_that(
    is.character(efo_trait),
    length(efo_trait) > 0,
    assertthat::noNA(efo_trait))

  resource_urls <- sprintf("%s%s", "/efoTraits/search/findByEfoTrait?trait=",
                           urltools::url_encode(efo_trait))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty traits object.
  if(rlang::is_empty(responses_ok)) return(traits())

  obj <- plst_left_join(responses_ok)

  my_traits <- e_obj_to_traits(obj)

  return(my_traits)
}

#' Get all GWAS Catalog EFO traits
#'
#' Gets all EFO traits. Beware this can take several minutes!
#'
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{traits} object.
#' @keywords internal
get_traits_all <- function(verbose = FALSE, warnings = TRUE, page_size = 20L) {

  resource_urls <- "/efoTraits"

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty traits object.
  if(rlang::is_empty(responses_ok)) return(traits())

  obj <- plst_left_join(responses_ok)

  my_traits <- e_obj_to_traits(obj)

  return(my_traits)
}

#' Get GWAS Catalog EFO traits
#'
#' Retrieves traits via the NHGRI-EBI GWAS Catalog REST API. The REST
#' API is queried multiple times with the criteria passed as arguments (see
#' below). By default all traits that match the criteria supplied in the
#' arguments are retrieved: this corresponds to the default option
#' \code{set_operation} set to \code{'union'}. If you rather have only the
#' traits that match simultaneously all criteria provided, then set
#' \code{set_operation} to \code{'intersection'}.
#'
#' Please note that all search criteria are vectorised, thus allowing for batch
#' mode search, e.g., one can search by multiple trait identifiers at once by
#' passing a vector of identifiers to \code{efo_id}.
#'
#' @param study_id A \code{character} vector of GWAS Catalog study accession
#'   identifiers.
#' @param association_id A \code{character} vector of GWAS Catalog association
#'   identifiers.
#' @param efo_id A character vector of \href{https://www.ebi.ac.uk/efo/}{EFO}
#'   identifiers.
#' @param pubmed_id An \code{integer} vector of
#'   \href{https://en.wikipedia.org/wiki/PubMed}{PubMed} identifiers.
#' @param efo_uri A \code{character} vector of \href{https://www.ebi.ac.uk/efo/}{EFO} URIs.
#' @param efo_trait A \code{character} vector of
#'   \href{https://www.ebi.ac.uk/efo/}{EFO} trait descriptions, e.g.,
#'   \code{'uric acid measurement'}.
#' @param set_operation Either \code{'union'} or \code{'intersection'}. This
#'   tells how traits retrieved by different criteria  should be combined:
#'   \code{'union'} binds together all results removing duplicates and
#'   \code{'intersection'} only keeps same traits found with different
#'   criteria.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#'
#' @return A \linkS4class{traits} object.
#' @examples
#' # Get traits by study identifier
#' get_traits(study_id = 'GCST001085')
#'
#' # Get traits by association identifier
#' get_traits(association_id = '25389945')
#'
#' # Get a trait by its EFO identifier
#' get_traits(efo_id = 'EFO_0005537')
#'
#' @export
get_traits <- function(study_id = NULL,
                       association_id = NULL,
                       efo_id = NULL,
                       pubmed_id = NULL,
                       efo_uri = NULL,
                       efo_trait = NULL,
                       set_operation = 'union',
                       verbose = FALSE,
                       warnings = TRUE) {


  if(!(rlang::is_scalar_character(set_operation) && set_operation %in% c('union', 'intersection')))
    stop("set_operation must be either 'union' or 'intersection'")

  if(!(rlang::is_scalar_logical(verbose) && verbose %in% c(TRUE, FALSE)))
    stop("verbose must be either TRUE or FALSE")

  if(!(rlang::is_scalar_logical(warnings) && warnings %in% c(TRUE, FALSE)))
    stop("warnings must be either TRUE or FALSE")

  list_of_traits = list()

  if (!rlang::is_null(study_id))
    list_of_traits[['traits_by_study_id']] <-
    get_traits_by_study_id(study_id = study_id,
                                 verbose = verbose,
                                 warnings = warnings)

  if (!rlang::is_null(association_id))
    list_of_traits[['traits_by_association_id']] <-
    get_traits_by_association_id(association_id = association_id,
                                       verbose = verbose,
                                       warnings = warnings)

  if (!rlang::is_null(efo_id))
    list_of_traits[['traits_by_efo_id']] <-
    get_traits_by_efo_id(efo_id = efo_id,
                               verbose = verbose,
                               warnings = warnings)

  if (!rlang::is_null(pubmed_id))
    list_of_traits[['traits_by_pubmed_id']] <-
    get_traits_by_pubmed_id(pubmed_id = pubmed_id,
                                  verbose = verbose,
                                  warnings = warnings)

  if (!rlang::is_null(efo_uri))
    list_of_traits[['traits_by_efo_uri']] <-
    get_traits_by_efo_uri(efo_uri = efo_uri,
                           verbose = verbose,
                           warnings = warnings)

  if (!rlang::is_null(efo_trait))
    list_of_traits[['traits_by_efo_trait']] <-
    get_traits_by_efo_trait(efo_trait = efo_trait,
                                  verbose = verbose,
                                  warnings = warnings)

  # If no criteria have been passed, i.e. all are NULL then go fetch all
  # EFO traits.
  if(rlang::is_empty(list_of_traits))
    return(get_traits_all(verbose = verbose, warnings = warnings))

  if(identical(set_operation, "union")) {
    return(purrr::reduce(list_of_traits, union))
  }

  if(identical(set_operation, "intersection")) {
    return(purrr::reduce(list_of_traits, intersect))
  }

}
