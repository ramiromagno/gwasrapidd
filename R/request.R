gwas_rest_api_base_url <- "https://www.ebi.ac.uk/gwas/rest/api"
user_agent_id <- httr::user_agent("gwasrapidd: GWAS R API Data Download")

#' Request a GWAS Catalog REST API endpoint
#'
#' Performs a GET request on the specified \code{resource_url}.
#'
#' @param resource_url Endpoint URL. The endpoint is internally appended to the
#'   \code{base_url}. It should start with a forward slash (\code{/}).
#' @param base_url The GWAS REST API base URL (one should not need to change its
#'   default value).
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @param flatten Whether to flatten out the list returned by
#'   \code{jsonlite::fromJSON}.
#'
#' @return A list four named elements:
#' \describe{
#' \item{url}{The URL endpoint.}
#' \item{response_code}{\href{https://en.wikipedia.org/wiki/List_of_HTTP_status_codes}{HTTP
#' status code}.}
#' \item{status}{A string describing the status of the response obtained. It is
#' "OK" if everything went OK or some other string describing the problem
#' otherwise.}
#' \item{content}{The parsed JSON as a nested list, as returned by
#' \code{jsonlite::fromJSON}.}
#' }
#'
#' @keywords internal
gc_request <- function(resource_url = "/", base_url = gwas_rest_api_base_url,
                       verbose = FALSE, warnings = TRUE, flatten = FALSE) {

  if(verbose) message(glue::glue("Base URL: {base_url}."))

  url <- stringr::str_c(base_url, resource_url)
  if(verbose) message(glue::glue("Requesting resource: {url}."))

  if(verbose) message(glue::glue("Using the user agent: {user_agent_id$options$useragent}."))
  response <- httr::GET(url, user_agent_id)

  response_code <- status_code(response)
  if(verbose) message(glue::glue("Response code: {response_code}."))

  # Response object (a list of four elements):
  #   - url: the resource URL
  #   - response_code: response code
  #   - status: short remark about what went wrong with the response,
  #     or OK if successful.
  #   - response content: the content of the parsed JSON response, or NULL if
  #     not successful.
  obj <- list(url = url,
              response_code = response_code,
              status = NA_character_,
              content = NULL)

  # If response is not 200, i.e. if request did not complete successfully then
  # return an empty response object (NULL) and warn about the response code.
  if (!identical(response_code, 200L)) {
    wrg_msg <-glue::glue("The request for {url} failed: response code was {response_code}.")
    if(warnings) {
      warning(wrg_msg, immediate. = TRUE, call. = FALSE)
    }
    obj$status <- wrg_msg
    return(obj)
  } else { # Else response code is 200 and we move on to JSON parsing.

  # Check if the content type of the response is JSON.
  content_type <- httr::http_type(response)
  if(verbose) message(glue::glue("Response content type: {content_type}."))
  if (!identical(content_type, "application/json")) {
    if(warnings) {
    wrg_msg <- glue::glue("Response to {url} did not return JSON!")
    warning(wrg_msg, immediate. = TRUE, call. = FALSE)
    }
    obj$status <- "Response content was not application/json."
    return(obj)
  }

  # Parse JSON content
  content <- jsonlite::fromJSON(httr::content(response, "text"), flatten = flatten)

  obj$status <- "OK"
  # missing_to_na job here is to map those nasty NULLs to NA
  # For more about this problem:
  # https://www.lively-web.org/R-libraries/RJSONIO/doc/missingValues.html
  obj$content <- missing_to_na(content)
  #obj$content <- content
  return(obj)
  }
}

#' Adds an element named <obj_type> to content sub-element.
#'
#' This function takes a list, sees if it has an element named 'content' and
#' then checks if content contains an element named \code{obj_type}: if it does
#' not contain an element as passed in \code{obj_type} this function adds it,
#' otherwise it leaves the list \code{obj} untouched. The contents of 'content'
#' are set as value of \code{<obj>$content$<obj_type>}.
#'
#' @param obj A list.
#' @param obj_type A non-empty string.
#'
#' @return A list containing the \code{<obj>$content$<obj_type>}, whose value is
#'   either \code{NULL} if it had not any value before or the value of
#'   \code{<obj>$content}.
#' @keywords internal
add_object_tier <- function(obj, obj_type) {
  if(!rlang::is_list(obj))
    stop("obj must be a list.")

  if(!rlang::is_scalar_character(obj_type))
    stop("obj_type must be a string.")

  if(identical(obj_type, ""))
    stop("obj_type cannot be an empty string")

  if(rlang::is_empty(obj)) {
    lst <- list()
    lst$content[obj_type] <- list(NULL)
    return(lst)
  }

  has_tier_already <- rlang::has_name(obj$content, obj_type)
  if(has_tier_already)
    return(obj)
  else {
    content <- obj$content
    obj$content <- NULL
    obj$content[[obj_type]] <- content
    return(obj)
  }
}

#' Identify the GWAS object entity from the URL endpoint
#'
#' This function takes URL endpoints and returns one of: studies, associations,
#' variants or traits.
#'
#' @param resource_url A character vector of GWAS URL endpoints.
#'
#' @return A character vector of either \code{"studies"}, \code{"associations"},
#'   \code{"variants"} or \code{"traits"}.
#'
#' @keywords internal
object_type_from_url <- Vectorize(function(resource_url) {

  # Types of entities returned by the GWAS Catalog
  obj_types <- c(studies = "studies",
                 associations = "associations",
                 variants = "variants",
                 traits = "traits")

  patterns <- c(
    studies =      "^(https://www.ebi.ac.uk/gwas/rest/api)?/studies(/\\w*/?)?$",
    studies =      "^(https://www.ebi.ac.uk/gwas/rest/api)?/associations/\\w+/study/?$",
    studies =      "^(https://www.ebi.ac.uk/gwas/rest/api)?/singleNucleotidePolymorphisms/.+/studies/?$",
    studies =      "^(https://www.ebi.ac.uk/gwas/rest/api)?/efoTraits/\\w+/studies/?$",
    studies =      "^(https://www.ebi.ac.uk/gwas/rest/api)?/studies/search/.*$",
    associations = "^(https://www.ebi.ac.uk/gwas/rest/api)?/studies/\\w+/associations/?$",
    associations = "^(https://www.ebi.ac.uk/gwas/rest/api)?/associations(/\\w*/?)?$",
    associations = "^(https://www.ebi.ac.uk/gwas/rest/api)?/singleNucleotidePolymorphisms/.+/associations/?$",
    associations = "^(https://www.ebi.ac.uk/gwas/rest/api)?/efoTraits/\\w+/associations/?$",
    associations = "^(https://www.ebi.ac.uk/gwas/rest/api)?/associations/search/.*$",
    variants =     "^(https://www.ebi.ac.uk/gwas/rest/api)?/studies/\\w+/snps/?$",
    variants =     "^(https://www.ebi.ac.uk/gwas/rest/api)?/associations/\\w+/snps/?$",
    variants =     "^(https://www.ebi.ac.uk/gwas/rest/api)?/singleNucleotidePolymorphisms(/[^/]*/?)?$",
    variants =     "^(https://www.ebi.ac.uk/gwas/rest/api)?/singleNucleotidePolymorphisms/search/.*$",
    traits =    "^(https://www.ebi.ac.uk/gwas/rest/api)?/studies/\\w+/efoTraits/?$",
    traits =    "^(https://www.ebi.ac.uk/gwas/rest/api)?/associations/\\w+/efoTraits/?$",
    traits =    "^(https://www.ebi.ac.uk/gwas/rest/api)?/efoTraits(/\\w*/?)?$",
    traits =    "^(https://www.ebi.ac.uk/gwas/rest/api)?/efoTraits/search/.*$"
  )

  match <- str_detect(resource_url, patterns)

  if(identical(as.integer(sum(match)), 0L))
    stop("No pattern matched the URL: ", resource_url, ".")

  # Only one object type should be matched.
  obj_type <- unique(obj_types[names(patterns)[match]])
  if(length(obj_type) > 1L) {
    stop("More than one object type possible for the URL: ",
         resource_url, ": ",
         cc_and(obj_type),
         ".")
  }

  names(obj_type) <- NULL

  return(obj_type)

}, USE.NAMES = FALSE)

#' Is the GWAS response paginated?
#'
#' Checks if the response is paginated by checking if an element named 'page'
#' exists.
#'
#' @param content The response content as return by \code{jsonlite::fromJSON}.
#'
#' @return A logical value.
#'
#' @keywords internal
is_paginated <- function(content) 'page' %in% names(content)


#' Request a paginated GWAS Catalog REST API endpoint
#'
#' Performs a GET request on the specified \code{resource_url} and all its
#' pages.
#'
#' @param resource_url Endpoint URL. The endpoint is internally appended to the
#'   \code{base_url}. It should start with a forward slash (\code{/}).
#' @param base_url The GWAS REST API base URL (one should not need to change its
#'   default value).
#' @param page_size Page parameter used in the URL endpoint.
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @param progress_bar Whether to show a progress bar as the paginated resources
#'   are retrieved.
#'
#' @return A list four named elements:
#' \describe{
#' \item{url}{The URL endpoint.}
#' \item{response_code}{\href{https://en.wikipedia.org/wiki/List_of_HTTP_status_codes}{HTTP
#' status code}.}
#' \item{status}{A string describing the status of the response obtained. It is
#' "OK" if everything went OK or some other string describing the problem
#' otherwise.}
#' \item{content}{The parsed JSON as a nested list, as returned by
#' \code{jsonlite::fromJSON}.}
#' }
#'
#' @keywords internal
gc_request_all <- function(resource_url = "/",
                           base_url = gwas_rest_api_base_url,
                           page_size = 20L,
                           verbose = FALSE,
                           warnings = TRUE,
                           progress_bar = TRUE) {

  # response object
  obj <- gc_request(resource_url = resource_url, base_url = base_url,
                    verbose = verbose, warnings = warnings)

  if(warnings && !identical(obj$status, "OK")) {
    warning(obj$status)
    return(obj)
  }

  # Is the response paginated?
  is_it_paginated <- is_paginated(obj$content)

  # If it is not paginated return the obtained object.
  if(!is_it_paginated) return(obj)

  # Else, let's go after for those paginated resources.
  pagination <- obj$content$page

  # Check if page_size is within 1 and 1000.
  if(!rlang::is_scalar_integer(page_size) || page_size < 1L || page_size > 1000L)
    stop("page_size must be an integer scalar between 1 and 1000!")

  # Now we calculate the number of pages needed to cover the number
  # of elements in chunks of length = page_size
  n_pages <- (pagination[["totalElements"]] - 1L) %/% page_size + 1L

  # This is a hack!
  # For instance the URL:
  # https://www.ebi.ac.uk/ols/api/ontologies/efo/descendants?id=EFO_0009285
  # has a page object but lists:
  # $content$page$totalElements
  # [1] 0
  # So this next line deals with this exception.
  if(identical(n_pages, 0L)) n_pages <- 1L
  # Generate page indices, it starts at 0 and goes up to totalPages - 1.
  page_indices <- seq.int(0, n_pages - 1)

  # Generate each individual resource URL one per page
  if(contains_question_mark(resource_url)) {
    resource_url_by_page <- sprintf("%s&page=%d&size=%d", resource_url, page_indices, page_size)
  } else {
    resource_url_by_page <- sprintf("%s?page=%d&size=%d", resource_url, page_indices, page_size)
  }

  # Progress bar
  if(progress_bar) {
    pb <- progress::progress_bar$new(total = n_pages,
                                     show_after = 2,
                                     format = "  downloading [:bar] :percent eta: :eta")
    gc_request2 <- function(resource_url = "/",
                              base_url = base_url,
                              verbose = FALSE,
                              warnings = TRUE,
                              flatten = FALSE) {
      pb$tick()
      gc_request(resource_url = resource_url,
                 base_url = base_url,
                 verbose = verbose,
                 warnings = warnings,
                 flatten = flatten)
    }
  } else {
    gc_request2 <- gc_request
  }


  # Each element of this list is a returned object from a page
  objs <-purrr::map(resource_url_by_page,
                    gc_request2,
                    base_url = base_url,
                    verbose = verbose,
                    warnings = warnings,
                    flatten = FALSE)

  is_ok <- purrr::map_lgl(objs, ~ identical(.x$status, "OK"))

  if(!all(is_ok))
    stop(glue::glue("Failed to get all pages of {resource_url}!"))

  # Let's join all those pages into one object only
  return(plst_inner_join(objs))
}

#' Is the GWAS response wrapped in an '_embedded' object?
#'
#' Checks if the response is wrapped in an '_embedded' object by checking if an
#' element named '_embedded' exists.
#'
#' @param obj The response object as return by \code{jsonlite::fromJSON}.
#'
#' @return A logical value.
#'
#' @keywords internal
is_embedded <- function(obj) utils::hasName(obj$content, '_embedded')

#' Normalise a JSON-list.
#'
#' This function normalises a JSON-list. The concept of JSON-list is here
#' defined as an ordinary R list object whose elements are either all named or
#' none is named. These lists map naturally to JSON elements: objects and
#' arrays. What this function does is wrap certain elements of the nested
#' list \code{obj} in \code{list()} to make the tree structure of the \code{obj}
#' list homologous across different responses. This makes all responses of the
#' same entity type (studies, associations, variants or traits) homologous and
#' hence joinable with family functions \code{lst_*_join}.
#'
#' This normalisation is GWAS object type specific. The parameter
#' \code{resource_url} should be mappable to either studies, associations,
#' variants or traits by \code{\link{object_type_from_url}}.
#'
#' @param obj A JSON-list. This is just an ordinary list as returned by
#'   \code{\link{gc_request}} or \code{\link{gc_request_all}}.
#' @param resource_url The URL endpoint used to obtain the JSON-list \code{obj}.
#'   This is used to infer the type of GWAS entity returned. See
#'   \code{\link{object_type_from_url}} for more details.
#'
#' @return A normalised JSON-list.
#' @keywords internal
normalise_obj <- function(obj, resource_url) {

  # If is embedded then object is not of scalar type
  # and there is no need to normalise it, so it return it
  # as is.
  if (is_embedded(obj))
    return(obj)

  # We remove names from obj_type so that identical() down there works.
  #obj_type <- unname(object_type_from_url(resource_url))
  obj_type <- object_type_from_url(resource_url)
  # n_obj: normalised object
  n_obj <- obj

  # normalise a studies object type
  if(identical(obj_type, "studies")) {
    n_obj <- add_object_tier(obj, "studies")
    n_obj$content$studies$platforms <- list(n_obj$content$studies$platforms)
    n_obj$content$studies$ancestries <- list(n_obj$content$studies$ancestries)
    n_obj$content$studies$genotypingTechnologies <- list(n_obj$content$studies$genotypingTechnologies)
    return(n_obj)
  }

  # normalise an associations object type
  if(identical(obj_type, "associations")) {
    n_obj <- add_object_tier(obj, "associations")
    n_obj$content$associations$loci <- list(n_obj$content$associations$loci)
    return(n_obj)
  }

  # normalise a variant object type
  if(identical(obj_type, "variants")) {
    n_obj <- add_object_tier(obj, "singleNucleotidePolymorphisms")
    n_obj$content$singleNucleotidePolymorphisms$locations <- list(n_obj$content$singleNucleotidePolymorphisms$locations)
    n_obj$content$singleNucleotidePolymorphisms$genomicContexts <- list(n_obj$content$singleNucleotidePolymorphisms$genomicContexts)
    return(n_obj)
  }

  # normalise a traits object type
  if(identical(obj_type, "traits")) {
    n_obj <- add_object_tier(obj, "efoTraits")
    return(n_obj)
  }

  # We should not get to this point. If obj_type was not one of:
  # studies, associations, variants or traits then object_type_from_url would
  # have already returned with an error.
}

#' Peels off the _embedded tier from a JSON-list.
#'
#' This function removes an element named \code{_embedded} from the element
#' \code{content} moving all of its contents one level up. If
#' \code{obj$content$`_embedded`} does not exist then returns \code{obj}
#' untouched.
#'
#' @param obj A JSON-list.
#'
#' @return A JSON-list.
#' @keywords internal
peel_off_embedded <- function(obj) {

  if(is_embedded(obj)) {
    # peel off the _embedded tier
    obj$content <- obj$content$`_embedded`
    return(obj)
  } else { # we're facing a scalar type of object.
    return(obj)
  }
}

#' Get a GWAS Catalog resource
#'
#' This function gets a GWAS Catalog by URL endpoint. The response must
#' correspond to one of the four types of entities: studies, associations,
#' variants or traits.
#'
#' @param resource_url Endpoint URL. The endpoint is internally appended to the
#'   \code{base_url}. It should start with a forward slash (\code{/}).
#' @param base_url The GWAS REST API base URL (one should not need to change its
#'   default value).
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @param page_size Page parameter used in the URL endpoint.
#'
#' @return A normalised JSON-list corresponding to either studies, associations,
#'   variants or traits.
#'
#' @keywords internal
gc_get <- function(resource_url,
                   base_url = gwas_rest_api_base_url,
                   verbose = FALSE,
                   warnings = TRUE,
                   page_size = 20L) {

  if(!rlang::is_scalar_character(resource_url))
    stop('resource_url must be a single string.')

  if(!rlang::is_scalar_character(base_url))
    stop('base_url must be a single string.')

  if(!rlang::is_scalar_integer(page_size) || page_size < 1L || page_size > 1000L)
    stop("page_size must be an integer scalar between 1 and 1000!")

  # If resource is paginated get all the paginated resources.
  obj <- gc_request_all(resource_url = resource_url,
                 base_url = base_url,
                 verbose = verbose,
                 warnings = warnings,
                 page_size = page_size)

  # If object is of scalar type, normalise it.
  obj <- normalise_obj(obj, resource_url)

  # Peel off the "_embedded" tier if that is the case.
  obj <- peel_off_embedded(obj)

  return(obj)
}

