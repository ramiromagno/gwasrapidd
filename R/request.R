# GWAS Catalog REST API options
## Maximum page size
max_page_size <- 500

gwas_rest_api_base_url <- "https://www.ebi.ac.uk/gwas/rest/api"
user_agent_id <- httr::user_agent("gwasrapidd: GWAS R API Data Download.")

#' Number of elements in response of a GWAS Catalog REST API request.
#'
#' Gets the number of elements in a response (\code{list}) as returned by
#' \code{\link{request}}.
#'
#' @param response A response (\code{list}) as returned by \code{\link{request}}.
#'
#' @return An integer value.
#'
#' @export
no_content_elements <- function(response)
  return(response$content$page$totalElements)


#' Did the response to a GWAS Catalog REST API request contain no elements?
#'
#' Checks if the response contained no elements, i.e., if the content is empty.
#'
#' @param response A response (\code{list}) as returned by \code{\link{request}}.
#'
#' @return A logical value.
#'
#' @export
is_content_empty <- function(response)
  identical(response$content$page$totalElements, 0L)

#' Was the response to a GWAS Catalog REST API request successful?
#'
#' Checks if the response code of a GWAS Catalog REST API request is
#' \code{200L}.
#'
#' @param response A response (\code{list}) as returned by \code{\link{request}}.
#'
#' @return A logical value.
#'
#' @export
is_response_successful <- function(response)
  identical(response$response_code, 200L)

#' Request a GWAS REST API resource
#'
#' Performs a GET request on the specified \code{resource_url}.
#'
#' @param resource_url Endpoint URL. The endpoint is internally appended to the
#'   \code{base_url}.
#' @param base_url The GWAS REST API base URL (one should not need to change its
#'   default value).
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#'
#' @return A list of two elements: (i) response code, and (ii) the parsed JSON
#'   content.
#'
#' @export
request <- function(resource_url = "/", base_url = gwas_rest_api_base_url, verbose = FALSE, warnings = TRUE) {

  if(verbose) message("Base URL: ", base_url, ".")
  url <- stringr::str_c(base_url, resource_url)
  if(verbose) message("Requesting resource: ", url, ".")

  if(verbose) message("Using the user agent: ", user_agent_id$options$useragent)
  response <- httr::GET(url, user_agent_id)

  response_code <- httr::status_code(response)
  if(verbose) message("Response code: ", response_code, ".")

  # If response is not 200, i.e. if not completed successfully then
  # abort and report reason.
  if (!identical(response_code, 200L)) {
    error_response(url, response, warnings = warnings)
    return(list(response_code = response_code, content = NULL))
  }
  # Else, all fine and proceeding with parsing of the response content.

  # Check if the content type of the response is JSON.
  content_type <- httr::http_type(response)
  if(verbose) message("Response content type is \"", content_type, "\".")
  if (!identical(content_type, "application/json")) {
    stop("Response did not return JSON", call. = FALSE)
  }

  # Parse JSON content
  content <- jsonlite::fromJSON(httr::content(response, "text"),
                                flatten = TRUE)

  # Assuming NULL means not available (NA)
  content <- null_to_na(content)

  return(list(response_code = response_code, content = content))
}

error_response <- function(url, response, warnings = FALSE) {
  if(warnings)
    warning("The request for ",
            url,
            " did not completed successfully.",
            call. = FALSE)
}

get_pagination <- function(resource_url,
                           base_url = gwas_rest_api_base_url,
                           verbose = FALSE,
                           size = 1) {

  resource_url2 <- sprintf("%s?size=%d", resource_url, size)
  content <- request(resource_url2, base_url, verbose)

  pagination <- unlist(content$page)
  return(pagination)
}

get_resource <- function(resource_url = "/",
                         base_url = gwas_rest_api_base_url,
                         verbose = FALSE,
                         size = 20,
                         progress_bar = TRUE) {

  if(size < 1 || size > 500)
    stop("size must be an integer between 1..500.")

  # We use size = 1 for a quick answer because we're just
  # interested in the number of elements at this stage.
  pagination <- get_pagination(resource_url, base_url, verbose, size = 1)

  # Now we calculate the number of pages needed to cover the number
  # of elements in chunks of length = size.
  n_pages <- (pagination["totalElements"] - 1L) %/% size + 1L

  # Generate page indices, it starts at 0 and goes up to totalPages - 1.
  page_indices <- seq.int(0, n_pages - 1)

  # Generate each individual resource URL one per page
  resource_url_by_page <- sprintf("%s?page=%d&size=%d", resource_url, page_indices, size)

  content_by_page <- list()
  if(progress_bar)
    pb = utils::txtProgressBar(min = page_indices[1],
                        max = page_indices[length(page_indices)],
                        initial = page_indices[1],
                        width = 50, style = 3)

  for(i in page_indices) {
    if(progress_bar)
      utils::setTxtProgressBar(pb,i)

    content_by_page[[i + 1]] <- request(resource_url_by_page[i + 1],
                                        base_url = gwas_rest_api_base_url,
                                        verbose = verbose)
  }

  return(invisible(content_by_page))

}
