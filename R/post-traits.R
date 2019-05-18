#' Get all child terms of this trait in the EFO hierarchy
#'
#' @param efo_id A \href{https://www.ebi.ac.uk/efo/}{EFO} identifier.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#' @param progress_bar Whether to show a progress bar as the paginated resources
#'   are retrieved.
#'
#' @return A character vector of EFO identifiers.
#' @keywords internal
child_efo_ids <- function(efo_id, verbose = FALSE, warnings = TRUE, page_size = 20L, progress_bar = TRUE) {

  resource_url <- sprintf("%s%s", "/descendants?id=", efo_id)

  obj <- gc_request_all(
    resource_url = resource_url,
    base_url = "https://www.ebi.ac.uk/ols/api/ontologies/efo",
    verbose = verbose,
    warnings = warnings,
    page_size = page_size,
    progress_bar = progress_bar
  )

  if(rlang::is_null(obj$content$`_embedded`$terms))
    return(character())
  else
    return(obj$content$`_embedded`$terms$short_form)

}

#' Get all child terms of this trait in the EFO hierarchy
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
#' @param progress_bar Whether to show a progress bar as the paginated resources
#'   are retrieved.
#'
#' @return A named list whose values are character vectors of EFO identifiers.
#' @examples
#' get_child_efo(c('EFO_0004884', 'EFO_0004343', 'EFO_0005299'))
#' @export
get_child_efo <- function(efo_id,
                          verbose = FALSE,
                          warnings = TRUE,
                          page_size = 20L,
                          progress_bar = TRUE) {

  efo_list <- purrr::map(efo_id, child_efo_ids,
                      verbose = verbose,
                      warnings = warnings,
                      page_size = page_size,
                      progress_bar = progress_bar)

  names(efo_list) <- efo_id
  return(efo_list)
}
