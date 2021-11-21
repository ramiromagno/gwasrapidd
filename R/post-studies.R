#' Browse PubMed from PubMed identifiers.
#'
#' This function launches the web browser and opens a tab for each PubMed
#' citation.
#'
#' @param pubmed_id A PubMed identifier, either a character or an integer
#'   vector.
#' @return Returns \code{TRUE} if successful. Note however that this
#' function is run for its side effect.
#' @examples
#' open_in_pubmed(c('26301688', '30595370'))
#'
#' @export
open_in_pubmed <- function(pubmed_id) {
  if (!(rlang::is_double(pubmed_id) ||
        rlang::is_integer(pubmed_id) ||
        rlang::is_character(pubmed_id) ))
    stop("pubmed_id must be a vector of numbers.")

  if (rlang::is_double(pubmed_id) || rlang::is_integer(pubmed_id))
    pubmed_id2 <- as.character(pubmed_id)
  else
    pubmed_id2 <- pubmed_id

  if (any(!is_pubmed_id(pubmed_id2)))
    stop("These are not valid PubMed IDs: ",
         concatenate::cc_and(pubmed_id[!is_pubmed_id(pubmed_id2)],
                             oxford = TRUE),
         ".")

  if (interactive()) {
  urls <-
    glue::glue("https://pubmed.ncbi.nlm.nih.gov/{pubmed_id2}")

  purrr::walk(urls, utils::browseURL)

  return(invisible(TRUE))
  } else {
    return(invisible(TRUE))
  }
}

