#' Grows vectors to match longest vector
#'
#' This function determines the longest vector and pads the shorter ones by
#' adding NAs until they match the longest vector.
#'
#' @param lst_of_vectors A list of atomic vectors.
#'
#' @return A list of atomic vectors of the same length.
#'
#' @keywords internal
equal_length <- function(lst_of_vectors) {

  if(rlang::is_empty(lst_of_vectors))
    return(lst_of_vectors)

  if(!rlang::is_list(lst_of_vectors))
    stop('lst_of_vectors must be a list!')

  if(!all(purrr::map_lgl(lst_of_vectors, rlang::is_atomic)))
    stop('One or more elements of lst_of_vectors is not an atomic vector.')

  len <- purrr::map_int(lst_of_vectors, length)
  max_len <- max(len)
  lst_of_vectors2 <- purrr::map(lst_of_vectors, `length<-`, max_len)
  return(lst_of_vectors2)
}
