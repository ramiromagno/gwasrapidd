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

  if (rlang::is_empty(lst_of_vectors))
    return(lst_of_vectors)

  if (!rlang::is_list(lst_of_vectors))
    stop('lst_of_vectors must be a list!')

  if (!all(purrr::map_lgl(lst_of_vectors, rlang::is_atomic)))
    stop('One or more elements of lst_of_vectors is not an atomic vector.')

  len <- purrr::map_int(lst_of_vectors, length)
  max_len <- max(len)
  lst_of_vectors2 <- purrr::map(lst_of_vectors, `length<-`, max_len)
  return(lst_of_vectors2)
}

#' Converts an empty vector to a scalar NA
#'
#' This function converts an empty vector to a scalar NA of the same type as the
#' input.
#'
#' @param x An atomic vector, of one of these types: \code{character},
#'   \code{integer}, \code{double} or \code{logical}.
#'
#' @return An atomic vector of the same type as \code{x}. If \code{x} is empty
#'   then \code{NA} is returned, otherwise \code{x} is returned as is.
#'
#' @keywords internal
empty_to_na <- function(x) {

  if (!rlang::is_atomic(x))
    stop('x must be an atomic vector')

  if (rlang::is_empty(x)) {

    if (lubridate::is.POSIXct(x))
      return(lubridate::ymd_hms(NA_real_))

    return(
      switch(
        typeof(x),
        character = rlang::na_chr,
        integer = rlang::na_int,
        double = rlang::na_dbl,
        logical = rlang::na_lgl,
        rlang::na_chr # default
      )
    )
  } else {
    return(x)
  }
}

