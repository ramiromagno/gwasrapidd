#' @keywords internal
equal_length <- function(lst_of_vectors) {
  len <- purrr::map_int(lst_of_vectors, length)
  max_len <- max(len)
  lst_of_vectors2 <- purrr::map(lst_of_vectors, `length<-`, max_len)
  return(lst_of_vectors2)
}
