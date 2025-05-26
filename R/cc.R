cc <- function(...) {
  s <- unlist(list(...))
  s <- trimws(s)
  paste(s, sep = ", ", collapse = ", ")
}

cc_and <- function(..., oxford = FALSE) {
  x = unlist(list(...))
  res <- cc(x[-length(x)])
  comma <- ifelse(isTRUE(oxford) && length(x) > 2, ",", "")
  and <- ifelse(length(x) > 1L, " and ", "")
  paste0(res, comma, and, x[length(x)])
}
