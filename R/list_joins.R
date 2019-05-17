#
# Inspired by dplyr's joins: https://r4ds.had.co.nz/relational-data.html#inner-join
# Here's some (more or less equivalent) list joins
#


#' Joins for lists.
#'
#' These functions join lists by matching elements by name. In the case of
#' \code{lst_left_join} \code{lst_x} is used as reference for traversal and its
#' names are looked for in \code{lst_y} for joining. In the case of
#' \code{lst_left_join}, \code{lst_y} is used as reference.
#' \code{lst_inner_join} only uses names common to both \code{lst_x} and
#' \code{lst_y} for combining elements.
#'
#' The functions \code{plst_left_join}, \code{plst_right_join} and
#' \code{plst_inner_join} are parallel versions that allow joining more than two
#' lists easily, i.e., just pass a list of lists to be joined.
#'
#' @param lst_x,lst_y lists.
#' @param list_of_lsts A list of lists to be joined together.
#'
#' @return A list.
#' @name lstjoin
NULL

#' @rdname lstjoin
#' @keywords internal
lst_left_join <- function(lst_x, lst_y) {
  if(is.atomic(lst_x) || is.null(names(lst_x))){
    c(lst_x, lst_y)
  } else {
    purrr::imap(lst_x, ~lst_left_join(lst_x[[.y]], lst_y[[.y]]))
  }
}

#' @rdname lstjoin
#' @keywords internal
lst_right_join <- function(lst_x, lst_y) {
  if(is.atomic(lst_y) || is.null(names(lst_y))){
    c(lst_x, lst_y)
  } else {
    purrr::imap(lst_y, ~lst_right_join(lst_x[[.y]], lst_y[[.y]]))
  }
}

#' @rdname lstjoin
#' @keywords internal
lst_inner_join <- function(lst_x, lst_y) {
  if( ( is.atomic(lst_x) || is.null(names(lst_x)) ) && ( is.atomic(lst_y) || is.null(names(lst_y)) ) ){
    c(lst_x, lst_y)
  } else {
    common_names <- intersect(names(lst_x), names(lst_y))

    if(!rlang::is_empty(common_names))
      names(common_names) <- common_names # so that map preserves names

    purrr::map(common_names, ~lst_inner_join(lst_x[[.x]], lst_y[[.x]]))
  }
}

#' @rdname lstjoin
#' @keywords internal
plst_left_join <- function(list_of_lsts) purrr::reduce(list_of_lsts, lst_left_join)

#' @rdname lstjoin
#' @keywords internal
plst_right_join <- function(list_of_lsts) purrr::reduce(list_of_lsts, lst_right_join)

#' @rdname lstjoin
#' @keywords internal
plst_inner_join <- function(list_of_lsts) purrr::reduce(list_of_lsts, lst_inner_join)
