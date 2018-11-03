quote <- function(str) sprintf("\"%s\"", str)


#' Does a string start with \code{_links.*}?
#'
#' Find which strings match the the following regular expression:
#' \code{^_links.*$}.
#'
#' @param str A character vector of strings.
#' @param convert_NA_to_FALSE Whether to treat \code{NA} as \code{NA}
#'   (\code{convert_NA_to_FALSE = FALSE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = TRUE}).
#' @return A logical vector.
#' @examples
#' \donttest{# Is "_links"?
#' is_links("_links") # TRUE
#'
#' # Test a bunch of strings:
#' is_links(
#'   c("_links", "links", "A_links", "_linksB")) # TRUE FALSE FALSE TRUE
#'
#' # By default NAs are returned as they are.
#' is_links(
#'   c("_links", "links", "A_links", "_linksB", NA_character_)) # TRUE FALSE FALSE TRUE NA
#'
#' # Use the argument convert_NA_to_FALSE = TRUE to get FALSE instead of NA.
#' is_links(
#'   c("_links", "links", "A_links", "_linksB", NA_character_),
#'   convert_NA_to_FALSE = TRUE) # TRUE FALSE FALSE TRUE FALSE
#' }
#' @keywords internal
is_links <- function(str, convert_NA_to_FALSE = FALSE) {
  if (!is.character(str))
    stop("str argument must be a character vector.")

  if (identical(length(str), 0L))
    stop("str contains no values, it must contain at least one string.")

  # Replace NA with "".
  if (convert_NA_to_FALSE) {
    str2 <- str
    str2[is.na(str)] <- ""
  } else {
    str2 <- str
  }

  is_links <- stringr::str_detect(str2, "^_links.*$")

  return(is_links)
}

#' Discard elements whose name starts with "_links".
#'
#' Takes a list and discards any elements whose name starts with
#' \code{"_links"}. \code{drop_links} can be applied on column names of
#' dataframes because dataframes are also lists after all.
#'
#' @param lst A list (remember that a dataframe is also a list).
#'
#' @return A list (dataframe) with potentially less elements (columns if dataframe).
#'
#' @export
drop_links <- function(lst) {

  if(!identical(typeof(lst), "list"))
    stop("lst must be of type list.")

  # If list is empty, do nothing and return as is.
  if(length(lst) < 1L) {
    warning("Input list is empty. Nothing to do. Returning the input list as is.")
    return(lst)
  }

  # If list has no named elements, return as is.
  if(is.null(names(lst))) {
     warning("Input list has no names. Nothing to do. Returning the input list as is.")
     return(lst)
  }

  # Which elements to keep (logical)
  keep <- !is_links(names(lst))

  if(all(!keep))
    warning("Returned list has no elements left.")

  # Discard and return list
  return(lst[keep])
}

#' Simple rapply version that deals with NULL values.
#'
#' Like \code{\link[base]{rapply}}, \code{recursive_apply} is a recursive
#' version \code{\link[base]{lapply}} but contrary to
#' \code{\link[base]{rapply}}, \code{recursive_apply} does not ignore
#' \code{\link[base]{NULL}} values. Each element of the list which is not itself
#' a list is replaced by the result of applying \code{fn}. If down the line there
#' are data.frames, then their class is preserved.
#'
#' @param x A list (of potentially many nested lists).
#' @param fn A function of a single argument.
#'
#' @return A list whose non-list elements have been replaced by the result of
#' applying \code{fn}.
#'
#' @keywords internal
recursive_apply <- function(x, fn)
{
  # If x is a list, return a list.
  if (is.list(x) && !is.data.frame(x))
    return(purrr::map(x, recursive_apply, fn))

  # If x is a data.frame, return a data.frame.
  if (is.list(x) && is.data.frame(x))
    return(purrr::map_dfr(x, recursive_apply, fn))

  # If x is something else, return fn applied to x.
  return(fn(x))
}

#' Recursively map all NULL values to NA.
#'
#' Traverse a recursive list and map \code{\link[base]{NULL}} values to
#' \code{\link[base]{NA}}.
#'
#' @param lst A list.
#'
#' @return A list whose NULL elements have been replaced by NA.
#'
#' @keywords internal
null_to_na <- function(lst)
  recursive_apply(lst, function(x) if(is.null(x)) NA_character_ else x)
