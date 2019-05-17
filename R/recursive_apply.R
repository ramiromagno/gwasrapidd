#' Simple rapply version that deals with NULL values.
#'
#' Like \code{\link[base]{rapply}}, \code{recursive_apply} is a recursive
#' version \code{\link[base]{lapply}} but contrary to
#' \code{\link[base]{rapply}}, \code{recursive_apply} does not ignore
#' \code{\link[base]{NULL}} values. Each element of the list which is not itself
#' a list is replaced by the result of applying \code{fn}. If down the line
#' there are data.frames, then their class is preserved.
#'
#' @param x A list (of potentially many nested lists).
#' @param fn A function of a single argument.
#'
#' @return A list whose non-list elements have been replaced by the result of
#'   applying \code{fn}.
#'
#' @keywords internal
recursive_apply <- function(x, fn)
{
  # If x is a list, return a list.
  if (is.list(x))
    return(purrr::map(x, recursive_apply, fn))

  # If x is something else, return fn applied to x.
  return(fn(x))
}
