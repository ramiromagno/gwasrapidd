#' Recursively map all missing values to \code{NA}.
#'
#' Traverse a recursive list and map missing values to \code{\link[base]{NA}}.
#' Missing values are: \itemize{ \item \code{NULL} \item \code{"NR"} }
#'
#' Note: This function is case sensitive on \code{"NR"}, i.e., it will leave
#' \code{"nr"} untouched.
#'
#' @param lst A list.
#'
#' @return A list whose missing values have been replaced by NA.
#'
#' @keywords internal
missing_to_na <- function(lst, na = NA_character_) {
  recursive_apply(lst, function(x) {
    if (is.null(x) || purrr::is_empty(x)) { # This is_empty condition does not seems to be working!!
      return(na)
    }

    if (is.character(x) && is.character(na)) {
      y <- x
      y[y == "NR" | y == "NA" | is.na(y) | is_empty_str(y)] <- na
      return(y)
    }

    if (is.character(x) && is.integer(na)) {
      y <- x
      y[y == "NR" | y == "NA" | is.na(y) | is_empty_str(y)] <- na
      return(as.integer(y))
    }

    if (is.character(x) && is.double(na)) {
      y <- x
      y[y == "NR" | y == "NA" | is.na(y) | is_empty_str(y)] <- na
      return(as.double(y))
    }

    if (is.character(x) && is.logical(na)) {
      y <- x
      y[y == "NR" | y == "NA" | is.na(y) | is_empty_str(y)] <- na
      return(as.logical(y))
    }


    if (is.logical(x)) {
      y <- x
      y[is.na(y)] <- na
      return(y)
    }
    return(x)
  })
}

#' @keywords internal
recode_to_chr_na <- function(chr_vec,
                             from = c('nr', 'NR', 'NA', 'na'),
                             recode_empty_string = TRUE) {

  if(!rlang::is_character(from))
    stop('from is expected to be a character!')

  if(rlang::is_character(chr_vec) && rlang::is_empty(chr_vec))
    return(chr_vec)

  n <- length(from)
  if(rlang::is_character(chr_vec)) {
    chr_vec2 <- plyr::mapvalues(chr_vec, from = from, to = rep(rlang::na_chr, n), warn_missing = FALSE)
    if(recode_empty_string)
      chr_vec2[is_empty_str(chr_vec2)] <- rlang::na_chr
    return(chr_vec2)
  } else {
    stop('chr_vec is expected to be a character!')
  }
}

#' @keywords internal
recode_missing <- function(x,
                           type = 'chr',
                           from = c('nr', 'NR', 'NA', 'na'),
                           recode_empty_string = TRUE) {

  if(!(rlang::is_null(x) ||
       rlang::is_character(x) ||
       rlang::is_logical(x) ||
       rlang::is_integer(x) ||
       rlang::is_double(x)))
    stop('x must be NULL, character, logical, integer or double.')

  if(!(type %in% c('chr', 'int', 'dbl', 'lgl')))
    stop('type must be of one of these types: chr, dbl, int or lgl.')

  if(!(rlang::is_character(from)))
    stop('from must be character.')

  s <- suppressWarnings # to suppress warnings coming from coercion

  # If x is NULL return NA but of the asked type.
  if(rlang::is_null(x) && identical(type, 'chr')) return(rlang::na_chr)
  if(rlang::is_null(x) && identical(type, 'dbl')) return(rlang::na_dbl)
  if(rlang::is_null(x) && identical(type, 'int')) return(rlang::na_int)
  if(rlang::is_null(x) && identical(type, 'lgl')) return(rlang::na_lgl)

  # If x is character, then recode first as character and then coerce.
  if(rlang::is_character(x) && identical(type, 'chr'))
    return(recode_to_chr_na(x, from = from, recode_empty_string = recode_empty_string))
  if(rlang::is_character(x) && identical(type, 'dbl'))
    return(s(as.numeric(recode_to_chr_na(x, from = from, recode_empty_string = recode_empty_string))))
  if(rlang::is_character(x) && identical(type, 'int'))
    return(s(as.integer(recode_to_chr_na(x, from = from, recode_empty_string = recode_empty_string))))
  if(rlang::is_character(x) && identical(type, 'lgl'))
    return(s(as.logical(recode_to_chr_na(x, from = from, recode_empty_string = recode_empty_string))))

  # If x is logical, no recoding to be done, so just coerce to the asked type.
  if(rlang::is_logical(x) && identical(type, 'chr')) return(as.character(x))
  if(rlang::is_logical(x) && identical(type, 'dbl')) return(as.numeric(x))
  if(rlang::is_logical(x) && identical(type, 'int')) return(as.integer(x))
  if(rlang::is_logical(x) && identical(type, 'lgl')) return(x)

  # If x is integer, no recoding to be done, so just coerce to the asked type.
  if(rlang::is_integer(x) && identical(type, 'chr')) return(as.character(x))
  if(rlang::is_integer(x) && identical(type, 'dbl')) return(as.numeric(x))
  if(rlang::is_integer(x) && identical(type, 'int')) return(x)
  if(rlang::is_integer(x) && identical(type, 'lgl')) return(s(as.logical(x)))

  # If x is double, no recoding to be done, so just coerce to the asked type.
  if(rlang::is_double(x) && identical(type, 'chr')) return(as.character(x))
  if(rlang::is_double(x) && identical(type, 'dbl')) return(x)
  if(rlang::is_double(x) && identical(type, 'int')) return(s(as.integer(x)))
  if(rlang::is_double(x) && identical(type, 'lgl')) return(s(as.logical(x)))
}
