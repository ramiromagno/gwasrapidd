#' Is a string a GWAS Catalog study accession ID?
#'
#' Find which strings are valid GWAS Catalog study accession IDs (returns
#' \code{TRUE}). Study accession IDs are tested against the following regular
#' expression: \code{^GCST\\\\d\{6\}$}.
#'
#' @param str A character vector of strings.
#' @param convert_NA_to_FALSE Whether to treat \code{NA} as \code{NA}
#'   (\code{convert_NA_to_FALSE = FALSE}) or whether to return \code{FALSE} when
#'   an \code{NA} is found (\code{convert_NA_to_FALSE = TRUE}).
#' @return A logical vector.
#' @examples
#' # Can "GCST123456" be a valid GWAS Catalog study accession ID?
#' is_study_accession("GCST123456") # TRUE
#'
#' # Test a bunch of strings:
#' is_study_accession(
#'   c("GCST123456", "GCST000042", "123456")) # TRUE TRUE NA
#'
#' # By default NAs are returned as they are.
#' is_study_accession(
#'   c("123456", "GCST123456", NA_character_)) # FALSE TRUE NA
#'
#' # Use the argument convert_NA_to_FALSE = TRUE to get FALSE instead of NA.
#' is_study_accession(c("123456", "GCST123456", NA_character_),
#'                    convert_NA_to_FALSE = TRUE) # FALSE TRUE FALSE
#'
#' @export
is_study_accession <- function(str, convert_NA_to_FALSE = FALSE) {
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

  is_accession <- stringr::str_detect(str2, "^GCST\\d{6}$")

  return(is_accession)
}

