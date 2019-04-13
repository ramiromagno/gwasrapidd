setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of GWAS Catalog EFO traits.
#'
#' The traits object consists of one slot only, a table
#' (\code{\link[tibble]{tibble}}) of GWAS Catalog EFO traits. Each EFO trait is
#' an observation (row) in the traits table --- main table.
#'
#' @slot traits A \code{\link[tibble]{tibble}} listing EFO traits Columns:
#' \describe{
#' \item{efo_id}{\href{https://www.ebi.ac.uk/efo/}{EFO} identifier.}
#' \item{trait}{\href{https://www.ebi.ac.uk/efo/}{EFO} trait description.}
#' \item{uri}{The full URI of the \href{https://www.ebi.ac.uk/efo/}{EFO} term.}
#' }
#' @export
setClass(
  "traits",
  slots = c(
    traits = "tbl_df"
  )
)

#' Constructor for the S4 traits object.
#'
#' Constructor for the S4 \linkS4class{traits} object.
#'
#' @param efo_id A \code{character} vector of \href{https://www.ebi.ac.uk/efo/}{EFO} identifiers.
#' @param trait A \code{character} vector of \href{https://www.ebi.ac.uk/efo/}{EFO} trait descriptions.
#' @param uri A \code{character} vector of  \href{https://www.ebi.ac.uk/efo/}{EFO} URIs.
#'
#' @return An object of class \linkS4class{traits}.
#' @keywords internal
traits <- function(traits = traits_tbl()) {
  methods::new("traits", traits = traits)
}

#' @keywords internal
traits_tbl <- function(efo_id = character(),
                          trait = character(),
                          uri = character()) {
  tbl <- tibble::tibble(efo_id = efo_id,
                        trait = trait,
                        uri = uri)
  tbl2 <- dplyr::distinct(tbl)
  return(tbl2)
}
