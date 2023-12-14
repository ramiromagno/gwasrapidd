#' Export a GWAS Catalog object to xlsx
#'
#' This function exports a GWAS Catalog object to Microsoft Excel xlsx file.
#' Each table (slot) is saved in its own sheet.
#'
#' @param x A \linkS4class{studies}, \linkS4class{associations},
#'   \linkS4class{variants} or \linkS4class{traits} object.
#' @param file A file name to write to.
#'
#' @return Although this function is run for its side effect of writing an xlsx
#'   file, the path to the exported file is returned.
#'
#' @examples
#' # Initial setup
#' .old_wd <- setwd(tempdir())
#'
#' # Save an `associations` object, e.g. `associations_ex01`, to xlsx.
#' write_xlsx(associations_ex01, "associations.xlsx")
#'
#' # Cleanup
#' unlink("associations.xlsx")
#' setwd(.old_wd)
#'
#' @export
write_xlsx <- function(x, file = stop('`file` must be specified')) {
  lst <- s4_to_list(x)
  writexl::write_xlsx(lst, path = file)
}
