#' Generate a list to hold GWAS metadata
#'
#' Creates a list to hold GWAS Catalog metadata.
#'
#' @param ensembl_release_number
#'   \href{https://www.ensembl.org/info/docs/api/versions.html}{Ensembl release
#'   number}.
#' @param genome_build_version
#'   \href{https://www.ncbi.nlm.nih.gov/grc/human}{Genome build version}.
#' @param dbsnp_version
#'   \href{https://www.ncbi.nlm.nih.gov/projects/SNP/snp_summary.cgi}{dbSNP
#'   version}.
#' @param usage_start_date Date since this combination of resource versions has
#'   been in use.
#'
#' @return A list of 4 named elements as passed as arguments:
#'   \code{ensembl_release_number}, \code{genome_build_version},
#'   \code{dbsnp_version} and \code{usage_start_date}.
#' @keywords internal
metadata_lst <- function(
  ensembl_release_number = NA_integer_,
  genome_build_version = NA_character_,
  dbsnp_version = NA_integer_,
  usage_start_date = lubridate::ymd_hms()) {

  if(rlang::is_null(ensembl_release_number))
    ensembl_release_number <- NA_integer_

  if(rlang::is_null(genome_build_version))
    genome_build_version <- NA_character_

  if(rlang::is_null(dbsnp_version))
    dbsnp_version <- NA_integer_

  if(rlang::is_null(usage_start_date))
    usage_start_date <- lubridate::ymd_hms()

  if(!rlang::is_scalar_integer(ensembl_release_number))
    stop("ensembl_release_number must be a scalar integer.")

  if(!rlang::is_scalar_character(genome_build_version))
    stop("genome_build_version must be a scalar character.")

  if(!rlang::is_scalar_integer(dbsnp_version))
    stop("dbsnp_version must be a scalar integer.")

  if(!(lubridate::is.POSIXct(usage_start_date) && length(usage_start_date) < 2L))
    stop("usage_start_date must be a scalar POSIXct object.")

  lst <- list(
    ensembl_release_number = ensembl_release_number,
    genome_build_version = genome_build_version,
    dbsnp_version = dbsnp_version,
    usage_start_date = usage_start_date
  )

  return(lst)
}


#' Get GWAS Catalog metadata
#'
#' Provides a list of the resources the GWAS Catalog data is currently mapped
#' against: \href{https://www.ensembl.org/info/docs/api/versions.html}{Ensembl
#' release number}, \href{https://www.ncbi.nlm.nih.gov/grc/human}{Genome build
#' version} and
#' \href{https://www.ncbi.nlm.nih.gov/projects/SNP/snp_summary.cgi}{dbSNP
#' version}.In addition, the date since this combination of resource versions
#' has been in use is also returned.
#'
#' @param verbose Whether to be chatty.
#' @param warnings Whether to trigger a warning if the request is not successful.
#'
#' @return A named \code{\link[base]{list}} whose \code{\link[base]{names}} are:
#' \itemize{
#' \item \code{ensembl_release_number}: \href{https://www.ensembl.org/info/docs/api/versions.html}{Ensembl release number};
#' \item \code{genome_build_version}: \href{https://www.ncbi.nlm.nih.gov/grc/human}{Genome build version};
#' \item \code{dbsnp_version}: \href{https://www.ncbi.nlm.nih.gov/projects/SNP/snp_summary.cgi}{dbSNP version}.
#' \item \code{usage_start_date}: Date since this combination of resource versions has been in use.
#' }
#' @examples
#' get_metadata(warnings = FALSE)
#' @export
get_metadata <- function(verbose = FALSE, warnings = TRUE) {
  response <- gc_request("/metadata", verbose = verbose, warnings = warnings)

  if(!identical(response$status, "OK")) {
    return(metadata_lst())
  } else {
    metadata <- response$content$`_embedded`$mappingMetadatas
    return(
      metadata_lst(
        ensembl_release_number = metadata$ensemblReleaseNumber,
        genome_build_version = metadata$genomeBuildVersion,
        dbsnp_version = metadata$dbsnpVersion,
        usage_start_date = lubridate::ymd_hms(metadata$usageStartDate)
      )
    )
  }


}
