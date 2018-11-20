#' GWAS Catalog Metadata
#'
#' Provides a list of the resources the GWAS Catalog data is currently mapped
#' against, namely:
#' \itemize{
#' \item Ensembl release number;
#' \item Genome build version;
#' \item dbSNP version.
#' }
#'
#' In addition, the date since this combination of resource versions has been
#' in use is also returned.
#'
#' @param warnings A logical value indicating whether to trigger a warning if the request is not successful.
#'
#' @return A named character vector whose names are:
#'
#' @examples
#' \donttest{get_metadata()
#' # ensembl_release_number  genome_build_version  dbsnp_version                 usage_start_date
#' #                   "94"          "GRCh38.p12"          "151"   "2018-10-02T13:00:02.054+0000"
#' }
#'
#' @export
get_metadata <- function(warnings = TRUE) {
  response <- request("/metadata", warnings = warnings)

  # If the response was not successful for some reason
  # then return an empty vector.
  if (!is_response_successful(response) && warnings) {
    return(
      c(
        ensembl_release_number = NA_character_,
        genome_build_version = NA_character_,
        dbsnp_version = NA_character_,
        usage_start_date = NA_character_
      )
    )
  } else {
    metadata <- response$content$`_embedded`$mappingMetadatas
    return(
      c(
        ensembl_release_number = metadata$ensemblReleaseNumber,
        genome_build_version = metadata$genomeBuildVersion,
        dbsnp_version = metadata$dbsnpVersion,
        usage_start_date = metadata$usageStartDate
      )
    )
  }

}
