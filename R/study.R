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
#'   c("GCST123456", "GCST000042", "123456")) # TRUE TRUE FALSE
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

#' Convert a publication list object to a tibble.
#'
#' Convert a publication list object to a tibble.
#'
#' @param publication_content The publication content (list).
#'
#' @return A \code{\link[tibble]{tibble}} version of \code{publication_content}.
#'
#' @keywords internal
publication_to_tibble <- function(publication_content) {

  pub_content <- publication_content

  pub_content[["author_fullname"]] <- publication_content$author$fullname
  pub_content[["author_orcid"]] <- publication_content$author$orcid
  pub_content$author <- NULL

  tibble::as_tibble(pub_content)
}


#' Convert a study list object to a tibble.
#'
#' Convert a study list object to a tibble.
#'
#' @param study_content The study content (list).
#'
#' @return A \code{\link[tibble]{tibble}} version of \code{study_content}.
#'
#' @keywords internal
study_content_to_tibble <- function(study_content) {

  study <- study_content

  study[["platforms_manufacturer"]] <- study$platforms$manufacturer
  study$platforms <- NULL
  study$ancestries <- list(study$ancestries)
  #study$ancestries <- NULL
  study[["diseaseTrait_trait"]] <- study$diseaseTrait$trait
  study$diseaseTrait <- NULL

  study$genotypingTechnologies <- study$genotypingTechnologies$genotypingTechnology
  study$publicationInfo <- list(publication_to_tibble(drop_links(study$publicationInfo)))
  study$`_links` <- NULL

  study_df <- tibble::as_tibble(study)

  return(study_df)

}



#' Get association study by accession ID
#'
#' Get association studies in the GWAS Catalog by their
#' accession ID.
#'
#' @param study_ids study IDs as a character vector, e.g., "GCST000854".
#' @param verbose Whether to be chatty or not.
#' @param warnings Whether to print warnings or not.
#' @param remove_duplicated_studies Whether to remove duplicated studies.
#'
#' @return A \code{\link[tibble]{tibble}} where rows are studies and columns are
#'   the following variables:
#'   \itemize{
#'   \item \code{accessionId}, GWAS Catalog study accession ID;
#'   \item \code{diseaseTrait_trait}, Free text description of the trait investigated in this study;
#'   \item \code{initialSampleSize}, 	Initial sample description;
#'   \item \code{replicationSampleSize}, Replication sample description;
#'   \item \code{gxe}, Whether the study investigates a gene-environment interaction;
#'   \item \code{gxg}, Whether the study investigates a gene-gene interaction;
#'   \item \code{snpCount}, Number of SNPs passing QC;
#'   \item \code{qualifier}, Qualifier of number of SNPs passing QC (e.g. >);
#'   \item \code{imputed}, Whether SNPs were imputed;
#'   \item \code{pooled}, Whether samples were pooled;
#'   \item \code{studyDesignComment}, Any other relevant study design information;
#'   \item \code{fullPvalueSet}, Whether full summary statistics are available for this study;
#'   \item \code{userRequested}, Whether the addition of this study to the Catalog was requested by a user;
#'   \item \code{ancestries}, Ancestry entries for this study. A list of
#'   \code{\link[tibble]{tibble}}s whose columns are:
#'   \itemize{
#'   \item \code{type}, TODO;
#'   \item \code{numberOfIndividuals}, TODO;
#'   \item \code{ancestralGroups}, TODO;
#'   \item \code{countryOfOrigin}, TODO;
#'   \item \code{countryOfRecruitment}, TODO;
#'   }
#'   \item \code{genotypingTechnologies}, Genotyping technology used in this study;
#'   \item \code{publicationInfo}, Convenience representation of the publication.
#'   A list of #'   \code{\link[tibble]{tibble}}s whose columns are:
#'   \itemize{
#'   \item \code{pubmedId}, PubMed ID;
#'   \item \code{publicationDate}, Publication date;
#'   \item \code{publication}, Journal;
#'   \item \code{title}, Title of the association study;
#'   \item \code{author_fullname}, First author name;
#'   }
#'   \item \code{platforms_manufacturer}, Genotyping platform(s) used in this study;
#' }
#' @examples
#' \donttest{get_studies_by_id(c("GCST000854", "GCST005268"))}
#'
#' @export
get_studies_by_id <- function(study_ids, verbose = FALSE, warnings = TRUE, remove_duplicated_studies = FALSE) {

  valid_study_ids <- is_study_accession(study_ids, convert_NA_to_FALSE = TRUE)
  if(!all(valid_study_ids))
    stop("The following are not valid study accession IDs: ",
         concatenate::cc_and(add_quotes(unique(study_ids[!valid_study_ids])), oxford = TRUE), ".")

  endpoint <- "/studies"

  # Prepend the endpoint to assemble the resource URLs
  resource_urls <- sprintf("%s/%s", endpoint, study_ids)

  # Request all studies by ID
  responses <- purrr::map(resource_urls, request, verbose = verbose, warnings = warnings)

  is_valid_response <-
    purrr::map_lgl(responses, is_response_successful) & !purrr::map_lgl(responses, is_content_empty)

  studies <- purrr::map_dfr(
    .x = responses[is_valid_response],
    .f = function(x) { study_content_to_tibble(x$content) })

  if(identical(nrow(studies), 0L) && warnings) {
    warning("No studies found.")
    return(tibble::tibble(
      "accessionId" = character(),
      "diseaseTrait_trait" = character(),
      "initialSampleSize" = character(),
      "replicationSampleSize" = character(),
      "gxe" = logical(),
      "gxg" = logical(),
      "snpCount" = integer(),
      "qualifier" = character(),
      "imputed" = logical(),
      "pooled" = logical(),
      "studyDesignComment" = character(),
      "fullPvalueSet" = logical(),
      "userRequested" = logical(),
      "ancestries" = list(),
      "genotypingTechnologies" = character(),
      "publicationInfo" = list(),
      "platforms_manufacturer" = character()
    ))
  }
  #If remove_duplicated_studies = TRUE, we remove those duplicated entries (rows).
  if(remove_duplicated_studies) {
    accessionId <- NULL # To appease R CMD check (not happy with this.)
    studies <- dplyr::distinct(studies, accessionId, .keep_all = TRUE)
  }

  # re-order columns
  studies <- dplyr::select(
    studies,
    "accessionId",
    "diseaseTrait_trait",
    "initialSampleSize",
    "replicationSampleSize",
    "gxe",
    "gxg",
    "snpCount",
    "qualifier",
    "imputed",
    "pooled",
    "studyDesignComment",
    "fullPvalueSet",
    "userRequested",
    "ancestries",
    "genotypingTechnologies",
    "publicationInfo",
    "platforms_manufacturer"
  )

  return(studies)
}
