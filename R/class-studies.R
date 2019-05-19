setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of GWAS Catalog studies
#'
#' The studies object consists of eight slots, each a table
#' (\code{\link[tibble]{tibble}}), that combined form a relational database of a
#' subset of GWAS Catalog studies. Each study is an observation (row) in the
#' \code{studies} table --- main table. All tables have the column
#' \code{study_id} as primary key.
#'
#' @slot studies
#' \describe{
#' \item{study_id}{GWAS Catalog study accession identifier, e.g.,
#' \code{"GCST002735"}.}
#' \item{reported_trait}{Phenotypic trait as reported by the authors of the
#' study, e.g. \code{"Breast cancer"}.}
#' \item{initial_sample_size}{Free text description of the initial cohort sample
#' size.}
#' \item{replication_sample_size}{Free text description of the replication
#' cohort sample size.}
#' \item{gxe}{Whether the study investigates a gene-environment interaction.}
#' \item{gxg}{Whether the study investigates a gene-gene interaction.}
#' \item{snp_count}{Number of variants passing quality control.}
#' \item{qualifier}{Qualifier of number of variants passing quality control.}
#' \item{imputed}{Whether variants were imputed.}
#' \item{pooled}{Whether samples were pooled.}
#' \item{study_design_comment}{Any other relevant study design information.}
#' \item{full_pvalue_set}{Whether full summary statistics are available for this
#' study.}
#' \item{user_requested}{Whether the addition of this study to the GWAS Catalog
#' was requested by a user.}
#' }
#' @slot genotyping_techs A \code{\link[tibble]{tibble}} listing genotyping
#'   technologies employed in each study. Columns:
#' \describe{
#' \item{study_id}{GWAS Catalog study accession identifier.}
#' \item{genotyping_technology}{Genotyping technology employed, e.g.
#' \code{"Exome genotyping array"}, \code{"Exome-wide sequencing"},
#' \code{"Genome-wide genotyping array"}, \code{"Genome-wide sequencing"}, or
#' \code{"Targeted genotyping array"}.}
#' }
#' @slot platforms A \code{\link[tibble]{tibble}} listing platforms used per
#'   study.
#' \describe{
#' \item{study_id}{GWAS Catalog study accession identifier.}
#' \item{manufacturer}{Platform manufacturer, e.g., \code{"Affymetrix"},
#' \code{"Illumina"}, or \code{"Perlegen"}.}
#' }
#' @slot ancestries A \code{\link[tibble]{tibble}} listing ancestry of samples
#'   used in each study.
#' \describe{
#' \item{study_id}{GWAS Catalog study accession identifier.}
#' \item{ancestry_id}{Ancestry identifier.}
#' \item{type}{Stage of the ancestry sample: either \code{'initial'} or
#' \code{'replication'}.}
#' \item{number_of_individuals}{Number of individuals comprising this ancestry
#' sample.}
#' }
#' @slot ancestral_groups A \code{\link[tibble]{tibble}} listing ancestral
#'   groups used in each ancestry.
#' \describe{
#' \item{study_id}{GWAS Catalog study accession identifier.}
#' \item{ancestry_id}{Ancestry identifier.}
#' \item{ancestral_group}{Genetic ancestry groups present in the sample.}
#' }
#' @slot countries_of_origin A \code{\link[tibble]{tibble}} listing countries of
#'   origin of samples.
#' \describe{
#' \item{study_id}{GWAS Catalog study accession identifier.}
#' \item{ancestry_id}{Ancestry identifier.}
#' \item{country_name}{Country name, according to
#' \href{https://unstats.un.org/unsd/methodology/m49/overview/}{The United
#' Nations M49 Standard of Geographic Regions.}}
#' \item{major_area}{Region name, according to
#' \href{https://unstats.un.org/unsd/methodology/m49/overview/}{The United
#' Nations M49 Standard of Geographic Regions.}} \item{region}{Sub-region name,
#' according to \href{https://unstats.un.org/unsd/methodology/m49/overview/}{The
#' United Nations M49 Standard of Geographic Regions.}}
#' }
#' @slot countries_of_recruitment A \code{\link[tibble]{tibble}} listing
#'   countries of recruitment of samples.
#' \describe{
#' \item{study_id}{GWAS Catalog study accession identifier.}
#' \item{ancestry_id}{Ancestry identifier.}
#' \item{country_name}{Country name, according to
#' \href{https://unstats.un.org/unsd/methodology/m49/overview/}{The United
#' Nations M49 Standard of Geographic Regions.}}
#' \item{major_area}{Region name, according to
#' \href{https://unstats.un.org/unsd/methodology/m49/overview/}{The United
#' Nations M49 Standard of Geographic Regions.}} \item{region}{Sub-region name,
#' according to \href{https://unstats.un.org/unsd/methodology/m49/overview/}{The
#' United Nations M49 Standard of Geographic Regions.}}
#' }
#' @slot publications A \code{\link[tibble]{tibble}} listing publications
#'   associated with each study.
#' \describe{
#' \item{study_id}{GWAS Catalog study accession identifier.}
#' \item{pubmed_id}{\href{https://en.wikipedia.org/wiki/PubMed}{PubMed}
#' identifier.}
#' \item{publication_date}{Publication date (online date if available) formatted
#' as \code{\link[lubridate]{ymd}}.}
#' \item{publication}{Abbreviated journal name.}
#' \item{title}{Publication title.}
#' \item{author_fullname}{Last name and initials of first author.}
#' \item{author_orcid}{Author's \href{https://en.wikipedia.org/wiki/ORCID}{ORCID
#' iD} (Open Researcher and Contributor ID).}
#' }
#' @export
setClass(
  "studies",
  slots = c(
    studies = "tbl_df",
    genotyping_techs = "tbl_df",
    platforms = "tbl_df",
    ancestries = "tbl_df",
    ancestral_groups = "tbl_df",
    countries_of_origin = "tbl_df",
    countries_of_recruitment = "tbl_df",
    publications = "tbl_df"
  )
)

#' Constructor for the S4 studies object.
#'
#' Constructor for the S4 \linkS4class{studies} object.
#'
#' @param studies A \code{\link{studies_tbl}} tibble.
#' @param genotyping_techs A \code{\link{genotyping_techs_tbl}} tibble.
#' @param platforms A \code{\link{platforms_tbl}} tibble.
#' @param ancestries A \code{\link{ancestries_tbl}} tibble.
#' @param ancestral_groups A \code{\link{ancestral_groups_tbl}} tibble.
#' @param countries_of_origin A \code{\link{countries_tbl}} tibble.
#' @param countries_of_recruitment A \code{\link{countries_tbl}} tibble.
#' @param publications A \code{\link{publications_tbl}} tibble.
#'
#' @return An object of class \linkS4class{studies}.
#' @keywords internal
studies <- function(studies = studies_tbl(),
                    genotyping_techs = genotyping_techs_tbl(),
                    platforms = platforms_tbl(),
                    ancestries = ancestries_tbl(),
                    ancestral_groups = ancestral_groups_tbl(),
                    countries_of_origin = countries_tbl(),
                    countries_of_recruitment = countries_tbl(),
                    publications = publications_tbl()) {
  s4_studies <- methods::new("studies",
      studies = studies,
      genotyping_techs = genotyping_techs,
      platforms = platforms,
      ancestries = ancestries,
      ancestral_groups = ancestral_groups,
      countries_of_origin = countries_of_origin,
      countries_of_recruitment = countries_of_recruitment,
      publications = publications
  )

  # Drop rows in tibbles whose value of study_id == NA_character.
  studies_drop_na(s4_studies)
}

#' Creates a studies table.
#'
#' Creates a studies table.
#'
#' @param study_id GWAS Catalog study accession identifier.
#' @param reported_trait Phenotypic trait as reported by the authors of the
#'   study.
#' @param initial_sample_size Free text description of the initial cohort sample
#'   size.
#' @param replication_sample_size Free text description of the replication
#'   cohort sample size.
#' @param gxe Whether the study investigates a gene-environment interaction.
#' @param gxg Whether the study investigates a gene-gene interaction.
#' @param snp_count Number of variants passing quality control.
#' @param qualifier Qualifier of number of variants passing quality control.
#' @param imputed Whether variants were imputed.
#' @param pooled Whether samples were pooled.
#' @param study_design_comment Any other relevant study design information.
#' @param full_pvalue_set Whether full summary statistics are available for this
#'   study.
#' @param user_requested Whether the addition of this study to the GWAS Catalog
#'   was requested by a user.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
studies_tbl <- function(study_id = character(),
                        reported_trait = character(),
                        initial_sample_size = character(),
                        replication_sample_size = character(),
                        gxe = logical(),
                        gxg = logical(),
                        snp_count = integer(),
                        qualifier = character(),
                        imputed = logical(),
                        pooled = logical(),
                        study_design_comment = character(),
                        full_pvalue_set = logical(),
                        user_requested = logical()) {
  tbl <- tibble::tibble(
    study_id = study_id,
    reported_trait = reported_trait,
    initial_sample_size = initial_sample_size,
    replication_sample_size = replication_sample_size,
    gxe = gxe,
    gxg = gxg,
    snp_count = snp_count,
    qualifier = qualifier,
    imputed = imputed,
    pooled = pooled,
    study_design_comment = study_design_comment,
    full_pvalue_set = full_pvalue_set,
    user_requested = user_requested
  )

  return(tbl)
}

#' Creates a genotyping technologies table.
#'
#' Creates a genotyping technologies table.
#'
#' @param study_id GWAS Catalog study accession identifier.
#' @param genotyping_technology Genotyping technology.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
genotyping_techs_tbl <- function(study_id = character(),
                                 genotyping_technology = character()) {
  tbl <- tibble::tibble(study_id = study_id,
                        genotyping_technology = genotyping_technology)

  return(tbl)
}

#' Creates a platforms table.
#'
#' Creates a platforms table.
#'
#' @param study_id GWAS Catalog study accession identifier.
#' @param manufacturer Platform manufacturer.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
platforms_tbl <- function(study_id = character(),
                          manufacturer = character()) {
  tbl <- tibble::tibble(study_id = study_id,
                        manufacturer = manufacturer)

  return(tbl)
}

#' Creates an ancestries table.
#'
#' Creates an ancestries table.
#'
#' @param study_id GWAS Catalog study accession identifier.
#' @param ancestry_id Ancestry identifier.
#' @param type Type of cohort sample, either \code{"initial"} or
#'   \code{"replication"}.
#' @param number_of_individuals Number of individuals in the cohort sample.
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
ancestries_tbl <- function(study_id = character(),
                           ancestry_id = integer(),
                           type = character(),
                           number_of_individuals = integer()) {
  tbl <- tibble::tibble(
    study_id = study_id,
    ancestry_id = ancestry_id,
    type = type,
    number_of_individuals = number_of_individuals
  )
  return(tbl)
}

#' Creates an ancestral groups table.
#'
#' Creates a ancestral groups table.
#'
#' @param study_id GWAS Catalog study accession identifier.
#' @param ancestry_id Ancestry identifier.
#' @param ancestral_group Ancestral group.
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
ancestral_groups_tbl <- function(study_id = character(),
                                 ancestry_id = integer(),
                                 ancestral_group = character()) {
  tbl <- tibble::tibble(
    study_id = study_id,
    ancestry_id = ancestry_id,
    ancestral_group = ancestral_group
  )
  return(tbl)
}

#' Creates a countries table.
#'
#' Creates a countries table. This
#' function is used internally to create both the \code{countries_of_origin} and
#' \code{countries_of_recruitment} slots of a \linkS4class{studies} object.
#'
#' @param study_id GWAS Catalog study accession identifier.
#' @param ancestry_id Ancestry identifier.
#' @param country_name Country name, according to
#' \href{https://unstats.un.org/unsd/methodology/m49/overview/}{The United
#' Nations M49 Standard of Geographic Regions.}
#' @param major_area Region name, according to
#' \href{https://unstats.un.org/unsd/methodology/m49/overview/}{The United
#' Nations M49 Standard of Geographic Regions.}
#' @param region Sub-region name, according to
#' \href{https://unstats.un.org/unsd/methodology/m49/overview/}{The United
#' Nations M49 Standard of Geographic Regions.}
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
countries_tbl <- function(study_id = character(),
                          ancestry_id = integer(),
                          country_name = character(),
                          major_area = character(),
                          region = character()) {
  tbl <- tibble::tibble(
    study_id = study_id,
    ancestry_id = ancestry_id,
    country_name = country_name,
    major_area = major_area,
    region = region
  )
  return(tbl)
}

#' Creates a publications table.
#'
#' Creates a publications table.
#'
#' @param study_id GWAS Catalog study accession identifier.
#' @param pubmed_id \href{https://en.wikipedia.org/wiki/PubMed}{PubMed}
#'   identifier.
#' @param publication_date Publication date (online date if available) formatted
#'   as \code{\link[lubridate]{ymd}}.
#' @param publication Abbreviated journal name.
#' @param title Publication title.
#' @param author_fullname Last name and initials of first author.
#' @param author_orcid Author's \href{https://en.wikipedia.org/wiki/ORCID}{ORCID
#'   iD} (Open Researcher and Contributor ID).
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
publications_tbl <- function(study_id = character(),
                             pubmed_id = integer(),
                             publication_date = lubridate::ymd(),
                             publication = character(),
                             title = character(),
                             author_fullname = character(),
                             author_orcid = character()) {

  tbl <- tibble::tibble(
    study_id = study_id,
    pubmed_id = pubmed_id,
    publication_date = publication_date,
    publication = publication,
    title = title,
    author_fullname = author_fullname,
    author_orcid = author_orcid
  )
  return(tbl)
}

#' Drop any NA studies.
#'
#' This function takes a studies S4 object and removes any study identifiers
#' that might have been NA. This ensures that there is always a non-NA
#' \code{study_id} value in all tables. This is important as the \code{study_id}
#' is the primary key.
#'
#' @param s4_studies An object of class \linkS4class{studies}.
#'
#' @return An object of class \linkS4class{studies}.
#' @keywords internal
studies_drop_na <- function(s4_studies) {

  # Drop any study_id == NA_character_
  study_id <- rlang::expr(study_id)
  s4_studies@studies <- tidyr::drop_na(s4_studies@studies, !!study_id)

  # Extract non-NA study ids
  study_ids <- s4_studies@studies$study_id

  # Filter remaining tibbles with non-NA study ids to ensure that the primary
  # key (study_id) always exists and is not NA.
  s4_studies@genotyping_techs <- dplyr::filter(
    s4_studies@genotyping_techs, !!study_id %in% study_ids)

  s4_studies@platforms <- dplyr::filter(
    s4_studies@platforms, study_id %in% !!study_ids)

  s4_studies@ancestries <- dplyr::filter(
    s4_studies@ancestries, study_id %in% !!study_ids)

  s4_studies@ancestral_groups <- dplyr::filter(
    s4_studies@ancestral_groups, !!study_id %in% study_ids)

  s4_studies@countries_of_origin <- dplyr::filter(
    s4_studies@countries_of_origin, !!study_id %in% study_ids)

  s4_studies@countries_of_recruitment <- dplyr::filter(
    s4_studies@countries_of_recruitment, !!study_id %in% study_ids)

  s4_studies@publications <- dplyr::filter(
    s4_studies@publications, !!study_id %in% study_ids)

  return(s4_studies)
}
