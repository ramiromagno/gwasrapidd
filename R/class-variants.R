setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of GWAS Catalog variants
#'
#' The variants object consists of four slots, each a table
#' (\code{\link[tibble]{tibble}}), that combined form a relational database of a
#' subset of GWAS Catalog variants Each variant is an observation (row) in the
#' variants table --- main table. All tables have the column \code{variant_id} as
#' primary key.
#'
#' @slot variants A \code{\link[tibble]{tibble}} listing variants. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' }
#' @slot genomic_contexts A \code{\link[tibble]{tibble}} listing genomic
#'   contexts associated with each variant. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' }
#' @slot ensembl_ids A \code{\link[tibble]{tibble}} listing gene Ensembl
#'   identifiers associated with each genomic context. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' }
#' @slot entrez_ids A \code{\link[tibble]{tibble}} listing gene Entrez
#'   identifiers associated with each genomic context. Columns:
#' \describe{
#' \item{TODO}{TODO}
#' }
#' @export
setClass(
  "variants",
  slots = c(
    variants = "tbl_df",
    genomic_contexts = "tbl_df",
    ensembl_ids = "tbl_df",
    entrez_ids = "tbl_df"
  )
)

#' Constructor for the S4 variants object.
#'
#' Constructor for the S4 \linkS4class{variants} object.
#'
#' @param variants TODO.
#' @param genomic_contexts TODO.
#' @param risk_alleles TODO.
#' @param ensembl_ids TODO.
#' @param entrez_ids TODO.
#'
#' @return An object of class \linkS4class{variants}.
#' @keywords internal
variants <- function(variants = variants_tbl(),
                     genomic_contexts = genomic_contexts_tbl(),
                     ensembl_ids = v_ensembl_ids_tbl(),
                     entrez_ids = v_entrez_ids_tbl()) {
  methods::new("variants",
      variants = variants,
      genomic_contexts = genomic_contexts,
      ensembl_ids = ensembl_ids,
      entrez_ids = entrez_ids
  )
}

#' Creates a variants table.
#'
#' Creates a variants table.
#'
#' @param variant_id TODO.
#' @param merged TODO.
#' @param functional_class TODO.
#' @param chromosome_name TODO.
#' @param chromosome_position TODO.
#' @param chromosome_region TODO.
#' @param last_update_date TODO.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
variants_tbl <- function(variant_id = character(),
                         merged = integer(),
                         functional_class = character(),
                         chromosome_name = character(),
                         chromosome_position = integer(),
                         chromosome_region = character(),
                         last_update_date = lubridate::ymd_hms()) {

  tbl <- tibble::tibble(
    variant_id = variant_id,
    merged = merged,
    functional_class = functional_class,
    chromosome_name = chromosome_name,
    chromosome_position = chromosome_position,
    chromosome_region = chromosome_region,
    last_update_date = last_update_date
  )

  return(tbl)
}

#' Creates a genomic contexts table.
#'
#' Creates a genomic contexts table.
#'
#' @param variant_id TODO.
#' @param gene_name TODO.
#' @param chromosome_name TODO.
#' @param distance TODO.
#' @param is_closest_gene TODO.
#' @param is_intergenic TODO.
#' @param is_upstream TODO.
#' @param is_downstream TODO.
#' @param source TODO.
#' @param mapping_method TODO.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
genomic_contexts_tbl <- function(variant_id = character(),
                                 gene_name = character(),
                                 chromosome_name = character(),
                                 chromosome_position = integer(),
                                 distance = integer(),
                                 is_closest_gene = logical(),
                                 is_intergenic = logical(),
                                 is_upstream = logical(),
                                 is_downstream = logical(),
                                 source = character(),
                                 mapping_method = character()
) {
  tbl <- tibble::tibble(
    variant_id = variant_id,
    gene_name = gene_name,
    chromosome_name = chromosome_name,
    chromosome_position = chromosome_position,
    distance = distance,
    is_closest_gene = is_closest_gene,
    is_intergenic = is_intergenic,
    is_upstream = is_upstream,
    is_downstream = is_downstream,
    source = source,
    mapping_method = mapping_method
  )

  return(tbl)
}

#' Creates a gene Ensembl identifiers table.
#'
#' Creates a gene Ensembl identifiers table.
#'
#' @param variant_id TODO.
#' @param gene_name TODO.
#' @param ensembl_id TODO.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
v_ensembl_ids_tbl <- function(variant_id = character(),
                             gene_name = character(),
                             ensembl_id = character()) {
  tbl <- tibble::tibble(variant_id = variant_id,
                        gene_name = gene_name,
                        ensembl_id = ensembl_id)

  return(tbl)
}

#' Creates a gene Entrez identifiers table.
#'
#' Creates a gene Entrez identifiers table.
#'
#' @param variant_id TODO.
#' @param gene_name TODO.
#' @param entrez_id TODO.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
v_entrez_ids_tbl <- function(variant_id = character(),
                          gene_name = character(),
                          entrez_id = character()) {
  tbl <- tibble::tibble(variant_id = variant_id,
                        gene_name = gene_name,
                        entrez_id = entrez_id)

  return(tbl)
}


