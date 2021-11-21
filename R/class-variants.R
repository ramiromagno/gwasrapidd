setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of GWAS Catalog variants
#'
#' The variants object consists of four slots, each a table
#' (\code{\link[tibble]{tibble}}), that combined form a relational database of a
#' subset of GWAS Catalog variants. Each variant is an observation (row) in the
#' \code{variants} table --- main table. All tables have the column
#' \code{variant_id} as primary key.
#'
#' @slot variants A \code{\link[tibble]{tibble}} listing variants. Columns:
#' \describe{
#' \item{variant_id}{Variant identifier, e.g., \code{'rs1333048'}.}
#' \item{merged}{Whether this SNP has been merged with another SNP in a newer
#' genome build.}
#' \item{functional_class}{Class according to Ensembl's predicted consequences
#' that each variant allele may have on transcripts. See
#' \href{https://www.ensembl.org/info/genome/variation/prediction/predicted_data.html}{Ensembl
#' Variation - Calculated variant consequences}.}
#' \item{chromosome_name}{Chromosome name.}
#' \item{chromosome_position}{Chromosome position.}
#' \item{chromosome_region}{\href{https://medlineplus.gov/genetics/understanding/howgeneswork/genelocation/}{Cytogenetic
#' location}.}
#' \item{last_update_date}{Last time this variant was updated.}
#' }
#' @slot genomic_contexts A \code{\link[tibble]{tibble}} listing genomic
#'   contexts associated with each variant. Columns:
#' \describe{
#' \item{variant_id}{Variant identifier.}
#' \item{gene_name}{Gene symbol according to
#' \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.}
#' \item{chromosome_name}{Chromosome name.}
#' \item{chromosome_position}{Chromosome position.}
#' \item{distance}{Genomic distance between the variant and the gene (in base
#' pairs).}
#' \item{is_mapped_gene}{Whether this is a mapped gene to this variant. A mapped
#' gene is either an overlapping gene with the variant or the two closest genes
#' upstream and downstream of the variant. Moreover, only genes whose mapping
#' source is 'Ensembl' are considered.}
#' \item{is_closest_gene}{Whether this is the closest gene to this variant.}
#' \item{is_intergenic}{Whether this variant is intergenic, i.e, if there is no
#' gene up or downstream within 100kb.}
#' \item{is_upstream}{Whether this variant is upstream of this gene.}
#' \item{is_downstream}{Whether this variant is downstream of this gene.}
#' \item{source}{Gene mapping source, either \code{Ensembl} or \code{NCBI}.}
#' \item{mapping_method}{Gene mapping method.}
#' }
#' @slot ensembl_ids A \code{\link[tibble]{tibble}} listing gene Ensembl
#'   identifiers associated with each genomic context. Columns:
#' \describe{
#' \item{variant_id}{Variant identifier.}
#' \item{gene_name}{Gene symbol according to
#' \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.}
#' \item{ensembl_id}{The Ensembl identifier of an Ensembl gene, see Section
#' \href{https://www.ensembl.org/info/genome/genebuild/index.html}{Gene
#' annotation in Ensembl} for more information.}
#' }
#' @slot entrez_ids A \code{\link[tibble]{tibble}} listing gene Entrez
#'   identifiers associated with each genomic context. Columns:
#' \describe{
#' \item{variant_id}{Variant identifier.}
#' \item{gene_name}{Gene symbol according to
#' \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.}
#' \item{entrez_id}{The Entrez identifier of a gene, see ref.
#' \doi{10.1093/nar/gkq1237} for
#' more information.}
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
#' @param variants A \code{\link{variants_tbl}} tibble.
#' @param genomic_contexts A \code{\link{genomic_contexts_tbl}} tibble.
#' @param ensembl_ids A \code{\link{v_ensembl_ids_tbl}} tibble.
#' @param entrez_ids A \code{\link{v_entrez_ids_tbl}} tibble.
#'
#' @return An object of class \linkS4class{variants}.
#' @keywords internal
variants <- function(variants = variants_tbl(),
                     genomic_contexts = genomic_contexts_tbl(),
                     ensembl_ids = v_ensembl_ids_tbl(),
                     entrez_ids = v_entrez_ids_tbl()) {
  s4_variants <- methods::new("variants",
      variants = variants,
      genomic_contexts = genomic_contexts,
      ensembl_ids = ensembl_ids,
      entrez_ids = entrez_ids
  )
  # Drop rows in tibbles whose value of variant_id == NA_character.
  variants_drop_na(s4_variants)
}

#' Creates a variants table.
#'
#' Creates a variants table.
#'
#' @param variant_id A character vector of variant identifiers.
#' @param merged A logical vector indicating if a SNP has been merged with
#' another SNP in a newer genome build.
#' @param functional_class A character vector of functional classes, see
#'   \code{functional_class} in slot \code{variants} of \linkS4class{variants}.
#' @param chromosome_name A character vector of chromosome names.
#' @param chromosome_position An integer vector of chromosome positions.
#' @param chromosome_region A character vector of cytogenetic regions.
#' @param last_update_date A \code{\link[base:DateTimeClasses]{POSIXct}} object
#'   indicating the last time the variants have been updated.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#' to the function.
#' @keywords internal
variants_tbl <- function(variant_id = character(),
                         merged = integer(),
                         functional_class = character(),
                         chromosome_name = character(),
                         chromosome_position = integer(),
                         chromosome_region = character(),
                         last_update_date = lubridate::ymd_hms()) {

  tbl <- tibble::tibble(
    variant_id = empty_to_na(variant_id),
    merged = empty_to_na(merged),
    functional_class = empty_to_na(functional_class),
    chromosome_name = empty_to_na(chromosome_name),
    chromosome_position = empty_to_na(chromosome_position),
    chromosome_region = empty_to_na(chromosome_region),
    last_update_date = empty_to_na(last_update_date)
  )

  return(tbl)
}

#' Creates a genomic contexts table.
#'
#' Creates a genomic contexts table.
#'
#' @param variant_id A character vector of variant identifiers.
#' @param gene_name A character vector of gene symbols according to
#' \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.
#' @param chromosome_name A character vector of chromosome names.
#' @param chromosome_position An integer vector of chromosome positions.
#' @param distance An integer vector of genomic positions.
#' @param is_closest_gene A logical vector.
#' @param is_intergenic A logical vector.
#' @param is_upstream A logical vector.
#' @param is_downstream A logical vector.
#' @param source A character vector of gene mapping sources.
#' @param mapping_method A character vector of gene mapping methods.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
genomic_contexts_tbl <- function(variant_id = character(),
                                 gene_name = character(),
                                 chromosome_name = character(),
                                 chromosome_position = integer(),
                                 distance = integer(),
                                 is_mapped_gene = logical(),
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
    is_mapped_gene = is_mapped_gene,
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
#' @param variant_id A character vector of variant identifiers.
#' @param gene_name A character vector of gene symbols according to
#' \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.
#' @param ensembl_id A character vector of Ensembl identifiers.
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
#' @param variant_id A character vector of variant identifiers.
#' @param gene_name A character vector of gene symbols according to
#' \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.
#' @param entrez_id A character vector of Entrez identifiers.
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

#' Drop any NA variants.
#'
#' This function takes a variants S4 object and removes any variant identifiers
#' that might have been NA. This ensures that there is always a non-NA
#' \code{variant_id} value in all tables. This is important as the
#' \code{variant_id} is the primary key.
#'
#' @param s4_variants An object of class \linkS4class{variants}.
#'
#' @return An object of class \linkS4class{variants}.
#' @keywords internal
variants_drop_na <- function(s4_variants) {

  # Drop any variant_id == NA_character_
  variant_id <- rlang::expr(variant_id)
  s4_variants@variants <- tidyr::drop_na(s4_variants@variants, !!variant_id)

  # Extract non-NA variant ids
  variant_ids <- s4_variants@variants$variant_id

  # Filter remaining tibbles with non-NA variant ids to ensure that the primary
  # key (variant_id) always exists and is not NA.
  s4_variants@genomic_contexts <- dplyr::filter(
    s4_variants@genomic_contexts, !!variant_id %in% variant_ids)

  s4_variants@ensembl_ids <- dplyr::filter(
    s4_variants@ensembl_ids, !!variant_id %in% variant_ids)

  s4_variants@entrez_ids <- dplyr::filter(
    s4_variants@entrez_ids, !!variant_id %in% variant_ids)

  return(s4_variants)
}
