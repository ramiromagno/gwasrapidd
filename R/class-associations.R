setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of GWAS Catalog associations
#'
#' The association object consists of six slots, each a table
#' (\code{\link[tibble]{tibble}}), that combined form a relational database of a
#' subset of GWAS Catalog associations. Each association is an observation (row)
#' in the \code{associations} table --- main table. All tables have the column
#' \code{association_id} as primary key.
#'
#' @slot associations A \code{\link[tibble]{tibble}} listing associations.
#'   Columns:
#' \describe{
#' \item{association_id}{GWAS Catalog association accession identifier, e.g.,
#' \code{"20250"}.}
#' \item{pvalue}{Reported p-value for strongest variant risk or effect allele.}
#' \item{pvalue_description}{Information describing context of p-value.}
#' \item{pvalue_mantissa}{Mantissa of p-value.}
#' \item{pvalue_exponent}{Exponent of p-value.}
#' \item{multiple_snp_haplotype}{Whether the association is for a multi-SNP
#' haplotype.}
#' \item{snp_interaction}{Whether the association is for a SNP-SNP interaction.}
#' \item{snp_type}{Whether the SNP has previously been reported. Either
#' \code{'known'} or \code{'novel'}.}
#' \item{standard_error}{Standard error of the effect size.}
#' \item{range}{Reported 95\% confidence interval associated with strongest SNP
#' risk allele, along with unit in the case of beta coefficients. If 95\% CIs
#' have not been not reported, these are estimated using the standard error,
#' when available.}
#' \item{or_per_copy_number}{Reported odds ratio (OR) associated
#' with strongest SNP risk allele. Note that all ORs included in the
#' Catalog are >1.}
#' \item{beta_number}{Beta coefficient associated with strongest SNP risk
#' allele.}
#' \item{beta_unit}{Beta coefficient unit.}
#' \item{beta_direction}{Beta coefficient direction, either \code{'decrease'} or
#' \code{'increase'}.}
#' \item{beta_description}{Additional beta coefficient comment.}
#' \item{last_mapping_date}{Last time this association was mapped to Ensembl.}
#' \item{last_update_date}{Last time this association was updated.}
#' }
#' @slot loci A \code{\link[tibble]{tibble}} listing loci. Columns:
#' \describe{
#' \item{association_id}{GWAS Catalog association accession identifier, e.g.,
#' \code{"20250"}.}
#' \item{locus_id}{A locus identifier referring to a single variant locus or to
#' a multi-loci entity such as a multi-SNP haplotype.}
#' \item{haplotype_snp_count}{Number of variants per locus. Most loci are
#' single-SNP loci, i.e., there is a one to one relationship between a variant
#' and a \code{locus_id} (\code{haplotype_snp_count == NA}). There are however
#' cases of associations involving multiple loci at once, such as SNP-SNP
#' interactions and multi-SNP haplotypes. This is signalled in the columns:
#' \code{multiple_snp_haplotype} and \code{snp_interaction} with value
#' \code{TRUE}.}
#' \item{description}{Description of the locus identifier, e.g.,
#' \code{'Single variant'}, \code{SNP x SNP interaction}, or \code{3-SNP
#' Haplotype}.}
#' }
#' @slot risk_alleles A \code{\link[tibble]{tibble}} listing risk alleles.
#'   Columns:
#' \describe{
#' \item{association_id}{GWAS Catalog association accession identifier, e.g.,
#' \code{"20250"}.}
#' \item{locus_id}{A locus identifier referring to a single variant locus or to
#' a multi-loci entity such as a multi-SNP haplotype.}
#' \item{variant_id}{Variant identifier, e.g., \code{'rs1333048'}.}
#' \item{risk_allele}{Risk allele or effect allele.}
#' \item{risk_frequency}{Reported risk/effect allele frequency associated with
#' strongest SNP in controls (if not available among all controls, among the
#' control group with the largest sample size). If the associated locus is a
#' haplotype the haplotype frequency will be extracted.}
#' \item{genome_wide}{Whether this variant allele has been part of a genome-wide
#' study or not.}
#' \item{limited_list}{Undocumented.}
#' }
#' @slot genes A \code{\link[tibble]{tibble}} listing author reported genes.
#'   Columns:
#' \describe{
#' \item{association_id}{GWAS Catalog association accession identifier, e.g.,
#' \code{"20250"}.}
#' \item{locus_id}{A locus identifier referring to a single variant locus or to
#' a multi-loci entity such as a multi-SNP haplotype.}
#' \item{gene_name}{Gene symbol according to
#' \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.} }
#' @slot ensembl_ids A \code{\link[tibble]{tibble}} listing Ensembl gene
#'   identifiers. Columns:
#' \describe{
#' \item{association_id}{GWAS Catalog association accession identifier, e.g.,
#' \code{"20250"}.}
#' \item{locus_id}{A locus identifier referring to a single variant locus or to
#' a multi-loci entity such as a multi-SNP haplotype.}
#' \item{gene_name}{Gene symbol according to
#' \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.}
#' \item{entrez_id}{The Entrez identifier of a gene, see
#' \doi{10.1093/nar/gkq1237} for more information.}
#' \item{ensembl_id}{The Ensembl identifier of an Ensembl gene, see Section
#' \href{https://www.ensembl.org/info/genome/genebuild/index.html}{Gene
#' annotation in Ensembl} for more information.} }
#' @slot entrez_ids A \code{\link[tibble]{tibble}} listing Entrez gene
#'   identifiers. Columns:
#' \describe{
#' \item{association_id}{GWAS Catalog association accession identifier, e.g.,
#' \code{"20250"}.}
#' \item{locus_id}{A locus identifier referring to a single variant locus or to
#' a multi-loci entity such as a multi-SNP haplotype.}
#' \item{gene_name}{Gene symbol according to
#' \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.}
#' \item{entrez_id}{The Entrez identifier of a gene, see ref.
#' \doi{10.1093/nar/gkq1237} for
#' more information.}
#' }
#' @export
setClass(
  "associations",
  slots = c(
    associations = "tbl_df",
    loci = "tbl_df",
    risk_alleles = "tbl_df",
    genes = "tbl_df",
    ensembl_ids = "tbl_df",
    entrez_ids = "tbl_df"
  )
)

#' Constructor for the S4 associations object.
#'
#' Constructor for the S4 \linkS4class{associations} object.
#'
#' @param associations An \code{\link{associations_tbl}} tibble.
#' @param loci A \code{\link{loci_tbl}} tibble.
#' @param risk_alleles A \code{\link{risk_alleles_tbl}} tibble.
#' @param genes A \code{\link{reported_genes_tbl}} tibble.
#' @param ensembl_ids A \code{\link{ensembl_ids_tbl}} tibble.
#' @param entrez_ids A \code{\link{entrez_ids_tbl}} tibble.
#'
#' @return An object of class \linkS4class{associations}.
#' @keywords internal
associations <- function(associations = associations_tbl(),
                         loci = loci_tbl(),
                         risk_alleles = risk_alleles_tbl(),
                         genes = reported_genes_tbl(),
                         ensembl_ids = ensembl_ids_tbl(),
                         entrez_ids = entrez_ids_tbl()) {

  s4_associations <- methods::new("associations",
      associations = associations,
      loci = loci,
      risk_alleles = risk_alleles,
      genes = genes,
      ensembl_ids = ensembl_ids,
      entrez_ids = entrez_ids
  )

  # Drop rows in tibbles whose value of association_id == NA_character.
  associations_drop_na(s4_associations)
}

#' Creates an associations table.
#'
#' Creates an associations table.
#'
#' @param association_id A character vector of association identifiers.
#' @param pvalue A numeric vector of p-values.
#' @param pvalue_description A character vector of p-value context descriptions.
#' @param pvalue_mantissa An integer vector of p-value mantissas.
#' @param pvalue_exponent An integer vector of p-value exponents.
#' @param multiple_snp_haplotype A logical vector.
#' @param snp_interaction A logical vector.
#' @param snp_type A character vector indicating SNP novelty: 'novel' or
#'   'known'.
#' @param standard_error A numeric vector of standard errors.
#' @param range A character vector of free text descriptions of confidence
#'   intervals.
#' @param or_per_copy_number A numeric vector of odds ratios.
#' @param beta_number A numeric vector of beta coefficients.
#' @param beta_unit A character vector of beta coefficient units.
#' @param beta_direction A character vector of beta coefficient directions.
#' @param beta_description A character vector of beta descriptions.
#' @param last_mapping_date  A \code{\link[base:DateTimeClasses]{POSIXct}}
#'   object indicating last time this association was mapped to Ensembl.
#' @param last_update_date A \code{\link[base:DateTimeClasses]{POSIXct}} object
#'   indicating the last time the associations have been updated.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
associations_tbl <- function(
  association_id = character(),
  pvalue = double(),
  pvalue_description = character(),
  pvalue_mantissa = integer(),
  pvalue_exponent = integer(),
  multiple_snp_haplotype = logical(),
  snp_interaction = logical(),
  snp_type = character(),
  standard_error = double(),
  range = character(),
  or_per_copy_number = double(),
  beta_number = double(),
  beta_unit = character(),
  beta_direction = character(),
  beta_description = character(),
  last_mapping_date = lubridate::ymd_hms(),
  last_update_date = lubridate::ymd_hms()) {

  tbl <- tibble::tibble(
    association_id = association_id,
    pvalue = pvalue,
    pvalue_description = pvalue_description,
    pvalue_mantissa = pvalue_mantissa,
    pvalue_exponent = pvalue_exponent,
    multiple_snp_haplotype = multiple_snp_haplotype,
    snp_interaction = snp_interaction,
    snp_type = snp_type,
    standard_error = standard_error,
    range = range,
    or_per_copy_number = or_per_copy_number,
    beta_number = beta_number,
    beta_unit = beta_unit,
    beta_direction = beta_direction,
    beta_description = beta_description,
    last_mapping_date = last_mapping_date,
    last_update_date = last_update_date
  )
  return(tbl)
}

#' Creates a loci table.
#'
#' Creates a loci table.
#'
#' @param association_id A character vector of association identifiers.
#' @param locus_id An integer vector of locus identifiers.
#' @param haplotype_snp_count An integer vector indicating the number of
#'   variants in the haplotype.
#' @param description A character vector of descriptions, one per locus
#'   identifier.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
loci_tbl <- function(
  association_id = character(),
  locus_id = integer(),
  haplotype_snp_count = integer(),
  description = character()
) {

  tbl <- tibble::tibble(
    association_id = association_id,
    locus_id = locus_id,
    haplotype_snp_count = haplotype_snp_count,
    description = description
  )
  return(tbl)
}


#' Creates a risk alleles table.
#'
#' Creates a risk alleles table.
#'
#' @param association_id A character vector of association identifiers.
#' @param locus_id An integer vector of locus identifiers.
#' @param variant_id A character vector of variant identifiers.
#' @param risk_allele A character vector of risk or effect allele names.
#' @param risk_frequency A numeric vector of the frequency of risk or effect
#'   alleles.
#' @param genome_wide A logical vector.
#' @param limited_list A logical vector.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
risk_alleles_tbl <- function(
  association_id = character(),
  locus_id = integer(),
  variant_id = character(),
  risk_allele = character(),
  risk_frequency = double(),
  genome_wide = logical(),
  limited_list = logical()
) {
  tbl <- tibble::tibble(
    association_id = association_id,
    locus_id = locus_id,
    variant_id = variant_id,
    risk_allele = risk_allele,
    risk_frequency = risk_frequency,
    genome_wide = genome_wide,
    limited_list = limited_list
  )
  return(tbl)
}


#' Creates an authors' reported genes table.
#'
#' Creates an authors' reported genes table.
#'
#' @param association_id A character vector of association identifiers.
#' @param locus_id An integer vector of locus identifiers.
#' @param gene_name A character vector of gene symbol according to
#'   \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
reported_genes_tbl <- function(
  association_id = character(),
  locus_id = integer(),
  gene_name = character()
) {
  tbl <- tibble::tibble(
    association_id = association_id,
    locus_id = locus_id,
    gene_name = gene_name
  )
  return(tbl)
}

#' Creates an Ensembl gene identifiers' table.
#'
#' Creates an Ensembl gene identifiers' table.
#'
#' @param association_id A character vector of association identifiers.
#' @param locus_id An integer vector of locus identifiers.
#' @param gene_name A character vector of gene symbol according to
#'   \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.
#' @param ensembl_id A character vector of Ensembl identifiers.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
ensembl_ids_tbl <- function(
  association_id = character(),
  locus_id = integer(),
  gene_name = character(),
  ensembl_id = character()
) {
  tbl <- tibble::tibble(
    association_id = association_id,
    locus_id = locus_id,
    gene_name = gene_name,
    ensembl_id = ensembl_id
  )
  return(tbl)
}

#' Creates an Entrez gene identifiers' table.
#'
#' Creates an Entrez gene identifiers' table.
#'
#' @param association_id A character vector of association identifiers.
#' @param locus_id An integer vector of locus identifiers.
#' @param gene_name A character vector of gene symbol according to
#'   \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.
#' @param entrez_id A character vector of Entrez identifiers.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
entrez_ids_tbl <- function(
  association_id = character(),
  locus_id = integer(),
  gene_name = character(),
  entrez_id = character()
) {
  tbl <- tibble::tibble(
    association_id = association_id,
    locus_id = locus_id,
    gene_name = gene_name,
    entrez_id = entrez_id
  )
  return(tbl)
}


#' Drop any NA associations.
#'
#' This function takes an associations S4 object and removes any association
#' identifiers that might have been NA. This ensures that there is always a
#' non-NA \code{association_id} value in all tables. This is important as the
#' \code{association_id} is the primary key.
#'
#' @param s4_associations An object of class \linkS4class{associations}.
#'
#' @return An object of class \linkS4class{associations}.
#' @keywords internal
associations_drop_na <- function(s4_associations) {

  # Drop any association_id == NA_character_
  association_id <- rlang::expr(association_id)
  s4_associations@associations <- tidyr::drop_na(
    s4_associations@associations, !!association_id)

  # Extract non-NA association ids
  association_ids <- s4_associations@associations$association_id

  # Filter remaining tibbles with non-NA association ids to ensure that the
  # primary key (association_id) always exists and is not NA.
  s4_associations@loci <- dplyr::filter(
    s4_associations@loci, !!association_id %in% association_ids)

  s4_associations@risk_alleles <- dplyr::filter(
    s4_associations@risk_alleles, !!association_id %in% association_ids)

  s4_associations@genes <- dplyr::filter(
    s4_associations@genes, !!association_id %in% association_ids)

  s4_associations@ensembl_ids <- dplyr::filter(
    s4_associations@ensembl_ids, !!association_id %in% association_ids)

  s4_associations@entrez_ids <- dplyr::filter(
    s4_associations@entrez_ids, !!association_id %in% association_ids)

  return(s4_associations)
}
