setOldClass(c("tbl_df", "tbl", "data.frame"))

#' An S4 class to represent a set of GWAS Catalog associations
#'
#' The association object consists of four slots, each a table
#' (\code{\link[tibble]{tibble}}), that combined form a relational database of a
#' subset of GWAS Catalog associations. Each association is an observation (row) in the
#' associations table --- main table. All tables have the column \code{association_id} as
#' primary key.
#'
#' @slot associations A \code{\link[tibble]{tibble}} listing associations. Columns:
#' \describe{
#' \item{association_id}{GWAS Catalog association accession identifier, e.g., \code{20250}.}
#' \item{pvalue}{Reported p-value for strongest variant risk allele.}
#' \item{pvalue_description}{Information describing context of p-value.}
#' \item{pvalue_mantissa}{Mantissa of p-value.}
#' \item{pvalue_exponent}{Exponent of p-value.}
#' \item{multiple_snp_haplotype}{Whether the association is for a multi-SNP haplotype.}
#' \item{snp_interaction}{Whether the association is for a SNP-SNP interaction.}
#' \item{snp_type}{Whether the SNP has previously been reported. Either \code{'known'} or \code{'novel'}.}
#' \item{standard_error}{Standard error of the effect size.}
#' \item{range}{Reported 95\% confidence interval associated with strongest SNP
#' risk allele, along with unit in the case of beta-coefficients. If 95% CIs have not been
#' not reported, these are estimated using the standard error, when available.}
#' \item{or_per_copy_number}{Reported odds ratio (OR) associated
#' with strongest SNP risk allele. Note that all ORs included in the
#' Catalog are >1.}
#' \item{beta_number}{Beta-coefficient associated with strongest SNP risk allele.}
#' \item{beta_unit}{Beta coefficient unit.}
#' \item{beta_direction}{Beta coefficient direction.}
#' \item{beta_description}{Additional beta coefficient comment.}
#' \item{last_mapping_date}{Last time this association was mapped to Ensembl.}
#' \item{last_update_date}{Last time this association was updated.}
#' }
#' @slot loci A \code{\link[tibble]{tibble}} listing loci. Columns:
#' \describe{
#' \item{association_id}{GWAS Catalog association accession identifier, e.g., \code{20250}.}
#' \item{locus_id}{TODO}
#' \item{haplotype_snp_count}{TODO}
#' \item{description}{TODO}
#' }
#' @slot risk_alleles A \code{\link[tibble]{tibble}} listing risk alleles. Columns:
#' \describe{
#' \item{association_id}{GWAS Catalog association accession identifier, e.g., \code{20250}.}
#' \item{locus_id}{TODO}
#' \item{variant_id}{TODO}
#' \item{risk_allele}{TODO}
#' \item{risk_frequency}{TODO}
#' \item{genome_wide}{TODO}
#' \item{limited_list}{TODO}
#' }
#' @slot genes A \code{\link[tibble]{tibble}} listing author reported genes. Columns:
#' \describe{
#' \item{association_id}{GWAS Catalog association accession identifier, e.g., \code{20250}.}
#' \item{locus_id}{TODO.}
#' \item{gene_name}{TODO.}
#' }
#' @slot ensembl_ids A \code{\link[tibble]{tibble}} listing Ensembl gene identifiers. Columns:
#' \describe{
#' \item{association_id}{GWAS Catalog association accession identifier, e.g., \code{20250}.}
#' \item{locus_id}{TODO.}
#' \item{gene_name}{Gene symbol according to \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.}
#' \item{entrez_id}{The Entrez identifier of a gene, see ref.
#' \href{https://dx.doi.org/10.1093\%2Fnar\%2Fgkq1237}{10.1093/nar/gkq1237} for
#' more information.}
#' \item{ensembl_id}{The Ensembl identifier of an Ensembl gene, see Section
#' \href{https://www.ensembl.org/info/genome/genebuild/genome_annotation.html}{Gene
#' annotation in Ensembl} for more information.}
#' }
#' @slot entrez_ids A \code{\link[tibble]{tibble}} listing Entrez gene identifiers. Columns:
#' \describe{
#' \item{association_id}{GWAS Catalog association accession identifier, e.g., \code{20250}.}
#' \item{locus_id}{TODO.}
#' \item{gene_name}{Gene symbol according to \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.}
#' \item{entrez_id}{The Entrez identifier of a gene, see ref.
#' \href{https://dx.doi.org/10.1093\%2Fnar\%2Fgkq1237}{10.1093/nar/gkq1237} for
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
#' @param associations TODO.
#' @param loci TODO.
#' @param risk_alleles TODO.
#' @param genes TODO.
#' @param ensembl_ids TODO.
#' @param entrez_ids TODO.
#'
#' @return An object of class \linkS4class{associations}.
#' @keywords internal
associations <- function(associations = associations_tbl(),
                         loci = loci_tbl(),
                         risk_alleles = risk_alleles_tbl(),
                         genes = reported_genes_tbl(),
                         ensembl_ids = ensembl_ids_tbl(),
                         entrez_ids = entrez_ids_tbl()) {
  methods::new("associations",
      associations = associations,
      loci = loci,
      risk_alleles = risk_alleles,
      genes = genes,
      ensembl_ids = ensembl_ids,
      entrez_ids = entrez_ids
  )
}

#' Creates an associations table.
#'
#' Creates an associations table.
#'
#' @param association_id TODO.
#' @param pvalue TODO.
#' @param pvalue_description TODO.
#' @param pvalue_mantissa TODO.
#' @param pvalue_exponent TODO.
#' @param multiple_snp_haplotype TODO.
#' @param snp_interaction TODO.
#' @param snp_type TODO.
#' @param standard_error TODO.
#' @param range TODO.
#' @param or_per_copy_number TODO.
#' @param beta_number TODO.
#' @param beta_unit TODO.
#' @param beta_direction TODO.
#' @param beta_description TODO.
#' @param last_mapping_date TODO.
#' @param last_update_date TODO.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
associations_tbl <- function(
  association_id = integer(),
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
#' @param association_id TODO.
#' @param locus_id TODO.
#' @param haplotype_snp_count TODO.
#' @param description TODO.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
loci_tbl <- function(
  association_id = integer(),
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
#' @param association_id TODO.
#' @param locus_id TODO.
#' @param variant_id TODO.
#' @param risk_allele TODO.
#' @param risk_frequency TODO.
#' @param genome_wide TODO.
#' @param limited_list TODO.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
risk_alleles_tbl <- function(
  association_id = integer(),
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
#' @param association_id TODO.
#' @param locus_id TODO.
#' @param gene_name TODO.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
reported_genes_tbl <- function(
  association_id = integer(),
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
#' @param association_id TODO.
#' @param locus_id TODO.
#' @param gene_name TODO.
#' @param ensembl_id TODO.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
ensembl_ids_tbl <- function(
  association_id = integer(),
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
#' @param association_id TODO.
#' @param locus_id TODO.
#' @param gene_name TODO.
#' @param entrez_id TODO.
#'
#' @return A \code{\link[tibble]{tibble}} whose columns are the named arguments
#'   to the function.
#' @keywords internal
entrez_ids_tbl <- function(
  association_id = integer(),
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
