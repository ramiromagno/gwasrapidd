#' Get GWAS Catalog variants by study identifiers
#'
#' Gets variants by GWAS Catalog internal study identifiers.
#'
#' @param study_id A character vector of GWAS Catalog study accession
#'   identifiers.
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_study_id <- function(study_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(study_id))
    return(variants())

  assertthat::assert_that(
    is.character(study_id),
    length(study_id) > 0,
    assertthat::noNA(study_id),
    all(is_study_id(study_id)))

  resource_urls <- sprintf("/%s/%s/%s", "studies", study_id, "snps")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty variants object.
  if(rlang::is_empty(responses_ok)) return(variants())

  obj <- plst_left_join(responses_ok)
  my_variants <- v_obj_to_variants(obj)

  return(my_variants)
}

#' Get GWAS Catalog variants by their association identifiers
#'
#' Gets variants by GWAS Catalog internal association identifiers.
#'
#' @param association_id A character vector of GWAS Catalog association
#'   identifiers.
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_association_id <- function(association_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(association_id))
    return(variants())

  assertthat::assert_that(
    is.character(association_id),
    length(association_id) > 0,
    assertthat::noNA(association_id),
    all(is_association_id(association_id)))

  resource_urls <- sprintf("/%s/%s/%s", "associations", association_id, "snps")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty variants object.
  if(rlang::is_empty(responses_ok)) return(variants())

  obj <- plst_left_join(responses_ok)

  my_variants <- v_obj_to_variants(obj)

  return(my_variants)
}

#' Get GWAS Catalog variants by variant identifiers
#'
#' Gets variants by variant identifiers.
#'
#' @param variant_id A character vector of GWAS Catalog variant identifiers.
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_variant_id <- function(variant_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(variant_id))
    return(variants())

  assertthat::assert_that(
    is.character(variant_id),
    length(variant_id) > 0,
    assertthat::noNA(variant_id))

  resource_urls <- sprintf("/%s/%s/%s", "singleNucleotidePolymorphisms",
                           urltools::url_encode(variant_id),
                           "")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty variants object.
  if(rlang::is_empty(responses_ok)) return(variants())

  obj <- plst_left_join(responses_ok)

  my_variants <- v_obj_to_variants(obj)

  return(my_variants)
}


#' Get GWAS Catalog studies by EFO identifier
#'
#' Gets variants whose phenotypic trait is matched by EFO identifiers.
#'
#' @param efo_id A character vector of \href{https://www.ebi.ac.uk/efo/}{EFO}
#'   identifiers.
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_efo_id <- function(efo_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(efo_id))
    return(variants())

  assertthat::assert_that(
    is.character(efo_id),
    length(efo_id) > 0,
    assertthat::noNA(efo_id),
    all(is_efo_id2(efo_id)))

  traits <- get_traits_by_efo_id(
    efo_id = efo_id,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # trait_descriptions <- traits@traits$trait
  trait_descriptions <- purrr::pluck(traits, 'traits', 'trait', .default = NULL)

  my_variants <- get_variants_by_efo_trait(
    efo_trait = trait_descriptions,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  return(my_variants)
}

#' Get GWAS Catalog variants by PubMed identifiers
#'
#' Gets variants whose associated publications match
#'  \href{https://en.wikipedia.org/wiki/PubMed}{PubMed} identifiers.
#'
#' @param pubmed_id An integer vector of
#'   \href{https://en.wikipedia.org/wiki/PubMed}{PubMed} identifiers.
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_pubmed_id <- function(pubmed_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(pubmed_id))
    return(variants())

  assertthat::assert_that(
    is.character(pubmed_id),
    length(pubmed_id) > 0,
    assertthat::noNA(pubmed_id))

  resource_urls <- sprintf("/%s%s", "singleNucleotidePolymorphisms/search/findByPubmedId?pubmedId=",
                           urltools::url_encode(pubmed_id))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(variants())

  obj <- plst_left_join(responses_ok)

  my_variants <- v_obj_to_variants(obj)

  return(my_variants)
}

#' Get GWAS Catalog variants by genomic range
#'
#' Gets variants by genomic range.
#'
#' @param chromosome A character vector of human chromosome names: autosomal
#' and sexual chromosomes only, i.e., 1--22, X and Y.
#' @param start Start position of range (starts at 1).
#' @param end End position of range (inclusive).
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_genomic_range <- function(chromosome = NULL, start = NULL, end = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  # If all three genomic arguments are NULL just return an empty variants object.
  if(rlang::is_null(chromosome) && rlang::is_null(start) && rlang::is_null(end))
    return(variants())

  # If however some are NULL but not all of them then trigger an error.
  if(rlang::is_null(chromosome))
    stop("chromosome argument is NULL.")

  if(rlang::is_null(start))
    stop("start argument is NULL.")

  if(rlang::is_null(end))
    stop("end argument is NULL.")

  # Check that all three genomic arguments are of the same length.
  n_chr <- length(chromosome)
  n_start <- length(start)
  n_end <- length(end)
  if(! (identical(n_start, n_end) && identical(n_start, n_chr))) # identical(n_end, n_chr) == TRUE follows.
    stop("chromosome, start and end vectors should be of same length: ",
         "len(chr) = ", n_chr, ", ",
         "len(start) = ", n_start, ", and ",
         "len(end) = ", n_end, ".")

  # Check that the chromosome names are valid human chromosome names: autosomal + sex chrs.
  if(any(!is_human_chromosome(chromosome)))
    stop("One or more chromosome names are not valid, i.e., not 1-22, X or Y.")

  starting_position = 1L
  max_end_position <- 999999999L

  # Check start and end ranges.
  is_start_below_starting_pos <- start < starting_position
  if(any(is_start_below_starting_pos))
    stop("All start positions must be greater than ", starting_position, ", these are not: ",
         concatenate::cc_and(start[is_start_below_starting_pos], oxford = TRUE), ".")

  is_end_below_starting_pos <- end < starting_position
  if(any(is_end_below_starting_pos))
    stop("All end positions must be greater than ", starting_position, ", these are not: ",
         concatenate::cc_and(end[is_end_below_starting_pos], oxford = TRUE), ".")

  is_start_above_max_ending_pos <- start > max_end_position
  if(any(is_start_above_max_ending_pos))
    stop("All start positions must be lesser than ", max_end_position, ", these are not: ",
         concatenate::cc_and(start[is_start_above_max_ending_pos], oxford = TRUE), ".")

  is_end_above_max_ending_pos <- end > max_end_position
  if(any(is_end_above_max_ending_pos))
    stop("All end positions must be lesser than ", max_end_position, ", these are not: ",
         concatenate::cc_and(end[is_end_above_max_ending_pos], oxford = TRUE), ".")



  resource_urls <- sprintf("%s%s%s%s%s%s",
                           "/singleNucleotidePolymorphisms/search/findByChromBpLocationRange?chrom=",
                           chromosome,
                           "&bpStart=", start,
                           "&bpEnd=", end)

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(variants())

  obj <- plst_left_join(responses_ok)

  my_variants <- v_obj_to_variants(obj)

  return(my_variants)
}

#' Get GWAS Catalog variants by cytogenetic band.
#'
#' Gets variants that are mapped onto specific regions as specified by
#' cytogenetic bands. See the dataframe
#' \code{\link[gwasrapidd]{cytogenetic_bands}} for more information on possible
#' values.
#'
#' @param cytogenetic_band A \code{character} vector of cytogenetic bands of the
#'   form \code{'1p36.11'}
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_cytogenetic_band <- function(cytogenetic_band = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(cytogenetic_band))
    return(variants())

  assertthat::assert_that(
    is.character(cytogenetic_band),
    length(cytogenetic_band) > 0,
    assertthat::noNA(cytogenetic_band))

  genomic_ranges <- cytogenetic_band_to_genomic_range(bands = cytogenetic_band)
  my_variants <- get_variants_by_genomic_range(chromosome = genomic_ranges$chromosome,
                                start = genomic_ranges$start,
                                end = genomic_ranges$end,
                                verbose = verbose,
                                warnings = warnings,
                                page_size = page_size)

  return(my_variants)
}

#' Get GWAS Catalog variants by gene name.
#'
#' Gets variants whose genomic context includes a specific gene or genes.
#'
#' @param gene_name A \code{character} vector of gene names.
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_gene_name <- function(gene_name = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(gene_name))
    return(variants())

  assertthat::assert_that(
    is.character(gene_name),
    length(gene_name) > 0,
    assertthat::noNA(gene_name))

  resource_urls <- sprintf("%s%s", "/singleNucleotidePolymorphisms/search/findByGene?geneName=",
                           urltools::url_encode(gene_name))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty studies object.
  if(rlang::is_empty(responses_ok)) return(variants())

  obj <- plst_left_join(responses_ok)

  my_variants <- v_obj_to_variants(obj)

  return(my_variants)
}

#' Get GWAS Catalog variants by EFO traits
#'
#' Gets variants that match \href{https://www.ebi.ac.uk/efo/}{EFO} trait
#' description.
#'
#' @param efo_trait A character vector of
#'   \href{https://www.ebi.ac.uk/efo/}{EFO} trait descriptions, e.g.,
#'   \code{'uric acid measurement'}.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_efo_trait <- function(efo_trait = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(efo_trait))
    return(variants())

  assertthat::assert_that(
    is.character(efo_trait),
    length(efo_trait) > 0,
    assertthat::noNA(efo_trait))

  resource_urls <- sprintf("%s%s", "/singleNucleotidePolymorphisms/search/findByEfoTrait?efoTrait=",
                           urltools::url_encode(efo_trait))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty variants object.
  if(rlang::is_empty(responses_ok)) return(variants())

  obj <- plst_left_join(responses_ok)

  my_variants <- v_obj_to_variants(obj)

  return(my_variants)
}

#' Get GWAS Catalog variants by reported traits
#'
#' Gets variants that match the reported traits, as reported by the original
#' authors' of the study.
#'
#' @param reported_trait A character vector of phenotypic traits as reported by
#'   the original authors' the study. Note: this parameter is case sensitive.
#' @param verbose Whether the function should be verbose about the different
#'   queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_reported_trait <- function(reported_trait = NULL, verbose = FALSE, warnings = TRUE, page_size = 20L) {

  if(rlang::is_null(reported_trait))
    return(variants())

  assertthat::assert_that(
    is.character(reported_trait),
    length(reported_trait) > 0,
    assertthat::noNA(reported_trait))

  resource_urls <- sprintf("%s%s", "/singleNucleotidePolymorphisms/search/findByDiseaseTrait?diseaseTrait=",
                           urltools::url_encode(reported_trait))

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty variants object.
  if(rlang::is_empty(responses_ok)) return(variants())

  obj <- plst_left_join(responses_ok)

  my_variants <- v_obj_to_variants(obj)

  return(my_variants)
}

#' Get all GWAS Catalog variants
#'
#' Gets all variants. Beware this can take several minutes!
#'
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_all <- function(verbose = FALSE, warnings = TRUE, page_size = 20L) {

  resource_urls <- "/singleNucleotidePolymorphisms"

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = warnings,
    page_size = page_size)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ all(.x$status == 'OK'))

  # If none of the responses were successful then return an empty variants object.
  if(rlang::is_empty(responses_ok)) return(variants())

  obj <- plst_left_join(responses_ok)

  my_variants <- v_obj_to_variants(obj)

  return(my_variants)
}

#' Get GWAS Catalog variants
#'
#' Retrieves variants via the NHGRI-EBI GWAS Catalog REST API. The REST
#' API is queried multiple times with the criteria passed as arguments (see
#' below). By default all variants that match the criteria supplied in the
#' arguments are retrieved: this corresponds to the default option
#' \code{set_operation} set to \code{'union'}. If you rather have only the
#' variants that match simultaneously all criteria provided, then set
#' \code{set_operation} to \code{'intersection'}.
#'
#' Please note that all search criteria are vectorised, thus allowing for batch
#' mode search, e.g., one can search by multiple variant identifiers at once by
#' passing a vector of identifiers to \code{variant_id}.
#'
#' @param study_id A character vector of GWAS Catalog study accession
#'   identifiers.
#' @param association_id A character vector of GWAS Catalog association
#'   identifiers.
#' @param variant_id A character vector of GWAS Catalog variant identifiers.
#' @param efo_id A character vector of \href{https://www.ebi.ac.uk/efo/}{EFO}
#'   identifiers.
#' @param pubmed_id An integer vector of
#'   \href{https://pubmed.ncbi.nlm.nih.gov/}{PubMed} identifiers.
#' @param genomic_range A named list of three vectors:
#' \describe{ \item{chromosome}{A character vector of chromosome names of the
#' form 1--22, X or Y.} \item{start}{A numeric vector of start positions,
#' starting at 1.} \item{end}{A numeric vector of end positions.} }
#' The three vectors need to be of the same length so that \code{chromosome}
#' names, \code{start} and \code{end} positions can be matched by position.
#' @param cytogenetic_band A character vector of cytogenetic bands of the form
#'   \code{'1p36.11'}.
#' @param gene_name Gene symbol according to
#'   \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.
#' @param efo_trait A character vector of
#'   \href{https://www.ebi.ac.uk/efo/}{EFO} trait descriptions, e.g.,
#'   \code{'uric acid measurement'}.
#' @param reported_trait A character vector of phenotypic traits as
#'   reported by the original authors of the study.
#' @param set_operation Either \code{'union'} or \code{'intersection'}. This
#'   tells how variants retrieved by different criteria  should be combined:
#'   \code{'union'} binds together all results removing duplicates and
#'   \code{'intersection'} only keeps same variants found with different
#'   criteria.
#' @param interactive A logical. If all variants are requested, whether to ask
#'   interactively if we really want to proceed.
#' @param std_chromosomes_only Whether to return only variants mapped to
#'   standard chromosomes: 1 thru 22, X, Y, and MT.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A \linkS4class{variants} object.
#' @examples
#' # Get variants by study identifier
#' get_variants(study_id = 'GCST001085', warnings = FALSE)
#'
#' # Get a variant by its identifier
#' \dontrun{
#' get_variants(variant_id = 'rs3798440', warnings = FALSE)
#' }
#'
#' @export
get_variants <- function(study_id = NULL,
                         association_id = NULL,
                         variant_id = NULL,
                         efo_id = NULL,
                         pubmed_id = NULL,
                         genomic_range = NULL,
                         cytogenetic_band = NULL,
                         gene_name = NULL,
                         efo_trait = NULL,
                         reported_trait = NULL,
                         set_operation = 'union',
                         interactive = TRUE,
                         std_chromosomes_only = TRUE,
                         verbose = FALSE,
                         warnings = TRUE) {


  if(!(rlang::is_scalar_character(set_operation) && set_operation %in% c('union', 'intersection')))
    stop("set_operation must be either 'union' or 'intersection'")

  if(!(rlang::is_scalar_logical(verbose) && verbose %in% c(TRUE, FALSE)))
    stop("verbose must be either TRUE or FALSE")

  if(!(rlang::is_scalar_logical(warnings) && warnings %in% c(TRUE, FALSE)))
    stop("warnings must be either TRUE or FALSE")

  if(!(rlang::is_scalar_logical(std_chromosomes_only) && std_chromosomes_only %in% c(TRUE, FALSE)))
    stop("std_chromosomes_only must be either TRUE or FALSE")

  if(!rlang::is_null(genomic_range) &&
     !all(rlang::has_name(genomic_range, c("chromosome", "start", "end")))) {
    stop("Argument genomic_range must be a list with three named elements:",
         "chromosome, start and end.")
  }

  list_of_variants = list()

  if (!rlang::is_null(study_id))
    list_of_variants[['variants_by_study_id']] <-
    get_variants_by_study_id(study_id = study_id,
                                 verbose = verbose,
                                 warnings = warnings)

  if (!rlang::is_null(association_id))
    list_of_variants[['variants_by_association_id']] <-
    get_variants_by_association_id(association_id = association_id,
                                       verbose = verbose,
                                       warnings = warnings)

  if (!rlang::is_null(variant_id))
    list_of_variants[['variants_by_variant_id']] <-
    get_variants_by_variant_id(variant_id = variant_id,
                                   verbose = verbose,
                                   warnings = warnings)

  if (!rlang::is_null(efo_id))
    list_of_variants[['variants_by_efo_id']] <-
    get_variants_by_efo_id(efo_id = efo_id,
                               verbose = verbose,
                               warnings = warnings)

  if (!rlang::is_null(pubmed_id))
    list_of_variants[['variants_by_pubmed_id']] <-
    get_variants_by_pubmed_id(pubmed_id = pubmed_id,
                                  verbose = verbose,
                                  warnings = warnings)

  if (!rlang::is_null(genomic_range))
    list_of_variants[['variants_by_genomic_range']] <-
    get_variants_by_genomic_range(
      chromosome = genomic_range$chromosome,
      start = genomic_range$start,
      end = genomic_range$end,
      verbose = verbose,
      warnings = warnings
    )

  if (!rlang::is_null(cytogenetic_band))
    list_of_variants[['variants_by_cytogenetic_band']] <-
    get_variants_by_cytogenetic_band(cytogenetic_band = cytogenetic_band,
                              verbose = verbose,
                              warnings = warnings)

  if (!rlang::is_null(gene_name))
    list_of_variants[['variants_by_gene_name']] <-
    get_variants_by_gene_name(gene_name = gene_name,
                              verbose = verbose,
                              warnings = warnings)

  if (!rlang::is_null(efo_trait))
    list_of_variants[['variants_by_efo_trait']] <-
    get_variants_by_efo_trait(efo_trait = efo_trait,
                                  verbose = verbose,
                                  warnings = warnings)

  if (!rlang::is_null(reported_trait))
    list_of_variants[['variants_by_reported_trait']] <-
    get_variants_by_reported_trait(reported_trait = reported_trait,
                              verbose = verbose,
                              warnings = warnings)

  # If no criteria have been passed, i.e. all are NULL then got fetch all
  # variants
  if(rlang::is_empty(list_of_variants)) {
    msg1 <- "You are about to download all variants from the GWAS Catalog.\nThis might take several hours."
    msg2 <- 'Returning an empty variants object!'
    msg3 <- 'OK! Getting all variants then. This is going to take a while...'
    if(interactive)
      default_answer = NULL  # i.e., use interactive mode.
    else
      default_answer = 'y'
    if(sure(before_question = msg1, after_saying_no = msg2, after_saying_yes = msg3, default_answer = default_answer))
      return(get_variants_all(verbose = verbose, warnings = warnings))
    else
      return(variants())
  } else {

    if (identical(set_operation, "union")) {
      v <- purrr::reduce(list_of_variants, union)
      if(std_chromosomes_only)
        v <- filter_variants_by_standard_chromosomes(v)
      return(v)
    }

    if (identical(set_operation, "intersection")) {
      v <- purrr::reduce(list_of_variants, intersect)
      if(std_chromosomes_only)
        v <- filter_variants_by_standard_chromosomes(v)
      return(v)
    }
  }
}

#' Check if a variant exists in the Catalog.
#'
#' This function attempts to get a variant by its variant identifier and checks
#' the response code. If the response code is 200 then the response has been
#' successful, meaning that the variant does exist in the GWAS Catalog. If the
#' response is 404 then the variant is not found in the Catalog database. Other
#' errors are mapped to NA.
#'
#' @param variant_id A character vector of GWAS Catalog variant identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param page_size An integer scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A named logical vector, \code{TRUE} indicates that the variant does
#'   exist in the Catalog, \code{FALSE} otherwise. \code{NA} codes other types
#'   of errors. The names of the vector are the variant identifiers passed as
#'   \code{variant_id}.
#'
#' @examples
#' exists_variant('rs12345')
#'
#' exists_variant('rs11235813')
#'
#' @export
exists_variant <- function(variant_id = NULL, verbose = FALSE, page_size = 20L) {

  if(rlang::is_null(variant_id))
    return(logical())

  assertthat::assert_that(
    is.character(variant_id),
    length(variant_id) > 0,
    assertthat::noNA(variant_id))

  resource_urls <- sprintf("/%s/%s/%s", "singleNucleotidePolymorphisms",
                           urltools::url_encode(variant_id),
                           "")

  responses <- purrr::map(
    resource_urls,
    gc_get,
    verbose = verbose,
    warnings = FALSE,
    page_size = page_size)

  # Check which variants returned 200 (status == 'OK), i.e., variants do exists in the Catalog.
  variant_exists <- purrr::map_lgl(responses, ~ .x$response_code == 200L)

  # Check which variants returned 404, i.e., variants do NOT exist in the Catalog.
  variant_not_exists <- purrr::map_lgl(responses, ~ .x$response_code == 404L)

  other_error_responses <- !(variant_exists | variant_not_exists)

  variant_exists[other_error_responses] <- NA
  names(variant_exists) <- variant_id

  return(variant_exists)
}
