#' Get GWAS Catalog variants by study identifiers
#'
#' Gets variants by GWAS Catalog internal study identifiers.
#'
#' @param study_id A \code{character} vector of GWAS Catalog study accession
#'   identifiers.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_study_id <- function(study_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 1000L) {

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
#' @param association_id An \code{integer} vector of GWAS Catalog association
#'   identifiers.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_association_id <- function(association_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 1000L) {

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
#' @param variant_id A \code{character} vector of GWAS Catalog variant identifiers.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_variant_id <- function(variant_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 1000L) {

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
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_efo_id <- function(efo_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 1000L) {

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

  trait_descriptions <- traits@traits$trait

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
#' @param pubmed_id An \code{integer} vector of
#'   \href{https://en.wikipedia.org/wiki/PubMed}{PubMed} identifiers.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_pubmed_id <- function(pubmed_id = NULL, verbose = FALSE, warnings = TRUE, page_size = 1000L) {

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
#' @param chromosome A \code{character} of human chromosome names: autosomal
#' and sexual chromosomes only, i.e., 1--22, X and Y.
#' @param start Start position of range (starts at 1).
#' @param end End position of range (inclusive).
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_genomic_range <- function(chromosome = NULL, start = NULL, end = NULL, verbose = FALSE, warnings = TRUE, page_size = 1000L) {

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

#' Get GWAS Catalog variants by gene name.
#'
#' Gets variants whose genomic context includes a specific gene or genes.
#'
#' @param gene_name A \code{character} vector of gene names.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_gene_name <- function(gene_name = NULL, verbose = FALSE, warnings = TRUE, page_size = 1000L) {

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
#' @param efo_trait A \code{character} vector of
#'   \href{https://www.ebi.ac.uk/efo/}{EFO} trait descriptions, e.g.,
#'   \code{'uric acid measurement'}.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_efo_trait <- function(efo_trait = NULL, verbose = FALSE, warnings = TRUE, page_size = 1000L) {

  if(rlang::is_null(efo_trait))
    return(variants())

  assertthat::assert_that(
    is.character(efo_trait),
    length(efo_trait) > 0,
    assertthat::noNA(efo_trait))

  resource_urls <- sprintf("%s%s", "/singleNucleotidePolymorphisms/search/findByEfoTrait?efoTrait=",
                           urltools::url_encode(tolower(efo_trait)))

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
#' Note: Right now there is a server-side bug and queries by
#' \code{reported_trait} are currently case sensitive but this should not be the
#' case.
#'
#' @param reported_trait A \code{character} vector of phenotypic traits as
#'   reported by the original authors' the study.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_by_reported_trait <- function(reported_trait = NULL, verbose = FALSE, warnings = TRUE, page_size = 1000L) {

  if(rlang::is_null(reported_trait))
    return(variants())

  assertthat::assert_that(
    is.character(reported_trait),
    length(reported_trait) > 0,
    assertthat::noNA(reported_trait))

  resource_urls <- sprintf("%s%s", "/singleNucleotidePolymorphisms/search/findByDiseaseTrait?diseaseTrait=",
                           urltools::url_encode(tolower(reported_trait)))

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
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#' @param page_size An \code{integer} scalar indicating the
#'   \href{https://www.ebi.ac.uk/gwas/rest/docs/api#_paging_resources}{page}
#'   value to be used in the JSON requests, can be between \code{1} and
#'   \code{1000}.
#'
#' @return A \linkS4class{variants} object.
#' @keywords internal
get_variants_all <- function(verbose = FALSE, warnings = TRUE, page_size = 1000L) {

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
#' @param study_id A \code{character} vector of GWAS Catalog study accession
#'   identifiers.
#' @param association_id An \code{integer} vector of GWAS Catalog association
#'   identifiers.
#' @param variant_id A \code{character} vector of GWAS Catalog variant identifiers.
#' @param efo_id A character vector of \href{https://www.ebi.ac.uk/efo/}{EFO}
#'   identifiers.
#' @param pubmed_id An \code{integer} vector of
#'   \href{https://en.wikipedia.org/wiki/PubMed}{PubMed} identifiers.
#' @param genomic_range A named \code{list} of three vectors:
#' \describe{ \item{chromosome}{A character vector of chromosome names of the
#' form 1--22, X or Y.} \item{start}{A numeric vector of start positions,
#' starting at 1.} \item{end}{A numeric vector of end positions.} }
#' The three vectors need to be of the same length so that \code{chromosome}
#' names, \code{start} and \code{end} positions are matched by position hence
#' defining a genomic range.
#' @param gene_name Gene symbol according to
#'   \href{https://www.genenames.org/}{HUGO Gene Nomenclature (HGNC)}.
#' @param efo_trait A \code{character} vector of
#'   \href{https://www.ebi.ac.uk/efo/}{EFO} trait descriptions, e.g.,
#'   \code{'uric acid measurement'}.
#' @param reported_trait A \code{character} vector of phenotypic traits as
#'   reported by the original authors' the study.
#' @param set_operation Either \code{'union'} or \code{'intersection'}. This
#'   tells how variants retrieved by different criteria  should be combined:
#'   \code{'union'} binds together all results removing duplicates and
#'   \code{'intersection'} only keeps same variants found with different
#'   criteria.
#' @param verbose A \code{logical} indicating whether the function should be
#'   verbose about the different queries or not.
#' @param warnings A \code{logical} indicating whether to print warnings, if any.
#'
#' @return A \linkS4class{variants} object.
#' @export
get_variants <- function(study_id = NULL,
                         association_id = NULL,
                         variant_id = NULL,
                         efo_id = NULL,
                         pubmed_id = NULL,
                         genomic_range = NULL,
                         gene_name = NULL,
                         efo_trait = NULL,
                         reported_trait = NULL,
                         set_operation = 'union',
                         verbose = FALSE,
                         warnings = TRUE) {


  if(!(rlang::is_scalar_character(set_operation) && set_operation %in% c('union', 'intersection')))
    stop("set_operation must be either 'union' or 'intersection'")

  if(!(rlang::is_scalar_logical(verbose) && verbose %in% c(TRUE, FALSE)))
    stop("verbose must be either TRUE or FALSE")

  if(!(rlang::is_scalar_logical(warnings) && warnings %in% c(TRUE, FALSE)))
    stop("warnings must be either TRUE or FALSE")

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
  # variants.
  msg <- "You are about to download all variants from the GWAS Catalog.
  This might take several hours..."
  if(rlang::is_empty(list_of_variants) && sure(msg))
    return(get_variants_all(verbose = verbose, warnings = warnings))

  if(identical(set_operation, "union")) {
    return(purrr::reduce(list_of_variants, union))
  }

  if(identical(set_operation, "intersection")) {
    return(purrr::reduce(list_of_variants, intersect))
  }

}
