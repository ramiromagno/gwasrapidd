#' @include class-variants.R
NULL

#' @keywords internal
v_obj_to_variants <- function(obj) {

  # Instantiate a new variants S4 object.
  v <- variants()

  # a_obj: alias for obj$content$obj$content$singleNucleotidePolymorphisms
  v_obj <- obj$content$singleNucleotidePolymorphisms

  # If the object is empty return the variants S4 object as is, i.e., with its
  # tables empty.
  if (rlang::is_empty(v_obj)) return(v)

  v@variants <- v_obj_to_variants_tbl(v_obj)
  v@genomic_contexts <- v_obj_to_genomic_contexts_tbl(v_obj)
  v@ensembl_ids <- v_obj_to_ensembl_ids_tbl(v_obj)
  v@entrez_ids <- v_obj_to_entrez_ids_tbl(v_obj)

  return(v)
}

#' @keywords internal
v_obj_to_variants_tbl <- function(obj) {

  if (rlang::is_empty(obj)) return(variants_tbl())

  cols <- c("variant_id",
            "merged",
            "functional_class",
            "chromosome_name",
            "chromosome_position",
            "chromosome_region",
            "last_update_date")

  # If obj has some elements missing, add them by name and assign NULL to them
  # Later on missing_to_na will convert NULL to NA appropriately.
  obj[cols[!rlang::has_name(obj, cols)]] <- list(NULL)

  obj_to_locations <- function(location_obj) {
    tbl <- tibble::tibble(
      chromosome_name = recode_missing(tws(location_obj$chromosomeName)),
      chromosome_position = recode_missing(tws(location_obj$chromosomePosition), type = 'int'),
      chromosome_region = recode_missing(tws(location_obj$region$name))
    )
    return(tbl)
  }

  purrr::imap_dfr(obj$rsId,
                  ~ {
                    loc <- obj_to_locations(obj$locations[[.y]])
                    variants_tbl(
                      variant_id = recode_missing(tws(obj$rsId[.y])),
                      merged = recode_missing(tws(obj$merged[.y]), type = 'int'),
                      functional_class = recode_missing(tws(obj$functionalClass[.y])),
                      chromosome_name = recode_missing(tws(loc$chromosome_name)),
                      chromosome_position = recode_missing(tws(loc$chromosome_position), type = 'int'),
                      chromosome_region = recode_missing(tws(loc$chromosome_region)),
                      last_update_date = lubridate::ymd_hms(recode_missing(tws(
                        obj$lastUpdateDate[.y]
                      )))
                    )
                  }) %>% dplyr::distinct()
}

#' @keywords internal
is_mapped_gene <- function(is_closest_gene, is_intergenic, source) {
  is_mapped_gene1 <- !is_intergenic & source == 'Ensembl'
  is_mapped_gene2 <- is_intergenic & source == 'Ensembl' & is_closest_gene
  if (any(is_mapped_gene1)) return(is_mapped_gene1)
  else return(is_mapped_gene2)
}

#' @keywords internal
v_obj_to_genomic_contexts_tbl <- function(obj) {

  if (rlang::is_empty(obj)) return(genomic_contexts_tbl())

  purrr::imap_dfr(obj$rsId,
                  ~ {
                    if (rlang::is_empty(obj$genomicContexts[[.y]])) {
                      genomic_contexts_tbl()
                    } else {
                      gc <- obj$genomicContexts[[.y]]

                      is_closest_gene = recode_missing(tws(gc$isClosestGene), type = 'lgl')
                      is_intergenic = recode_missing(tws(gc$isIntergenic), type = 'lgl')
                      source = recode_missing(tws(gc$source))

                      genomic_contexts_tbl(
                        variant_id = recode_missing(tws(obj$rsId[.y])),
                        gene_name = recode_missing(tws(gc$gene$geneName)),
                        # Temporary hack, waiting for confirmation from GWAS Catalog team on whether this
                        # two variables have indeed been dropped.
                        # https://github.com/ramiromagno/gwasrapidd/issues/5
                        chromosome_name = recode_missing(tws(purrr::pluck(gc, 'location', 'chromosomeName', .default = NA_character_))),
                        chromosome_position = recode_missing(tws(purrr::pluck(gc, 'location', 'chromosomePosition', .default = NA_character_)), type = 'int'),
                        # chromosome_name = recode_missing(tws(gc$location$chromosomeName)),
                        # chromosome_position = recode_missing(tws(gc$location$chromosomePosition), type = 'int'),
                        distance = recode_missing(tws(gc$distance), type = 'int'),
                        is_mapped_gene = is_mapped_gene(is_closest_gene, is_intergenic, source),
                        is_closest_gene = is_closest_gene,
                        is_intergenic = is_intergenic,
                        is_upstream = recode_missing(tws(gc$isUpstream), type = 'lgl'),
                        is_downstream = recode_missing(tws(gc$isDownstream), type = 'lgl'),
                        source = source,
                        mapping_method = recode_missing(tws(gc$mappingMethod))
                      )
                    }
                  }) %>% dplyr::distinct()
}

#' @keywords internal
v_obj_to_ensembl_ids_tbl <- function(obj) {

  if (rlang::is_empty(obj)) return(v_ensembl_ids_tbl())

  obj_to_ensembl_id_tbl <- function(variant_id, gene_obj) {
    if (rlang::is_empty(gene_obj))
      return(v_ensembl_ids_tbl())

    tbl <-
      tibble::as_tibble(gene_obj[c("geneName", "ensemblGeneIds")]) %>%
      tidyr::unnest(cols = "ensemblGeneIds") %>% # This is no typo.
      tidyr::unnest(cols = "ensemblGeneIds")

    tbl2 <- v_ensembl_ids_tbl(
      variant_id = recode_missing(tws(variant_id)),
      gene_name = recode_missing(tws(tbl$geneName)),
      ensembl_id = recode_missing(tws(unlist(tbl$ensemblGeneIds)))
    )
    return(tbl2)
  }

  purrr::imap_dfr(obj$genomicContexts, # over variants
                  ~ {
                    if (rlang::is_empty(.x)) {
                      v_ensembl_ids_tbl()
                    } else {
                      obj_to_ensembl_id_tbl(variant_id = obj$rsId[.y],
                                            gene_obj = .x$gene)
                    }
                  }) %>% dplyr::distinct()


}

#' @keywords internal
v_obj_to_entrez_ids_tbl <- function(obj) {

  if (rlang::is_empty(obj)) return(v_entrez_ids_tbl())

  obj_to_entrez_id_tbl <- function(variant_id, gene_obj) {
    if (rlang::is_empty(gene_obj))
      return(v_entrez_ids_tbl())

    tbl <-
      tibble::as_tibble(gene_obj[c("geneName", "entrezGeneIds")]) %>%
      tidyr::unnest(cols = "entrezGeneIds")

    # Hack, need to come back to this again
    # Test with these SNPs: 'rs147903261' and 'rs267606894'.
    if (!identical(nrow(tbl), 0L))
      tbl <- tidyr::unnest(tbl, cols = 'entrezGeneIds')

    tbl2 <- v_entrez_ids_tbl(
      variant_id = recode_missing(tws(variant_id)),
      gene_name = recode_missing(tws(tbl$geneName)),
      entrez_id = recode_missing(tws(unlist(tbl$entrezGeneIds)))
    )
    return(tbl2)
  }


  purrr::imap_dfr(obj$genomicContexts, # over variants
                  ~ {
                    if (rlang::is_empty(.x))
                      return(v_entrez_ids_tbl())
                    obj_to_entrez_id_tbl(variant_id = obj$rsId[.y],
                                         gene_obj = .x$gene)
                  }) %>% dplyr::distinct()
}

#' Filter variants by standard human chromosomes.
#'
#' This function filters a \linkS4class{variants} object by standard human chromosomes, i.e.,
#' 1--22, X and Y. In addition to these chromosomes, some variants retrieved
#' from the GWAS Catalog might be also mapped to non-standard locations, such as
#' GRC assembly patches, haplotype (HAPs) or pseudo autosomal regions (PARs).
#' When this happens the main table \code{variants} includes rows for these
#' cases too. This function removes these.
#'
#' @param s4_variants An object of class \linkS4class{variants}.
#' @param chromosomes A character vector of valid chromosome names. Default is
#'   autosomal chromosomes 1 thru 22 and, X, Y, and MT.
#' @return An object of class \linkS4class{variants}.
#' @keywords internal
filter_variants_by_standard_chromosomes <- function(s4_variants, chromosomes = c(seq_len(22), "X", "Y", "MT")) {

  not_valid_chr_name_lgl <- !is_human_chromosome(chromosomes)
  if (any(not_valid_chr_name_lgl))
    stop(
      "These are not valid chromosome names: ",
      concatenate::cc_and(chromosomes[not_valid_chr_name_lgl], oxford = TRUE),
      "."
    )

  chromosome_name <- rlang::expr(chromosome_name)
  s4_variants@variants <- dplyr::filter(s4_variants@variants, !!chromosome_name %in% c(chromosomes, NA_character_))
  variant_ids <- s4_variants@variants$variant_id

  # Filter variants by variant_ids
  return(s4_variants[variant_ids])
}
