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
  if(rlang::is_empty(v_obj)) return(v)

  v@variants <- v_obj_to_variants_tbl(v_obj)
  v@genomic_contexts <- v_obj_to_genomic_contexts_tbl(v_obj)
  v@ensembl_ids <- v_obj_to_ensembl_ids_tbl(v_obj)
  v@entrez_ids <- v_obj_to_entrez_ids_tbl(v_obj)

  return(v)
}

#' @keywords internal
v_obj_to_variants_tbl <- function(obj) {

  if(rlang::is_empty(obj)) return(variants_tbl())

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
      chromosome_name = missing_to_na(location_obj$chromosomeName),
      chromosome_position = missing_to_na(location_obj$chromosomePosition, NA_integer_),
      chromosome_region = missing_to_na(location_obj$region$name)
    )
    return(tbl)
  }

  with(obj,
       purrr::imap_dfr(rsId,
                       ~ {
                         loc <- obj_to_locations(locations[[.y]])
                         variants_tbl(
                           variant_id = missing_to_na(rsId[.y]),
                           merged = missing_to_na(merged[.y], na = NA_integer_),
                           functional_class = missing_to_na(functionalClass[.y]),
                           chromosome_name = missing_to_na(loc$chromosome_name),
                           chromosome_position = missing_to_na(loc$chromosome_position, na = NA_integer_),
                           chromosome_region = missing_to_na(loc$chromosome_region),
                           last_update_date = lubridate::ymd_hms(missing_to_na(lastUpdateDate[.y]))
                         )
                       }) %>% dplyr::distinct()
       )
}

#' @keywords internal
v_obj_to_genomic_contexts_tbl <- function(obj) {

  if(rlang::is_empty(obj)) return(genomic_contexts_tbl())

  with(obj,
       purrr::imap_dfr(rsId,
                       ~ {
                         if (rlang::is_empty(genomicContexts[[.y]])) {
                           genomic_contexts_tbl()
                         } else {
                           gc <- genomicContexts[[.y]]
                           genomic_contexts_tbl(
                             variant_id = missing_to_na(rsId[.y]),
                             gene_name = missing_to_na(gc$gene$geneName),
                             chromosome_name = missing_to_na(gc$location$chromosomeName),
                             chromosome_position = missing_to_na(gc$location$chromosomePosition, na = NA_integer_),
                             distance = missing_to_na(gc$distance, na = NA_integer_),
                             is_closest_gene = missing_to_na(gc$isClosestGene, na = NA),
                             is_intergenic = missing_to_na(gc$isIntergenic, na = NA),
                             is_upstream = missing_to_na(gc$isUpstream, na = NA),
                             is_downstream = missing_to_na(gc$isDownstream, na = NA),
                             source = missing_to_na(gc$source),
                             mapping_method = missing_to_na(gc$mappingMethod)
                           )
                         }
                       }) %>% dplyr::distinct())
}

#' @keywords internal
v_obj_to_ensembl_ids_tbl <- function(obj) {

  if(rlang::is_empty(obj)) return(v_ensembl_ids_tbl())

  obj_to_ensembl_id_tbl <- function(variant_id, gene_obj) {
    if (rlang::is_empty(gene_obj))
      return(v_ensembl_ids_tbl())

    tbl <-
      tibble::as_tibble(gene_obj[c("geneName", "ensemblGeneIds")]) %>%
      tidyr::unnest() %>% # This is no typo.
      tidyr::unnest()

    tbl2 <- v_ensembl_ids_tbl(
      variant_id = variant_id,
      gene_name = tbl$geneName,
      ensembl_id = unlist(tbl$ensemblGeneIds)
    )
    return(tbl2)
  }

  with(obj,
  purrr::imap_dfr(genomicContexts, # over variants
                  ~ {
                    if (rlang::is_empty(.x)) {
                      v_ensembl_ids_tbl()
                    } else {
                      obj_to_ensembl_id_tbl(variant_id = rsId[.y],
                                            gene_obj = .x$gene)
                    }
                  }) %>% dplyr::distinct()
  )

}

#' @keywords internal
v_obj_to_entrez_ids_tbl <- function(obj) {

  if(rlang::is_empty(obj)) return(v_entrez_ids_tbl())

  obj_to_entrez_id_tbl <- function(variant_id, gene_obj) {
    if (rlang::is_empty(gene_obj))
      return(v_entrez_ids_tbl())

    tbl <-
      tibble::as_tibble(gene_obj[c("geneName", "entrezGeneIds")]) %>%
      tidyr::unnest() %>% # This is no typo.
      tidyr::unnest()

    tbl2 <- v_entrez_ids_tbl(
      variant_id = variant_id,
      gene_name = tbl$geneName,
      entrez_id = unlist(tbl$entrezGeneIds)
    )
    return(tbl2)
  }

  with(obj,
       purrr::imap_dfr(genomicContexts, # over variants
                       ~ {
                         if (rlang::is_empty(.x))
                           return(v_entrez_ids_tbl())
                         obj_to_entrez_id_tbl(variant_id = rsId[.y],
                                              gene_obj = .x$gene)
                       }) %>% dplyr::distinct()
  )
}

