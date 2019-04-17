#' @include class-associations.R
NULL

#' @keywords internal
a_obj_to_associations <- function(obj) {

  # Instantiate a new associations S4 object.
  a <- associations()

  # a_obj: alias for obj$content$associations
  a_obj <- obj$content$associations

  # If the object is empty return the associations S4 object as is, i.e., with its
  # tables empty.
  if(rlang::is_empty(a_obj)) return(a)

  # Ironically, association identifiers are not part of any variable in the JSON
  # response for associations, except in the URLs, so there is where we go for
  # the identifiers.
  association_ids <- extract_association_id(a_obj$`_links`$self$href)

  a@associations <- a_obj_to_associations_tbl(association_ids, a_obj)
  a@loci <- a_obj_to_loci_tbl(association_ids, a_obj)
  a@risk_alleles <- a_obj_to_risk_alleles_tbl(association_ids, a_obj)
  a@genes <- a_obj_to_genes_tbl(association_ids, a_obj)
  a@ensembl_ids <- a_obj_to_ensembl_ids_tbl(association_ids, a_obj)
  a@entrez_ids <- a_obj_to_entrez_ids_tbl(association_ids, a_obj)

  return(a)
}

#' @keywords internal
a_obj_to_associations_tbl <- function(association_ids, obj) {

  if(rlang::is_empty(obj)) return(associations_tbl())

  cols <- c("pvalue",
            "pvalueDescription",
            "pvalueMantissa",
            "pvalueExponent",
            "multiSnpHaplotype",
            "snpInteraction",
            "snpType",
            "standardError",
            "range",
            "orPerCopyNum",
            "betaNum",
            "betaUnit",
            "betaDirection",
            "description",
            "lastMappingDate",
            "lastUpdateDate")

  # If obj has some elements missing, add them by name and assign NULL to them
  # Later on missing_to_na will convert NULL to NA appropriately.
  obj[cols[!rlang::has_name(obj, cols)]] <- list(NULL)

  with(
    obj,
    associations_tbl(
      association_id = missing_to_na(association_ids, na = NA_integer_),
      pvalue = missing_to_na(pvalue),
      pvalue_description = missing_to_na(pvalueDescription),
      pvalue_mantissa = missing_to_na(pvalueMantissa, na = NA_integer_),
      pvalue_exponent = missing_to_na(pvalueExponent, na = NA_integer_),
      multiple_snp_haplotype = missing_to_na(multiSnpHaplotype),
      snp_interaction = missing_to_na(snpInteraction),
      snp_type = missing_to_na(snpType),
      standard_error = missing_to_na(standardError, na = NA_real_),
      range = missing_to_na(range),
      or_per_copy_number = missing_to_na(orPerCopyNum, na = NA_real_),
      beta_number = missing_to_na(betaNum, na = NA_real_),
      beta_unit = missing_to_na(betaUnit),
      beta_direction = missing_to_na(betaDirection),
      beta_description = missing_to_na(description),
      last_mapping_date = lubridate::ymd_hms(missing_to_na(lastMappingDate)),
      last_update_date = lubridate::ymd_hms(missing_to_na(lastUpdateDate))
    )
  )
}

#' @keywords internal
a_obj_to_loci_tbl <- function(association_ids, obj) {

  if(rlang::is_empty(obj)) return(loci_tbl())

  n_associations <- length(association_ids)
  n_loci_objs <- length(obj$loci) # This length should be matching n_associations
                                  # Note that it is not the number of loci.

  if(!identical(n_associations, n_loci_objs))
    stop("Error parsing the loci object: number of associations does not match number of loci objs.")

  purrr::map2_df(association_ids,
                 obj$loci,
                 ~ {
                   if (rlang::is_empty(.y)) {
                     # if loci obj is empty
                     loci_tbl(association_id = .x)
                     }
                   else {
                     # equal_length: Automagically grows shortest vector; fills with NAs. (preemptive code)
                     l <- equal_length(list(description = .y$description, haplotype_snp_count = .y$haplotypeSnpCount))
                     loci_tbl(
                       association_id = .x,
                       locus_id = seq_along(l$description),
                       haplotype_snp_count = missing_to_na(l$haplotype_snp_count, na = NA_integer_),
                       description = missing_to_na(l$description)
                     )
                   }
                 })
}

#' @keywords internal
a_obj_to_risk_alleles_tbl <- function(association_ids, obj) {

  if(rlang::is_empty(obj)) return(risk_alleles_tbl())

  n_associations <- length(association_ids)
  n_loci_objs <- length(obj$loci) # This length should be matching n_associations
  # Note that it is not the number of loci.

  if(!identical(n_associations, n_loci_objs))
    stop("Error parsing the loci object: number of associations does not match number of loci objs.")

  purrr::map2_df(association_ids,
                 obj$loci,
                 ~ {
                   association_id = .x
                   if (rlang::is_empty(.y)) {
                     # if loci obj is empty
                     risk_alleles_tbl(association_id = association_id)
                   } else {
                     purrr::imap(.y$strongestRiskAlleles,
                                 ~ {
                                   risk_alleles_tbl(
                                     association_id = association_id,
                                     locus_id = .y,
                                     variant_id = missing_to_na(variant_name(.x$riskAlleleName)),
                                     risk_allele = missing_to_na(allele_name(.x$riskAlleleName)),
                                     risk_frequency = missing_to_na(.x$riskFrequency, na = NA_real_),
                                     genome_wide = missing_to_na(.x$genomeWide, na = NA),
                                     limited_list = missing_to_na(.x$limitedList, na = NA)
                                   )
                                 }) %>% dplyr::bind_rows()
                   }
                 })

}


#' @keywords internal
a_obj_to_genes_tbl <- function(association_ids, obj) {
  if (rlang::is_empty(obj))
    return(reported_genes_tbl())

  n_associations <- length(association_ids)
  n_loci_objs <-
    length(obj$loci) # This length should be matching n_associations
  # Note that it is not the number of loci.

  if (!identical(n_associations, n_loci_objs))
    stop(
      "Error parsing the loci object: number of associations does not match number of loci objs."
    )

  purrr::imap_dfr(obj$loci, ~ {
    association_id <- association_ids[[.y]]
    purrr::imap_dfr(.x$authorReportedGenes, ~ {
      if (rlang::is_empty(.x))
        return(reported_genes_tbl())
      else{
        locus_id <- .y
        reported_genes_tbl(
          association_id = association_id,
          locus_id = locus_id,
          gene_name = .x$geneName
        )
      }
    })
  })
}

#' @keywords internal
a_obj_ensembl_ids <- function(gene_obj) {
  if (rlang::is_empty(gene_obj))
    return(tibble::tibble(gene_name = character(), ensembl_id = character()))

  purrr::imap_dfr(gene_obj$geneName,
                  ~ {
                    ensembl_id <- gene_obj$ensemblGeneIds[[.y]]
                    if (rlang::is_empty(ensembl_id))
                      ensembl_id <- NA_character_
                    else
                      ensembl_id <- ensembl_id$ensemblGeneId

                    tibble::tibble(gene_name = .x, ensembl_id = ensembl_id)
                  })
}

#' @keywords internal
a_obj_to_ensembl_ids_tbl <- function(association_ids, obj) {

  if(rlang::is_empty(obj)) return(ensembl_ids_tbl())

  n_associations <- length(association_ids)
  n_loci_objs <- length(obj$loci) # This length should be matching n_associations
  # Note that it is not the number of loci.

  if(!identical(n_associations, n_loci_objs))
    stop("Error parsing the loci object: number of associations does not match number of loci objs.")

  purrr::imap_dfr(obj$loci, ~ {
    association_id <- association_ids[[.y]]
    purrr::imap_dfr(.x$authorReportedGenes, ~ {
      locus_id <- .y
      gene_tbl <- a_obj_ensembl_ids(gene_obj = .x)
      ensembl_ids_tbl(
        association_id = association_id,
        locus_id = locus_id,
        gene_name = gene_tbl$gene_name,
        ensembl_id = gene_tbl$ensembl_id)
    })
  })
}

#' @keywords internal
a_obj_entrez_ids <- function(gene_obj) {
  if (rlang::is_empty(gene_obj))
    return(tibble::tibble(gene_name = character(), entrez_id = character()))

  purrr::imap_dfr(gene_obj$geneName,
                  ~ {
                    entrez_id <- gene_obj$entrezGeneIds[[.y]]
                    if (rlang::is_empty(entrez_id))
                      entrez_id <- NA_character_
                    else
                      entrez_id <- entrez_id$entrezGeneId

                    tibble::tibble(gene_name = .x, entrez_id = entrez_id)
                  })
}

#' @keywords internal
a_obj_to_entrez_ids_tbl <- function(association_ids, obj) {

  if(rlang::is_empty(obj)) return(entrez_ids_tbl())

  n_associations <- length(association_ids)
  n_loci_objs <- length(obj$loci) # This length should be matching n_associations
  # Note that it is not the number of loci.

  if(!identical(n_associations, n_loci_objs))
    stop("Error parsing the loci object: number of associations does not match number of loci objs.")

  purrr::imap_dfr(obj$loci, ~ {
    association_id <- association_ids[[.y]]
    purrr::imap_dfr(.x$authorReportedGenes, ~ {
      locus_id <- .y
      gene_tbl <- a_obj_entrez_ids(gene_obj = .x)
      entrez_ids_tbl(
        association_id = association_id,
        locus_id = locus_id,
        gene_name = gene_tbl$gene_name,
        entrez_id = gene_tbl$entrez_id)
    })
  })
}
