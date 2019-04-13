#' @include class-studies.R
NULL


#' @keywords internal
obj_to_countries <- function(study_id, ancestries, countries) {
  a <- ancestries[[countries]]

  if(rlang::is_empty(a))
    return(countries_tbl())

  a <- purrr::imap(a,
                   .f = function(x, y) {
                     if(rlang::is_empty(x))
                       return(countries_tbl())
                     else
                       return(countries_tbl(study_id = study_id,
                                            ancestry_id = y,
                                            country_name = missing_to_na(x$countryName),
                                            major_area = missing_to_na(x$majorArea),
                                            region = missing_to_na(x$region)))
                   })

  tbl <- dplyr::bind_rows(a)
  return(tbl)
}

#' @keywords internal
obj_to_ancestral_groups <- function(study_id, ancestries) {
  a <- ancestries[["ancestralGroups"]]

  if(rlang::is_empty(a))
    return(ancestral_groups_tbl())

  a <- purrr::imap(a,
                   .f = function(x, y) {
                     if(rlang::is_empty(x))
                       return(ancestral_groups_tbl())
                     else
                       return(ancestral_groups_tbl(
                         study_id = study_id,
                         ancestry_id = y,
                         ancestral_group = missing_to_na(x$ancestralGroup)
                       ))
                   })

  tbl <- dplyr::bind_rows(a)
  return(tbl)
}

#' @keywords internal
obj_to_studies <- function(obj) {

  s <- studies()

  # studies table
  s@studies <- studies_tbl(
    study_id = missing_to_na(obj$content$studies$accessionId),
    reported_trait = missing_to_na(obj$content$studies$diseaseTrait$trait),
    initial_sample_size = missing_to_na(obj$content$studies$initialSampleSize),
    replication_sample_size = missing_to_na(obj$content$studies$replicationSampleSize),
    gxe = missing_to_na(obj$content$studies$gxe, na = NA),
    gxg = missing_to_na(obj$content$studies$gxg, na = NA),
    snp_count = missing_to_na(obj$content$studies$snpCount, na = NA_integer_),
    qualifier = missing_to_na(obj$content$studies$qualifier),
    imputed = missing_to_na(obj$content$studies$imputed, na = NA),
    pooled = missing_to_na(obj$content$studies$pooled, na = NA),
    study_design_comment = missing_to_na(obj$content$studies$studyDesignComment),
    full_pvalue_set = missing_to_na(obj$content$studies$fullPvalueSet, na = NA),
    user_requested = missing_to_na(obj$content$studies$userRequested, na = NA)
  ) %>% dplyr::distinct()

  # genotyping technologies table
  s@genotyping_techs <- purrr::map2_df(
    obj$content$studies$accessionId,
    obj$content$studies$genotypingTechnologies,
    ~ {
      if (rlang::is_empty(.y))
        return(genotyping_techs_tbl())
      genotyping_techs_tbl(study_id = .x,
                           genotyping_technology = missing_to_na(.y$genotypingTechnology))
    }
  ) %>% dplyr::distinct()

  # platforms table
  s@platforms <- purrr::map2_df(obj$content$studies$accessionId,
                                obj$content$studies$platforms,
                                ~ {
                                  if (rlang::is_empty(.y))
                                    return(platforms_tbl())

                                  platforms_tbl(study_id = .x,
                                                manufacturer = missing_to_na(.y$manufacturer))
                                }) %>% dplyr::distinct()

  # ancentries table
  s@ancestries <-
    purrr::map2_df(obj$content$studies$accessionId,
                   obj$content$studies$ancestries,
                   ~ {
                     if (rlang::is_empty(.y))
                       return(ancestries_tbl())

                     ancestries_tbl(
                       study_id = .x,
                       ancestry_id = seq_along(.y$type),
                       type = .y$type,
                       number_of_individuals = .y$numberOfIndividuals
                     )
                   }) %>% dplyr::distinct()

  # ancestral groups table
  s@ancestral_groups <- purrr::map2_df(
    obj$content$studies$accessionId,
    obj$content$studies$ancestries,
    ~ obj_to_ancestral_groups(.x, .y)
  ) %>% dplyr::distinct()

  # countries of origin table
  s@countries_of_origin <- purrr::map2_df(
    obj$content$studies$accessionId,
    obj$content$studies$ancestries,
    ~ obj_to_countries(.x, .y, countries = "countryOfOrigin")
  ) %>% dplyr::distinct()

  # countries of recruitment table
  s@countries_of_recruitment <- purrr::map2_df(
    obj$content$studies$accessionId,
    obj$content$studies$ancestries,
    ~ obj_to_countries(.x, .y, countries = "countryOfRecruitment")
  ) %>% dplyr::distinct()

  # publications table
  s@publications <- {
    if(rlang::is_empty(obj$content$studies$publicationInfo)) return(publications_tbl())
    publications_tbl(
      study_id = missing_to_na(obj$content$studies$accessionId),
      pubmed_id = missing_to_na(obj$content$studies$publicationInfo$pubmedId, na = NA_integer_),
      publication_date = lubridate::ymd(missing_to_na(obj$content$studies$publicationInfo$publicationDate)),
      publication = missing_to_na(obj$content$studies$publicationInfo$publication),
      title = missing_to_na(obj$content$studies$publicationInfo$title),
      author_fullname = missing_to_na(obj$content$studies$publicationInfo$author$fullname),
      author_orcid = missing_to_na(obj$content$studies$publicationInfo$author$orcid)
    )
  } %>% dplyr::distinct()

  return(s)
}

