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
                                            country_name = recode_missing(x$countryName),
                                            major_area = recode_missing(x$majorArea),
                                            region = recode_missing(x$region)))
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
                         ancestral_group = recode_missing(x$ancestralGroup)
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
    study_id = recode_missing(tws(obj$content$studies$accessionId)),
    # reported_trait = recode_missing(tws(obj$content$studies$diseaseTrait$trait)),
    reported_trait = tws(
      purrr::pluck(obj, 'content', 'studies', 'diseaseTrait', 'trait', .default = NA_character_)
    ),
    initial_sample_size = recode_missing(tws(obj$content$studies$initialSampleSize)),
    replication_sample_size = recode_missing(tws(obj$content$studies$replicationSampleSize)),
    gxe = recode_missing(tws(obj$content$studies$gxe), type = 'lgl'),
    gxg = recode_missing(tws(obj$content$studies$gxg), type = 'lgl'),
    snp_count = recode_missing(tws(obj$content$studies$snpCount), type = 'int'),
    qualifier = recode_missing(tws(obj$content$studies$qualifier)),
    imputed = recode_missing(tws(obj$content$studies$imputed), type = 'lgl'),
    pooled = recode_missing(tws(obj$content$studies$pooled), type = 'lgl'),
    study_design_comment = recode_missing(tws(obj$content$studies$studyDesignComment)),
    full_pvalue_set = recode_missing(tws(obj$content$studies$fullPvalueSet), type = 'lgl'),
    user_requested = recode_missing(tws(obj$content$studies$userRequested), type = 'lgl')
  ) %>% dplyr::distinct()

  # genotyping technologies table
  s@genotyping_techs <- purrr::map2_df(
    obj$content$studies$accessionId,
    obj$content$studies$genotypingTechnologies,
    ~ {
      if (rlang::is_empty(.y))
        return(genotyping_techs_tbl())
      genotyping_techs_tbl(study_id = .x,
                           genotyping_technology = recode_missing(tws(.y$genotypingTechnology)))
    }
  ) %>% dplyr::distinct()

  # platforms table
  s@platforms <- purrr::map2_df(obj$content$studies$accessionId,
                                obj$content$studies$platforms,
                                ~ {
                                  if (rlang::is_empty(.y))
                                    return(platforms_tbl())

                                  platforms_tbl(study_id = .x,
                                                manufacturer = recode_missing(tws(.y$manufacturer)))
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
                       number_of_individuals = recode_missing(tws(.y$numberOfIndividuals), type = 'int')
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
      study_id = recode_missing(tws(obj$content$studies$accessionId)),
      pubmed_id = recode_missing(tws(obj$content$studies$publicationInfo$pubmedId), type = 'int'),
      publication_date = lubridate::ymd(recode_missing(tws(obj$content$studies$publicationInfo$publicationDate))),
      publication = recode_missing(tws(obj$content$studies$publicationInfo$publication)),
      title = recode_missing(tws(obj$content$studies$publicationInfo$title)),
      author_fullname = recode_missing(tws(obj$content$studies$publicationInfo$author$fullname)),
      author_orcid = recode_missing(tws(obj$content$studies$publicationInfo$author$orcid))
    )
  } %>% dplyr::distinct()

  return(s)
}

