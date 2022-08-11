#' Map a study id to an association id
#'
#' Map a study accession identifier to an association accession identifier.
#'
#' @param study_id A character vector of study accession identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A dataframe of two identifiers. First column is the study identifier
#'   and the second column is the association identifier.
#'
#' @examples
#' \dontrun{
#' # Map GWAS study identifiers to association identifiers
#' study_to_association(c('GCST001084', 'GCST001085'))
#' }
#'
#' @export
study_to_association <- function(study_id, verbose = FALSE, warnings = TRUE) {

  empty <- tibble::tibble(study_id = character(),
                          association_id = character()
                          )

  if(rlang::is_null(study_id)) return(empty)

  assertthat::assert_that(
    is.character(study_id),
    length(study_id) > 0,
    assertthat::noNA(study_id),
    all(is_study_id(study_id)))

  names(study_id) <- study_id

  purrr::map_dfr(
    study_id,
    ~ get_associations(
      study_id = .x,
      verbose = verbose,
      warnings = warnings
    )@associations['association_id'],
    .id = 'study_id'
  )

}

#' Map a study id to a variant id
#'
#' Map a study accession identifier to a variant accession identifier.
#'
#' @param study_id A character vector of study accession identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A dataframe of two identifiers. First column is the study identifier
#'   and the second column is the variant identifier.
#'
#' @examples
#' \dontrun{
#' # Map GWAS study identifiers to variant identifiers
#' study_to_variant(c('GCST001084', 'GCST001085'))
#' }
#'
#' @export
study_to_variant <- function(study_id, verbose = FALSE, warnings = TRUE) {

  empty <- tibble::tibble(study_id = character(),
                          variant_id = character()
  )

  if(rlang::is_null(study_id)) return(empty)

  assertthat::assert_that(
    is.character(study_id),
    length(study_id) > 0,
    assertthat::noNA(study_id),
    all(is_study_id(study_id)))

  names(study_id) <- study_id

  purrr::map_dfr(
    study_id,
    ~ get_variants(
      study_id = .x,
      verbose = verbose,
      warnings = warnings
    )@variants['variant_id'],
    .id = 'study_id'
  )
}

#' Map a study id to a EFO trait id
#'
#' Map a study accession identifier to a EFO trait identifier.
#'
#' @param study_id A character vector of study accession identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A dataframe of two identifiers. First column is the study identifier
#'   and the second column is the EFO identifier.
#'
#' @examples
#' \dontrun{
#' # Map GWAS study identifiers to EFO trait identifiers
#' study_to_trait(c('GCST001084', 'GCST001085'))
#' }
#'
#' @export
study_to_trait <- function(study_id, verbose = FALSE, warnings = TRUE) {

  empty <- tibble::tibble(study_id = character(),
                          efo_id = character()
  )

  if(rlang::is_null(study_id)) return(empty)

  assertthat::assert_that(
    is.character(study_id),
    length(study_id) > 0,
    assertthat::noNA(study_id),
    all(is_study_id(study_id)))

  names(study_id) <- study_id

  purrr::map_dfr(
    study_id,
    ~ get_traits(
      study_id = .x,
      verbose = verbose,
      warnings = warnings
    )@traits['efo_id'],
    .id = 'study_id'
  )
}

#' Map an association id to a study id
#'
#' Map an association accession identifier to a study accession identifier.
#'
#' @param association_id A character vector of association accession
#'   identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A dataframe of two identifiers. First column is the association
#'   identifier and the second column is the study identifier.
#'
#' @examples
#' \dontrun{
#' # Map GWAS association identifiers to study identifiers
#' association_to_study(c('24300097', '24299759'))
#' }
#'
#' @export
association_to_study <- function(association_id, verbose = FALSE, warnings = TRUE) {

  empty <- tibble::tibble(association_id = character(),
                          study_id = character()
  )

  if(rlang::is_null(association_id)) return(empty)

  assertthat::assert_that(
    is.character(association_id),
    length(association_id) > 0,
    assertthat::noNA(association_id),
    all(is_association_id(association_id)))

  names(association_id) <- association_id

  purrr::map_dfr(
    association_id,
    ~ get_studies(
      association_id = .x,
      verbose = verbose,
      warnings = warnings
    )@studies['study_id'],
    .id = 'association_id'
  )

}

#' Map an association id to a variant id
#'
#' Map an association accession identifier to a variant identifier.
#'
#' @param association_id A character vector of association accession
#'   identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A dataframe of two identifiers. First column is the association
#'   identifier and the second column is the variant identifier.
#'
#' @examples
#' \dontrun{
#' # Map GWAS association identifiers to variant identifiers
#' association_to_variant(c('24300097', '24299759'))
#' }
#'
#' @export
association_to_variant <- function(association_id, verbose = FALSE, warnings = TRUE) {

  empty <- tibble::tibble(association_id = character(),
                          variant_id = character()
  )

  if(rlang::is_null(association_id)) return(empty)

  assertthat::assert_that(
    is.character(association_id),
    length(association_id) > 0,
    assertthat::noNA(association_id),
    all(is_association_id(association_id)))

  names(association_id) <- association_id

  purrr::map_dfr(
    association_id,
    ~ get_variants(
      association_id = .x,
      verbose = verbose,
      warnings = warnings
    )@variants['variant_id'],
    .id = 'association_id'
  )
}

#' Map an association id to an EFO trait id
#'
#' Map an association accession identifier to an EFO trait id.
#'
#' @param association_id A character vector of association accession
#'   identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A dataframe of two identifiers. First column is the association
#'   identifier and the second column is the EFO trait identifier.
#'
#' @examples
#' \dontrun{
#' # Map GWAS association identifiers to EFO trait identifiers
#' association_to_trait(c('24300097', '24299759'))
#' }
#'
#' @export
association_to_trait <- function(association_id, verbose = FALSE, warnings = TRUE) {

  empty <- tibble::tibble(association_id = character(),
                          efo_id = character()
  )

  if(rlang::is_null(association_id)) return(empty)

  assertthat::assert_that(
    is.character(association_id),
    length(association_id) > 0,
    assertthat::noNA(association_id),
    all(is_association_id(association_id)))

  names(association_id) <- association_id

  purrr::map_dfr(
    association_id,
    ~ get_traits(
      association_id = .x,
      verbose = verbose,
      warnings = warnings
    )@traits['efo_id'],
    .id = 'association_id'
  )
}

#' Map a variant id to a study id
#'
#' Map a variant identifier to a study accession identifier.
#'
#' @param variant_id A character vector of variant identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A dataframe of two identifiers. First column is the variant
#'   identifier and the second column is the study identifier.
#'
#' @examples
#' \dontrun{
#' # Map GWAS variant identifiers to study identifiers
#' variant_to_study(c('rs7904579', 'rs138331350'))
#' }
#'
#' @export
variant_to_study <- function(variant_id, verbose = FALSE, warnings = TRUE) {

  empty <- tibble::tibble(variant_id = character(),
                          study_id = character()
  )

  if(rlang::is_null(variant_id)) return(empty)

  assertthat::assert_that(
    is.character(variant_id),
    length(variant_id) > 0,
    assertthat::noNA(variant_id))

  names(variant_id) <- variant_id

  purrr::map_dfr(
    variant_id,
    ~ get_studies(
      variant_id = .x,
      verbose = verbose,
      warnings = warnings
    )@studies['study_id'],
    .id = 'variant_id'
  )

}

#' Map a variant id to an association id
#'
#' Map a variant identifier to an association identifier.
#'
#' @param variant_id A character vector of variant identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A dataframe of two identifiers. First column is the variant
#'   identifier and the second column is the association identifier.
#'
#' @examples
#' \dontrun{
#' # Map GWAS variant identifiers to association identifiers
#' variant_to_association(c('rs7904579', 'rs138331350'))
#' }
#'
#' @export
variant_to_association <- function(variant_id, verbose = FALSE, warnings = TRUE) {

  empty <- tibble::tibble(variant_id = character(),
                          association_id = character()
  )

  if(rlang::is_null(variant_id)) return(empty)

  assertthat::assert_that(
    is.character(variant_id),
    length(variant_id) > 0,
    assertthat::noNA(variant_id))

  names(variant_id) <- variant_id

  purrr::map_dfr(
    variant_id,
    ~ get_associations(
      variant_id = .x,
      verbose = verbose,
      warnings = warnings
    )@associations['association_id'],
    .id = 'variant_id'
  )

}

#' Map a variant id to an EFO trait
#'
#' Map a variant identifier to an EFO trait identifier. Variants are first
#' mapped to association identifiers, and then to EFO traits. Set the option
#' \code{keep_association_id} to \code{TRUE} to keep the intermediate mapping,
#' i.e., the association identifiers.
#'
#' @param variant_id A character vector of variant identifiers.
#' @param keep_association_id  Whether to keep the association identifier
#'   in the final output (default is \code{FALSE}).
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A dataframe of two or three identifiers. If
#'   \code{keep_association_id} is set to \code{FALSE}, the first column is the
#'   variant identifier and the second column is the EFO trait identifier,
#'   otherwise the variable \code{association_id} is also included as the second
#'   column.
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' # Map GWAS variant identifiers to EFO trait identifiers
#' variant_to_trait(c('rs7904579', 'rs138331350'))
#'
#' # Map GWAS variant identifiers to EFO trait identifiers
#' # but keep the intermediate association identifier
#' variant_to_trait(c('rs7904579', 'rs138331350'), keep_association_id = TRUE)
#' }
#'
#' @export
variant_to_trait <- function(variant_id, keep_association_id = FALSE, verbose = FALSE, warnings = TRUE) {

  empty <- `if`(keep_association_id,
                tibble::tibble(variant_id = character(),
                               association_id = character(),
                               efo_id = character()),
                tibble::tibble(variant_id = character(),
                               efo_id = character())
                )

  if(rlang::is_null(variant_id)) return(empty)

  assertthat::assert_that(
    is.character(variant_id),
    length(variant_id) > 0,
    assertthat::noNA(variant_id))

  # Query flow: variant_id -> association_id -> efo_id

  # Description:
  #   From variant ids we get association ids, then we go for the traits from
  #   those association ids. Note: the GWAS Catalog API does not provide a
  #   direct endpoint for getting traits ids from variant ids, that's why we go
  #   via association ids.

  var2assoc <- variant_to_association(variant_id, verbose = verbose, warnings = warnings)
  if(nrow(var2assoc) == 0) return(empty)

  assoc2trait <- dplyr::distinct(association_to_trait(var2assoc[['association_id']], verbose = verbose, warnings = warnings))

  if(keep_association_id)
    dplyr::left_join(var2assoc, assoc2trait, by = 'association_id')
  else
    dplyr::left_join(var2assoc, assoc2trait, by = 'association_id') %>%
    dplyr::select(-.data$association_id) # https://dplyr.tidyverse.org/articles/programming.html#eliminating-r-cmd-check-notes-1

}

#' Map an EFO trait id to a study id
#'
#' Map an EFO trait id to a study accession identifier.
#'
#' @param efo_id A character vector of EFO trait identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A dataframe of two identifiers. First column is the EFO trait
#'   identifier and the second column is the study identifier.
#'
#' @examples
#' \dontrun{
#' # Map EFO trait identifiers to study identifiers
#' trait_to_study(c('EFO_0005108', 'EFO_0005109'))
#' }
#'
#' @export
trait_to_study <- function(efo_id, verbose = FALSE, warnings = TRUE) {

  empty <- tibble::tibble(efo_id = character(),
                          study_id = character()
  )

  if(rlang::is_null(efo_id)) return(empty)

  assertthat::assert_that(
    is.character(efo_id),
    length(efo_id) > 0,
    assertthat::noNA(efo_id),
    all(is_efo_id2(efo_id)))

  names(efo_id) <- efo_id

  purrr::map_dfr(
    efo_id,
    ~ get_studies(
      efo_id = .x,
      verbose = verbose,
      warnings = warnings
    )@studies['study_id'],
    .id = 'efo_id'
  )
}

#' Map an EFO trait id to an association id
#'
#' Map an EFO trait id to an association identifier.
#'
#' @param efo_id A character vector of EFO trait identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A dataframe of two identifiers. First column is the EFO trait
#'   identifier and the second column is the association identifier.
#'
#' @examples
#' \dontrun{
#' # Map EFO trait identifiers to association identifiers
#' trait_to_association(c('EFO_0005108', 'EFO_0005109'))
#' }
#'
#' @export
trait_to_association <- function(efo_id, verbose = FALSE, warnings = TRUE) {

  empty <- tibble::tibble(efo_id = character(),
                          association_id = character()
  )

  if(rlang::is_null(efo_id)) return(empty)

  assertthat::assert_that(
    is.character(efo_id),
    length(efo_id) > 0,
    assertthat::noNA(efo_id),
    all(is_efo_id2(efo_id)))

  names(efo_id) <- efo_id

  purrr::map_dfr(
    efo_id,
    ~ get_associations(
      efo_id = .x,
      verbose = verbose,
      warnings = warnings
    )@associations['association_id'],
    .id = 'efo_id'
  )

}

#' Map an EFO trait id to a variant id
#'
#' Map an EFO trait id to a variant identifier.
#'
#' @param efo_id A character vector of EFO trait identifiers.
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#'
#' @return A dataframe of two identifiers. First column is the EFO trait
#'   identifier and the second column is the variant identifier.
#'
#' @examples
#' \dontrun{
#' # Map EFO trait identifiers to variant identifiers
#' trait_to_variant('EFO_0005229')
#' }
#'
#' @export
trait_to_variant <- function(efo_id, verbose = FALSE, warnings = TRUE) {

  empty <- tibble::tibble(efo_id = character(),
                          variant_id = character()
  )

  if(rlang::is_null(efo_id)) return(empty)

  assertthat::assert_that(
    is.character(efo_id),
    length(efo_id) > 0,
    assertthat::noNA(efo_id),
    all(is_efo_id2(efo_id)))

  names(efo_id) <- efo_id

  purrr::map_dfr(
    efo_id,
    ~ get_variants(
      efo_id = .x,
      verbose = verbose,
      warnings = warnings
    )@variants['variant_id'],
    .id = 'efo_id'
  )
}


