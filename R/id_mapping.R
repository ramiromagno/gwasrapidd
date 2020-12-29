#' Identifier mapping
#'
#' Use these functions to map between identifiers of studies, associations,
#' variants or traits. Each \code{association_id} maps to just one
#' \code{study_id}; all other mappings are one-to-many.
#'
#' @param study_id,association_id,variant_id,efo_id A character vector of
#'   accession identifiers for either studies (\code{study_id}), associations
#'   (\code{association_id}), variants (\code{variant_id}) or traits
#'   (\code{efo_id}).
#' @param keep_association_id In the case of the \code{variant_to_trait()}
#'   function, the mapping from variant ids to EFO traits ids is performed via
#'   association ids; this option allow you to keep the association identifier
#'   in the final output (default is \code{FALSE}).
#' @param verbose Whether the function should be
#'   verbose about the different queries or not.
#' @param warnings Whether to print warnings.
#' @return A dataframe of two identifiers (three if \code{keep_association_id}
#'   is \code{TRUE}). First column is the \emph{from} identifier and the second
#'   column is the \emph{to} identifier.
#'
#' @name identifier_mapping
NULL

#' @rdname identifier_mapping
#' @examples
#' \dontrun{study_to_association(c('GCST001084', 'GCST001085'))}
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

#' @rdname identifier_mapping
#' @examples
#' \dontrun{study_to_variant(c('GCST001084', 'GCST001085'))}
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

#' @rdname identifier_mapping
#' @examples
#' \dontrun{study_to_trait(c('GCST001084', 'GCST001085'))}
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

#' @rdname identifier_mapping
#' @examples
#' \dontrun{association_to_study(c('24300097', '24299759'))}
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

#' @rdname identifier_mapping
#' @examples
#' \dontrun{association_to_variant(c('24300097', '24299759'))}
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

#' @rdname identifier_mapping
#' @examples
#' \dontrun{association_to_trait(c('24300097', '24299759'))}
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

#' @rdname identifier_mapping
#' @examples
#' \dontrun{variant_to_study(c('rs7904579', 'rs138331350'))}
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

#' @rdname identifier_mapping
#' @examples
#' \dontrun{variant_to_association(c('rs7904579', 'rs138331350'))}
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

#' @importFrom rlang .data
#' @rdname identifier_mapping
#' @examples
#' \dontrun{variant_to_trait(c('rs7904579', 'rs138331350'))}
#' \dontrun{variant_to_trait(c('rs7904579', 'rs138331350'), keep_association_id = TRUE)}
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

#' @rdname identifier_mapping
#' @examples
#' \dontrun{trait_to_study(c('EFO_0005108', 'EFO_0005109'))}
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

#' @rdname identifier_mapping
#' @examples
#' \dontrun{trait_to_association(c('EFO_0005108', 'EFO_0005109'))}
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

#' @rdname identifier_mapping
#' @examples
#' \dontrun{trait_to_variant('EFO_0005229')}
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


