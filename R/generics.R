#' @include class-studies.R class-associations.R class-variants.R class-traits.R
NULL

# This is a function factory for methods that are binary endofunctions.
endofunction2 <- function(fn, obj_class) {
  function(x, y) {
    lst <- purrr::map2(s4_to_list(x), s4_to_list(y), fn)
    return(list_to_s4(lst, obj_class))
  }
}

# This is a function factory for methods that are binary predicate functions.
predicate2 <- function(fn) {
  function(x, y) {
    lst <- purrr::map2(s4_to_list(x), s4_to_list(y), fn)
    return(all(unlist(lst)))
  }
}

# This is a function factory for methods that are variadic endofunctions functions.
p_endofunction <- function(fn, obj_class) {
  # https://stackoverflow.com/questions/14679852/define-s4-method-with-3-dots
  function(x, ...) {
    s4_objs <- c(list(x), list(...))
    list_objs <- purrr::map(s4_objs, s4_to_list)
    list_obj <- purrr::pmap(list_objs, fn)
    return(list_to_s4(list_obj, obj_class))
  }
}

#' Set operations on GWAS Catalog objects
#'
#' Performs set union, intersection, and (asymmetric!) difference on two objects
#' of either class \linkS4class{studies}, \linkS4class{associations},
#' \linkS4class{variants}, or \linkS4class{traits}.
#'
#' @param x,y Objects of either class \linkS4class{studies}, \linkS4class{associations},
#' \linkS4class{variants}, or \linkS4class{traits}.
#' @param ... other arguments passed on to methods.
#'
#' @return An object of the same class as \code{x} and \code{y}, i.e., \linkS4class{studies}, \linkS4class{associations},
#' \linkS4class{variants}, or \linkS4class{traits}.
#' @name setop
NULL

#' @rdname setop
#' @importFrom dplyr union
#' @export
setGeneric('union', function(x, y) standardGeneric('union'))

#' @rdname setop
#' @importFrom dplyr intersect
#' @export
setGeneric('intersect', function(x, y) standardGeneric('intersect'))

#' @rdname setop
#' @importFrom dplyr setdiff
#' @export
setGeneric('setdiff', function(x, y) standardGeneric('setdiff'))

#' @rdname setop
#' @importFrom dplyr setequal
#' @export
setGeneric('setequal', function(x, y) standardGeneric('setequal'))


#' @export
setMethod("union",
          signature = c(x = "studies", y = "studies"),
          definition = endofunction2(dplyr::union, "studies"))

#' @export
setMethod("intersect",
          signature = c(x = "studies", y = "studies"),
          definition = endofunction2(dplyr::intersect, "studies"))

#' @export
setMethod("setdiff",
          signature = c(x = "studies", y = "studies"),
          definition = endofunction2(dplyr::setdiff, "studies"))

#' @export
setMethod("setequal",
          signature = c(x = "studies", y = "studies"),
          definition = predicate2(dplyr::setequal))

#' @export
setMethod("union",
          signature = c(x = "associations", y = "associations"),
          definition = endofunction2(dplyr::union, "associations"))

#' @export
setMethod("intersect",
          signature = c(x = "associations", y = "associations"),
          definition = endofunction2(dplyr::intersect, "associations"))

#' @export
setMethod("setdiff",
          signature = c(x = "associations", y = "associations"),
          definition = endofunction2(dplyr::setdiff, "associations"))

#' @export
setMethod("setequal",
          signature = c(x = "associations", y = "associations"),
          definition = predicate2(dplyr::setequal))

#' @export
setMethod("union",
          signature = c(x = "variants", y = "variants"),
          definition = endofunction2(dplyr::union, "variants"))

#' @export
setMethod("intersect",
          signature = c(x = "variants", y = "variants"),
          definition = endofunction2(dplyr::intersect, "variants"))

#' @export
setMethod("setdiff",
          signature = c(x = "variants", y = "variants"),
          definition = endofunction2(dplyr::setdiff, "variants"))

#' @export
setMethod("setequal",
          signature = c(x = "variants", y = "variants"),
          definition = predicate2(dplyr::setequal))

#' @export
setMethod("union",
          signature = c(x = "traits", y = "traits"),
          definition = endofunction2(dplyr::union, "traits"))

#' @export
setMethod("intersect",
          signature = c(x = "traits", y = "traits"),
          definition = endofunction2(dplyr::intersect, "traits"))

#' @export
setMethod("setdiff",
          signature = c(x = "traits", y = "traits"),
          definition = endofunction2(dplyr::setdiff, "traits"))

#' @export
setMethod("setequal",
          signature = c(x = "traits", y = "traits"),
          definition = predicate2(dplyr::setequal))

#' Bind GWAS Catalog objects
#'
#' Binds together GWAS Catalog objects of the same class.
#'
#' @param x An object of class: \linkS4class{studies},
#'   \linkS4class{associations}, \linkS4class{variants}, or
#'   \linkS4class{traits}.
#' @param ... Objects of the same class: either \linkS4class{studies}, \linkS4class{associations},
#' \linkS4class{variants}, or \linkS4class{traits}.
#'
#' @return An object of the same class as \code{x}, i.e., \linkS4class{studies}, \linkS4class{associations},
#' \linkS4class{variants}, or \linkS4class{traits}.
#' @export
setGeneric('bind', function(x, ...) standardGeneric('bind'))


#' @export
setMethod("bind",
          signature = "studies",
          definition = p_endofunction(dplyr::bind_rows, "studies"))

#' @export
setMethod("bind",
          signature = "associations",
          definition = p_endofunction(dplyr::bind_rows, "associations"))

#' @export
setMethod("bind",
          signature = "variants",
          definition = p_endofunction(dplyr::bind_rows, "variants"))

#' @export
setMethod("bind",
          signature = "traits",
          definition = p_endofunction(dplyr::bind_rows, "traits"))

#' Filter GWAS Catalog objects by Id
#'
#' Use \code{filter_by_id} to filter GWAS Catalog objects by their respective
#' \code{id}.
#'
#' @param x An object of class either \linkS4class{studies},
#'   \linkS4class{associations}, \linkS4class{variants}, or
#'   \linkS4class{traits}
#' @param id Id, e.g., \code{"GCST001374"} in the case of \linkS4class{studies},
#'   or an integer \code{24299710} if an association, or \code{"rs10910092"} if a
#'   variant, or \code{"EFO_0004761"}.
#'
#' @keywords internal
setGeneric("filter_by_id", function(x, id) standardGeneric('filter_by_id'))

#' @rdname filter_by_id
#' @keywords internal
setMethod("filter_by_id",
          signature(x = "studies", id = "character"),
          definition = function(x, id) {
            study_id <- rlang::expr(study_id)
            lst <- purrr::map(s4_to_list(x), ~ dplyr::filter(.x, !!study_id %in% id))
            y <- list_to_s4(lst, "studies")
            return(y)
          })

#' @rdname filter_by_id
#' @keywords internal
setMethod("filter_by_id",
          signature(x = "associations", id = "integer"),
          definition = function(x, id) {
            association_id <- rlang::expr(association_id)
            lst <- purrr::map(s4_to_list(x), ~ dplyr::filter(.x, !!association_id %in% id))
            y <- list_to_s4(lst, "associations")
            return(y)
          })

#' @rdname filter_by_id
#' @keywords internal
setMethod("filter_by_id",
          signature(x = "variants", id = "character"),
          definition = function(x, id) {
            variant_id <- rlang::expr(variant_id)
            lst <- purrr::map(s4_to_list(x), ~ dplyr::filter(.x, !!variant_id %in% id))
            y <- list_to_s4(lst, "variants")
            return(y)
          })

#' @rdname filter_by_id
#' @keywords internal
setMethod("filter_by_id",
          signature(x = "traits", id = "character"),
          definition = function(x, id) {
            efo_id <- rlang::expr(efo_id)
            lst <- purrr::map(s4_to_list(x), ~ dplyr::filter(.x, !!efo_id %in% id))
            y <- list_to_s4(lst, "traits")
            return(y)
          })

#' Subsetting GWAS Catalog objects
#'
#' This function allows you to subset \linkS4class{studies},
#' \linkS4class{associations}, \linkS4class{variants}, or \linkS4class{traits},
#' by their respective identifiers or by position.
#'
#'
#' @param x A \linkS4class{studies}, \linkS4class{associations},
#'   \linkS4class{variants}, or \linkS4class{traits} object.
#' @param i Position of the identifier or the name of the identifier itself.
#'
#' @return An object of the same class as \code{x}, i.e.,
#'   \linkS4class{studies}, \linkS4class{associations}, \linkS4class{variants},
#'   or \linkS4class{traits}.
#' @name subset
NULL


#' @rdname subset
#' @export
setMethod("[",
          signature(x = "studies", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i) x)

#' @rdname subset
#' @export
setMethod("[",
          signature(x = "studies", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i) {
            study_ids <- unique(x@studies$study_id)[i]
            filter_by_id(x, id = study_ids)
          })

#' @rdname subset
#' @export
setMethod("[",
          signature(x = "studies", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i) {
            filter_by_id(x, id = i)
          })

#' @rdname subset
#' @export
setMethod("[",
          signature(x = "associations", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i) x)

#' @rdname subset
#' @export
setMethod("[",
          signature(x = "associations", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i) {
            association_ids <- unique(x@associations$association_id)[i]
            filter_by_id(x, id = association_ids)
          })

#' @rdname subset
#' @export
setMethod("[",
          signature(x = "associations", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i) {
            filter_by_id(x, id = i)
          })

#' @rdname subset
#' @export
setMethod("[",
          signature(x = "variants", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i) x)

#' @rdname subset
#' @export
setMethod("[",
          signature(x = "variants", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i) {
            variant_ids <- unique(x@variants$variant_id)[i]
            filter_by_id(x, id = variant_ids)
          })

#' @rdname subset
#' @export
setMethod("[",
          signature(x = "variants", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i) {
            filter_by_id(x, id = i)
          })

#' @rdname subset
#' @export
setMethod("[",
          signature(x = "traits", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i) x)

#' @rdname subset
#' @export
setMethod("[",
          signature(x = "traits", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i) {
            efo_ids <- unique(x@traits$efo_id)[i]
            filter_by_id(x, id = efo_ids)
          })

#' @rdname subset
#' @export
setMethod("[",
          signature(x = "traits", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i) {
            filter_by_id(x, id = i)
          })

#' Number of studies
#'
#' This function returns the number of unique studies in a \linkS4class{studies} object.
#'
#' @param x A \linkS4class{studies} object.
#'
#' @return An integer scalar.
#'
#' @export
setGeneric('n_studies', function(x) standardGeneric('n_studies'))

#' Number of associations
#'
#' This function returns the number of unique associations in a \linkS4class{associations} object.
#'
#' @param x A \linkS4class{associations} object.
#'
#' @return An integer scalar.
#'
#' @export
setGeneric('n_associations', function(x) standardGeneric('n_associations'))

#' Number of variants
#'
#' This function returns the number of unique variants in a \linkS4class{variants} object.
#'
#' @param x A \linkS4class{variants} object.
#'
#' @return An integer scalar.
#'
#' @export
setGeneric('n_variants', function(x) standardGeneric('n_variants'))

#' Number of traits
#'
#' This function returns the number of unique traits in a \linkS4class{traits} object.
#'
#' @param x A \linkS4class{traits} object.
#'
#' @return An integer scalar.
#'
#' @export
setGeneric('n_traits', function(x) standardGeneric('n_traits'))

#' @rdname n_studies
#' @export
setMethod("n_studies",
          signature(x = "studies"),
          definition = function(x) {
            n <- dplyr::n_distinct(x@studies$study_id)
            return(n)
          })

#' @rdname n_associations
#' @export
setMethod("n_associations",
          signature(x = "associations"),
          definition = function(x) {
            n <- dplyr::n_distinct(x@associations$association_id)
            return(n)
          })

#' @rdname n_variants
#' @export
setMethod("n_variants",
          signature(x = "variants"),
          definition = function(x) {
            n <- dplyr::n_distinct(x@variants$variant_id)
            return(n)
          })

#' @rdname n_traits
#' @export
setMethod("n_traits",
          signature(x = "traits"),
          definition = function(x) {
            n <- dplyr::n_distinct(x@traits$efo_id)
            return(n)
          })
