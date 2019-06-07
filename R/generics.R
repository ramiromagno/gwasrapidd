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

# This is a function factory for methods that are variadic endofunctions
# functions.
p_endofunction <- function(fn, obj_class) {
  # https://stackoverflow.com/questions/14679852/define-s4-method-with-3-dots
  function(x, ...) {
    s4_objs <- c(list(x), list(...))
    list_objs <- purrr::map(s4_objs, s4_to_list)
    list_obj <- purrr::pmap(list_objs, fn)
    return(list_to_s4(list_obj, obj_class))
  }
}

#' Set operations on GWAS Catalog objects.
#'
#' Performs set union, intersection, and (asymmetric!) difference on two objects
#' of either class \linkS4class{studies}, \linkS4class{associations},
#' \linkS4class{variants}, or \linkS4class{traits}. Note that \code{union()}
#' removes duplicated entities, whereas \code{\link[gwasrapidd]{bind}()} does
#' not.
#'
#' @param x,y Objects of either class \linkS4class{studies},
#'   \linkS4class{associations}, \linkS4class{variants}, or
#'   \linkS4class{traits}.
#' @param ... other arguments passed on to methods.
#'
#' @return An object of the same class as \code{x} and \code{y}, i.e.,
#'   \linkS4class{studies}, \linkS4class{associations}, \linkS4class{variants},
#'   or \linkS4class{traits}.
#' @name setop
NULL

#' @rdname setop
#' @importFrom dplyr union
#' @examples
#' #
#' # union()
#' #
#' # Combine studies and remove duplicates
#' union(studies_ex01, studies_ex02)
#'
#' # Combine associations and remove duplicates
#' union(associations_ex01, associations_ex02)
#'
#' # Combine variants and remove duplicates
#' union(variants_ex01, variants_ex02)
#'
#' # Combine traits and remove duplicates
#' union(traits_ex01, traits_ex02)
#'
#' @export
setGeneric('union', function(x, y) standardGeneric('union'))

#' @rdname setop
#' @importFrom dplyr intersect
#' @examples
#' #
#' # intersect()
#' #
#' # Intersect common studies
#' intersect(studies_ex01, studies_ex02)
#'
#' # Intersect common associations
#' intersect(associations_ex01, associations_ex02)
#'
#' # Intersect common variants
#' intersect(variants_ex01, variants_ex02)
#'
#' # Intersect common traits
#' intersect(traits_ex01, traits_ex02)
#'
#' @export
setGeneric('intersect', function(x, y) standardGeneric('intersect'))

#' @rdname setop
#' @importFrom dplyr setdiff
#' @examples
#' #
#' # setdiff()
#' #
#' # Remove studies from ex01 that are also present in ex02
#' setdiff(studies_ex01, studies_ex02)
#'
#' # Remove associations from ex01 that are also present in ex02
#' setdiff(associations_ex01, associations_ex02)
#'
#' # Remove variants from ex01 that are also present in ex02
#' setdiff(variants_ex01, variants_ex02)
#'
#' # Remove traits from ex01 that are also present in ex02
#' setdiff(traits_ex01, traits_ex02)
#'
#' @export
setGeneric('setdiff', function(x, y) standardGeneric('setdiff'))

#' @rdname setop
#' @importFrom dplyr setequal
#' @examples
#' #
#' # setequal()
#' #
#' # Compare two studies objects
#' setequal(studies_ex01, studies_ex01)
#' setequal(studies_ex01, studies_ex02)
#'
#' # Compare two associations objects
#' setequal(associations_ex01, associations_ex01)
#' setequal(associations_ex01, associations_ex02)
#'
#' # Compare two variants objects
#' setequal(variants_ex01, variants_ex01)
#' setequal(variants_ex01, variants_ex02)
#'
#' # Compare two traits objects
#' setequal(traits_ex01, traits_ex01)
#' setequal(traits_ex01, traits_ex02)
#'
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
#' Binds together GWAS Catalog objects of the same class. Note that
#' \code{bind()} preserves duplicates whereas
#' \code{\link[gwasrapidd:setop]{union}} does not.
#'
#' @param x An object of class: \linkS4class{studies},
#'   \linkS4class{associations}, \linkS4class{variants}, or
#'   \linkS4class{traits}.
#' @param ... Objects of the same class as \code{x}.
#'
#' @return An object of the same class as \code{x}.
#' @examples
#' # Join two studies objects.
#' bind(studies_ex01, studies_ex02)
#'
#' # Join two associations objects.
#' bind(associations_ex01, associations_ex02)
#'
#' # Join two variants objects.
#' bind(variants_ex01, variants_ex02)
#'
#' # Join two traits objects.
#' bind(traits_ex01, traits_ex02)
#'
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

#' Filter GWAS Catalog objects by identifier.
#'
#' Use \code{filter_by_id} to filter GWAS Catalog objects by their respective
#' identifier (\code{id}).
#'
#' @param x An object of class either \linkS4class{studies},
#'   \linkS4class{associations}, \linkS4class{variants}, or
#'   \linkS4class{traits}.
#' @param id Identifier.
#'
#' @return Returns an object of class either \linkS4class{studies},
#'   \linkS4class{associations}, \linkS4class{variants}, or
#'   \linkS4class{traits}.
#' @keywords internal
setGeneric("filter_by_id", function(x, id) standardGeneric('filter_by_id'))


#' @keywords internal
setMethod("filter_by_id",
          signature(x = "studies", id = "character"),
          definition = function(x, id) {
            # study_id <- rlang::expr(study_id)
            # lst <- purrr::map(s4_to_list(x), ~ dplyr::filter(.x, !!study_id %in% id))
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(study_id = id),
                                .x,
                                by = 'study_id')
                              )
            y <- list_to_s4(lst, "studies")
            return(y)
})


#' @keywords internal
setMethod("filter_by_id",
          signature(x = "associations", id = "character"),
          definition = function(x, id) {
            # association_id <- rlang::expr(association_id)
            # lst <- purrr::map(s4_to_list(x), ~ dplyr::filter(.x, !!association_id %in% id))
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(association_id = id),
                                .x,
                                by = 'association_id')
            )
            y <- list_to_s4(lst, "associations")
            return(y)
          })


#' @keywords internal
setMethod("filter_by_id",
          signature(x = "variants", id = "character"),
          definition = function(x, id) {
            # variant_id <- rlang::expr(variant_id)
            # lst <- purrr::map(s4_to_list(x), ~ dplyr::filter(.x, !!variant_id %in% id))
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(variant_id = id),
                                .x,
                                by = 'variant_id')
            )
            y <- list_to_s4(lst, "variants")
            return(y)
})


#' @keywords internal
setMethod("filter_by_id",
          signature(x = "traits", id = "character"),
          definition = function(x, id) {
            # efo_id <- rlang::expr(efo_id)
            # lst <- purrr::map(s4_to_list(x), ~ dplyr::filter(.x, !!efo_id %in% id))
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(efo_id = id),
                                .x,
                                by = 'efo_id')
            )
            y <- list_to_s4(lst, "traits")
            return(y)
          })

#' Subset a studies object
#'
#' You can subset \linkS4class{studies} by identifier or by position using the
#' \code{`[`} operator.
#'
#' @param x A \linkS4class{studies} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{studies} object.
#' @examples
#' # Subset a studies object by identifier
#' studies_ex01['GCST001585']
#'
#' # Or by its position in table studies
#' studies_ex01[1]
#'
#' # Keep all studies except the first
#' studies_ex01[-1]
#'
#' @name subset-studies
NULL

#' @rdname subset-studies
#' @export
setMethod("[",
          signature(x = "studies", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) x)

#' @rdname subset-studies
#' @export
setMethod("[",
          signature(x = "studies", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            study_ids <- x@studies$study_id[i]
            filter_by_id(x, id = study_ids)
            }
          )

#' @rdname subset-studies
#' @export
setMethod("[",
          signature(x = "studies", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
            }
          )

#' Subset an associations object
#'
#' You can subset \linkS4class{associations} by identifier or by position using the
#' \code{`[`} operator.
#'
#' @param x A \linkS4class{associations} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{associations} object.
#' @examples
#' # Subset an associations object by identifier
#' associations_ex01['22505']
#'
#' # Or by its position in table associations
#' associations_ex01[2]
#'
#' # Keep all associations except the second
#' associations_ex01[-2]
#'
#' @name subset-associations
NULL

#' @rdname subset-associations
#' @export
setMethod("[",
          signature(x = "associations", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) x)

#' @rdname subset-associations
#' @export
setMethod("[",
          signature(x = "associations", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            association_ids <- x@associations$association_id[i]
            filter_by_id(x, id = association_ids)
            }
          )

#' @rdname subset-associations
#' @export
setMethod("[",
          signature(x = "associations", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
            }
          )

#' Subset a variants object
#'
#' You can subset \linkS4class{variants} by identifier or by position using the
#' \code{`[`} operator.
#'
#' @param x A \linkS4class{variants} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{variants} object.
#' @examples
#' # Subset a variants object by identifier
#' variants_ex01['rs4725504']
#'
#' # Or by its position in table variants
#' variants_ex01[3]
#'
#' # Keep all variants except the third
#' variants_ex01[-3]
#'
#' @name subset-variants
NULL

#' @rdname subset-variants
#' @export
setMethod("[",
          signature(x = "variants", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) x)

#' @rdname subset-variants
#' @export
setMethod("[",
          signature(x = "variants", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            variant_ids <- x@variants$variant_id[i]
            filter_by_id(x, id = variant_ids)
            }
          )

#' @rdname subset-variants
#' @export
setMethod("[",
          signature(x = "variants", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
            }
          )

#' Subset a traits object
#'
#' You can subset \linkS4class{traits} by identifier or by position using the
#' \code{`[`} operator.
#'
#' @param x A \linkS4class{traits} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#' @return A \linkS4class{traits} object.
#' @examples
#' # Subset a traits object by identifier
#' traits_ex01['EFO_0004884']
#'
#' # Or by its position in table traits
#' traits_ex01[1]
#'
#' # Keep all traits except the second
#' traits_ex01[-2]
#'
#' @name subset-traits
NULL

#' @rdname subset-traits
#' @export
setMethod("[",
          signature(x = "traits", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) x)

#' @rdname subset-traits
#' @export
setMethod("[",
          signature(x = "traits", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            efo_ids <- x@traits$efo_id[i]
            filter_by_id(x, id = efo_ids)
            }
          )

#' @rdname subset-traits
#' @export
setMethod("[",
          signature(x = "traits", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
          })


#' Number of GWAS Catalog entities
#'
#' This function returns the number of unique entities in a GWAS Catalog object.
#'
#' @param x A \linkS4class{studies}, an
#' \linkS4class{associations}, a \linkS4class{variants}, or a
#' \linkS4class{traits} object.
#' @param unique Whether to count only unique entries (\code{TRUE}) or not
#'   (\code{FALSE}).
#'
#' @return An integer scalar.
#'
#' @export
setGeneric('n', function(x, unique = FALSE) standardGeneric('n'))

#' @rdname n
#' @examples
#' # Determine number of studies
#' n(studies_ex01)
#'
#' # Determine number of associations
#' n(associations_ex01)
#'
#' # Determine number of variants
#' n(variants_ex01)
#'
#' # Determine number of traits
#' n(traits_ex01)
#'
#' @export
setMethod("n",
          signature(x = "studies"),
          definition = function(x, unique = FALSE) {
            if (unique) n <- dplyr::n_distinct(x@studies$study_id)
            else n <- nrow(x@studies)
            return(n)
            }
          )

#' @rdname n
#' @export
setMethod("n",
          signature(x = "associations"),
          definition = function(x, unique = FALSE) {
            if (unique) n <- dplyr::n_distinct(x@associations$association_id)
            else n <- nrow(x@associations)
            return(n)
            }
          )

#' @rdname n
#' @export
setMethod("n",
          signature(x = "variants"),
          definition = function(x, unique = FALSE) {
            if (unique) n <- dplyr::n_distinct(x@variants$variant_id)
            else n <- nrow(x@variants)
            return(n)
            }
          )

#' @rdname n
#' @export
setMethod("n",
          signature(x = "traits"),
          definition = function(x, unique = FALSE) {
            if (unique) n <- dplyr::n_distinct(x@traits$efo_id)
            else n <- nrow(x@traits)
            return(n)
            }
          )
