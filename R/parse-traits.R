#' @include class-traits.R
NULL

#' @keywords internal
e_obj_to_traits <- function(obj) {

  e <- traits()

  # e_obj: alias for obj$content$efoTraits
  e_obj <- obj$content$efoTraits

  # If the object is empty return the traits S4 object as is, i.e., empty.
  if(rlang::is_empty(e_obj)) return(e)

  # traits table
  e@traits <- traits_tbl(
    efo_id = recode_missing(tws(e_obj$shortForm)),
    trait = recode_missing(tws(e_obj$trait)),
    uri = recode_missing(tws(e_obj$uri))
  )

  return(e)
}
