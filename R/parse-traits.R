#' @include class-traits.R
NULL

#' @keywords internal
e_obj_to_traits <- function(obj) {

  e <- traits()

  # e_obj: alias for obj$content$traits
  e_obj <- obj$content$traits

  # If the object is empty return the traits S4 object as is, i.e., empty.
  if(rlang::is_empty(e_obj)) return(e)

  # traits table
  e@traits <- traits_tbl(
    efo_id = missing_to_na(e_obj$shortForm),
    trait = missing_to_na(e_obj$trait),
    uri = missing_to_na(e_obj$uri)
  )

  return(e)
}
