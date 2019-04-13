

#' Convert an S4 object into a list
#'
#' @param s4_obj an S4 object
#'
#' @return A list version of the S4 object.
#' @keywords internal
s4_to_list <- function(s4_obj){
  slots <- methods::slotNames(s4_obj)
  lst <- purrr::map(slots, ~ methods::slot(object = s4_obj, name = .x))
  names(lst) <- slots
  return(lst)
}

#' Convert a named list to an S4 object
#'
#' @param list list
#' @param class character vector indicating the S4 class
#'
#' @return S4 object of class \code{class}.
#' @keywords internal
list_to_s4 <-  function(list, class) {
  s4_obj <- methods::new(class)
  slots <- methods::slotNames(s4_obj)
  lst_names <- names(list)

  if (!setequal(slots, lst_names)) {
    stop("names of 'list' do not match 'class'")
  }
  for (slot in slots) {
    methods::slot(s4_obj, slot, check = FALSE) <- list[[slot]]
  }
  # TODO: Check on isValidObject
  isValidObject <- methods::validObject(s4_obj)
  return(s4_obj)
}
