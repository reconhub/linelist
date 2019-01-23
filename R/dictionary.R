#' Linelist Global Variables
#' 
#' This defines the legal variables that linelist will recognise.
#' 
#' @param ... quoted varaibles to add to the default variables
#' @param set when `TRUE`, the variables provided in `...` will be added to the
#'   global variables. Defaults to `FALSE``
#' @param reset when `TRUE`, the global variables are reset to the default 
#'   variables listed above. Defaults to `FALSE`
#'
#' @aliases linelist_options
#' @export
#' @rdname dictionary
#' @examples 
#' 
#' # see the default varaibles
#' get_dictionary()
#' 
#' # Equivalent
#' getOption("linelist_epivars")
#' 
#' # If you create a new method and need other varaibles, or just want a shorter
#' # representation, they can be added to your options:
#' 
#' get_dictionary("random", set = TRUE)
#' 
#' # You can also reset the variables
#' get_dictionary(reset = TRUE)
linelist_options <- function(..., set = FALSE, reset = FALSE) {
  linelist_epivars <- default_dictionary()
  out <- unique(c(getOption("linelist_epivars"), c(...), linelist_epivars))
  if (set)   options(linelist_epivars = out)
  if (reset) options(linelist_epivars = linelist_epivars)
  getOption("linelist_epivars")
}





#' @export
#' @rdname dictionary
get_dictionary <- function(...) {
  linelist_options(...)
}






#' @export
#' @rdname dictionary
set_dictionary <- function(..., reset = FALSE) {
  linelist_options(..., set = TRUE, reset = reset)
}






#' @export
#' @rdname dictionary
reset_dictionary <- function(...) {
  set_dictionary(..., reset = TRUE)
}
