#' Linelist Global Variables
#' 
#' This defines the legal variables that linelist will recognise.
#' 
#' @param ... quoted varaibles to add to the current dictionary
#' @param set when `TRUE`, the variables provided in `...` will be added to the
#'   global variables. Defaults to `FALSE``
#' @param reset when `TRUE`, the global variables are reset to the default 
#'   variables listed above. Defaults to `FALSE`
#' 
#' @description These functions let the user define variables that are important
#' to a given analysis script. These functions add to or reset variables
#' contained in `getOption("linelist_epivars")`. 
#'
#'  - `set_dictionary()`  will **add** variables to the current defined epivars.
#'     **Use this function at the beginning of your analysis script or Rmd file.**
#'  - `get_dictionary()` shows the current defined epivars that linelist will
#'     recognise.
#'  - `reset_dictionary()` will reset the dictionary to those in 
#'    `default_dictionary()`.
#'
#' All of these functions are wrappers for `linelist_options()`, which itself
#' is a wrapper for `options(linelist_epivars = ...)`. 
#' @author Zhian N. Kamvar, Thibaut Jombart 
#' @export
#' @rdname dictionary
#' @examples 
#' # see the default varaibles
#' get_dictionary()
#' 
#' # Equivalent
#' getOption("linelist_epivars")
#' 
#' # If you create a new method and need other varaibles, or just want a shorter
#' # representation, they can be added to your options:
#' set_dictionary("age_months", "outcome", "date_outcome")
#' 
#' # additional variables can be added by calling `set_dictionary()` again:
#' set_dictionary("case_definition")
#' 
#' # You can also reset the variables
#' reset_dictionary()
linelist_options <- function(..., set = FALSE, reset = FALSE) {
  linelist_epivars <- default_dictionary()
  out <- unique(c(getOption("linelist_epivars"), c(...), linelist_epivars))
  if (set)   options(linelist_epivars = out)
  if (reset) options(linelist_epivars = linelist_epivars)
  getOption("linelist_epivars")
}


#' @export
#' @rdname dictionary
default_dictionary <- function() {
  c("id",          # Unique identification
    "date_onset",  # Date of symptom onset
    "date_report", # Date of reporting
    "gender",      # Gender of individual
    "age",         # Age of individual
    "age_group",   # Age grouping
    "geo"          # Geographical coordinates (must be two columns)
    )  
}

#' @export
#' @rdname dictionary
get_dictionary <- function() {
  linelist_options()
}

#' @export
#' @rdname dictionary
set_dictionary <- function(...) {
  linelist_options(..., set = TRUE, reset = FALSE)
}

#' @export
#' @rdname dictionary
reset_dictionary <- function() {
  linelist_options(reset = TRUE)
}
