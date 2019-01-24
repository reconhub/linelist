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
#' 
#' # You can also reset the variables
#' reset_dictionary()
#' 
default_dictionary <- function() {
  file <- system.file("default_dictionary.txt",
                      package = "linelist",
                      stringsAsFactors = FALSE)
  read.delim(file, sep = "\t")
}





#' @export
#' @rdname dictionary
get_dictionary <- function() {
  getOption("linelist_dictionary")
}





#' @export
#' @rdname dictionary
#' @param x a `data.frame` with 3 columns 'epivar', 'hxl' and 'description',
#'   representing the new dictionary to be used; see `default_dictionary` for a
#'   template
set_dictionary <- function(x) {
  check_dictionary(x)
  for (i in seq_len(ncol(x))) {
    x[i] <- as.character(x[i])
  }
  options(linelist_dictionary = x)
}





#' @export
#' @rdname dictionary
reset_dictionary <- function() {
  defaults <- default_dictionary()
  options(linelist_dictionary = defaults)
}
