#' Linelist Global Variables
#' 
#' This defines the legal variables that linelist will recognise.
#' 
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
#' getOption("linelist_dictionary")
#' 
#' 
#' # Set a new, one-column dictionary 
#' hosp <- data.frame(
#'   epivar = "date_hospital",
#'   hxl    = "#date +start",
#'   description = "date at which patient was hospitalized",
#'   stringsAsFactors = FALSE
#' )
#'
#' set_dictionary(hosp)
#' get_dictionary()
#'
#' # Use the default_dictionary_path() to read in the default dictionary from
#' # disk
#' default_dictionary_path()
#' set_dictionary(default_dictionary_path())
#' get_dictionary()
#'
#' # You can also reset the variables automatically
#' reset_dictionary()
#' 
default_dictionary <- function() {
  file <- default_dictionary_path()
  utils::read.delim(file, sep = "\t", stringsAsFactors = FALSE)
}

#' @export
#' @rdname dictionary
default_dictionary_path <- function() {
  system.file("default_dictionary.txt", package = "linelist", mustWork = TRUE)
}





#' @export
#' @rdname dictionary
get_dictionary <- function() {
  getOption("linelist_dictionary")
}





#' @export
#' @rdname dictionary
#' @param x either a data frame or the path to a that can produce a data frame.
#' @param ... parameters passed on to [rio::import()].
#' @details The expected format for these data is a`data.frame` with 3 columns
#' 'epivar', 'hxl' and 'description', representing the new dictionary to be
#' used; A template example can be found by using [default_dictionary()]. 
#' @importFrom rio import
set_dictionary <- function(x, ...) {
  if (is.character(x)) {
    if (!file.exists(x)) {
      msg <- paste("The linelist dictionary must be either a data frame or a",
                   "path to a file. The file '%s' does not appear to exist.")
      stop(sprintf(msg, x))
    }
    x <- rio::import(x, ...)
  }
  check_dictionary(x)
  for (i in seq_len(ncol(x))) {
    x[[i]] <- as.character(x[[i]])
  }
  options(linelist_dictionary = x)
}





#' @export
#' @rdname dictionary
reset_dictionary <- function() {
  defaults <- default_dictionary()
  options(linelist_dictionary = defaults)
}
