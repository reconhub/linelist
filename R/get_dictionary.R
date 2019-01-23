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
#' @aliases linelist.epivars
#' @export
#' @examples 
#' 
#' # see the default varaibles
#' get_dictionary()
#' 
#' # Equivalent
#' getOption("linelist.epivars")
#' 
#' # If you create a new method and need other varaibles, or just want a shorter
#' # representation, they can be added to your options:
#' 
#' get_dictionary("random", set = TRUE)
#' 
#' # You can also reset the variables
#' get_dictionary(reset = TRUE)
get_dictionary <- function(..., set = FALSE, reset = FALSE) {
  linelist.epivars <- c("id",          # Unique identification
                        "date_onset",  # Date of symptom onset
                        "date_report", # Date of reporting
                        "gender",      # Gender of individual
                        "age",         # Age of individual
                        "age_group",   # Age grouping
                        "geo"          # Geographical coordinates (must be two columns)
                        )
  res <- unique(c(getOption("linelist.epivars"), c(...), linelist.epivars))
  if (set)   options(linelist.epivars = res)
  if (reset) options(linelist.epivars = linelist.epivars)
  getOption("linelist.epivars")
}
