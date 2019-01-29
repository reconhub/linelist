#' Lookup a column name defined by an epivar
#'
#' For use in dplyr pipelines and other functions that need to know the names
#' of columns, the lookup function looks up the column name for a given epivar
#' in your data set.
#'
#' @param x a [linelist][as_linelist] object. 
#' @param epivar the name of an epivar defined in the [dictionary][get_dictionary()].
#' @return a character vector of the column name
#' @examples
#' dat <- clean_data(messy_data())
#' ll  <- as_linelist(dat,
#'                    id = "id", 
#'                    gender = "gender",
#'                    date_onset = "date_of_onset", 
#'                    geo = c("lon", "lat")
#'                   )
#' 
lookup <- function(x, epivar = NULL) {
  stopifnot(inherits(x, "linelist"))
  ev <- attr(x, "epivars")
  if (is.null(epivar)) {
    return(ev)
  }
  stopifnot(is.character(epivar))
  ev[[epivar]]
}
