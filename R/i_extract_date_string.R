#' Extract date fron a character string
#'
#' Internal function. This function looks for a well-formatted date character
#' string inside a single character string, and returns the matching date using
#' the `%Y-%m-%d` format (e.g. `2018-01-23`).
#'
#'
#' @author Thibaut Jombart
#'
#' @return
#' Either `NA_character_` or a date, as a standardised character string.
#'
i_extract_date_string <- function(x) {

  ## This function tries converting a single character string into a
  ## well-formatted date, but still returning a character. If it can't convert
  ## it, it returns NA.

  date_info <- i_find_date_format(x)
  if (is.null(date_info)) {
    return(NA_character_)
  }

  as.character(as.Date(date_info["date"], format = date_info["format"]))

}
