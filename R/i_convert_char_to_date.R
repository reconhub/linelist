#' Try and convert a character string to dates
#'
#' Internal function. This function processes each element of a character vector
#' independently, trying for each to detect the date format, and extract the
#' date using Ymd format (e.g. `2018-01-21`).
#'
#'
#' @author Thibaut Jombart
#'
#' @details Converting ambiguous character strings to dates is difficult, as the
#'   behaviour of `as.Date` in the presence of non-date is hard to predict -
#'   sometimes returning `NA`, sometimes issuing an error. The approach
#'   implemented here processes items of a character vector one by one, and for
#'   each, tries to convert it to date; if it succeeds, it returns a
#'   well-formatted date as `character`, and if not, `NA`. The whole conversion
#'   to date of the entire vector is done at the end.
#'
i_convert_char_to_date <- function(x) {

  ## This function tries converting a single character string into a
  ## well-formatted date, but still returning a character. If it can't convert
  ## it, it returns NA.


}
