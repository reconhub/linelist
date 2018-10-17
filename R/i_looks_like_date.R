#' Finds whether a character string has a date format
#'
#' Internal function. Input is a character string vector. Examines if the format
#' is compatible with ISO date (YYYY-MM-DD), and generates a vector of logicals
#' indicating whether the input character strings have a date format, or
#' not. The final value returned is TRUE if the average number of entries
#' matching the format exceeds a given proportion (1 by default, i.e. all
#' entries must be dates).
#'
#'
#' @author Thibaut Jombart
#'
i_looks_like_date <- function(x, threshold = 1) {
  new_x <- i_convert_char_to_date(x)

  na_before <- sum(is.na(x))
  na_after <- sum(is.na(new_x))
  prop_successful <- (length(x) - na_after) / (length(x) - na_before)

  prop_successful >= threshold
}
