#' Try and guess dates from a characters
#'
#' Note that THIS FEATURE IS STILL EXPERIMENTAL: we strongly recommend checking
#' a few converted dates manually. This function tries to extract dates from a
#' `character` vector or a `factor`. It treats each entry independently, using
#' regular expressions to detect if a date is present, its format, and if
#' successful it converts that entry to a standard `Date` with the *Ymd* format
#' (e.g. `2018-01-21`). Entries which cannot be processed result in `NA`. An
#' error threshold can be used to define the maximum number of resulting `NA`
#' (i.e. entries without an identified date) that can be tolerated. If this
#' threshold is exceeded, the original vector is returned.
#'
#'
#' @author Thibaut Jombart
#'
#' @export
#'
#' @details Converting ambiguous character strings to dates is difficult for
#'     many reasons:
#'
#' - dates may not use the standard Ymd format
#' 
#' - within the same variable, dates may follow different formats
#' 
#' - dates may be mixed with things that are not dates
#' 
#' - the behaviour of `as.Date` in the presence of non-date is hard to predict,
#'   sometimes returning `NA`, sometimes issuing an error.
#'
#' This function tries to address all the above issues. Dates with the following
#' format should be automatically detected, irrespective of separators
#' (e.g. "-", " ", "/") and surrounding text:
#'
#' - "19 09 2018"
#' - "2018 09 19"
#' - "19 Sep 2018"
#' - "2018 Sep 19"
#'
#' Note that if a character string has multiple dates, it is currently hard to
#' predict which date will be returned.
#'
#' @param x a `character` vector or a `factor`
#'
#' @param error_tolerance a number between 0 and 1 indicating the proportion of
#'     entries which cannot be identified as dates to be tolerated; if this
#'     proportion is exceeded, the original vector is returned, and a message is
#'     issued; defaults to 0.1 (10 percent)
#'
#' @param quiet a logical indicating if messages should be displayed to the
#'     console (`TRUE`, default); set to `FALSE` to silence messages
#'
#' @examples
#' 
#' x <- c("01-12-2001", "male", "female", "2018-10-18", NA, NA, "2018_10_17",
#'       "2018 10 19", "// 24/12/1989", "this is 24/12/1989!",
#'       "RECON NGO: 19 Sep 2018 :)")
#' guess_dates(x, error_tolerance = 1) # forced conversion
#' guess_dates(x, error_tolerance = 0.15) # 15 percent errors allowed 

guess_dates <- function(x, error_tolerance = 0.1, quiet = TRUE) {

  ## This function tries converting a single character string into a
  ## well-formatted date, but still returning a character. If it can't convert
  ## it, it returns NA.

  
  if (is.factor(x)) {
    x <- as.character(x)
  }

  ## convert all entries to character strings
  new_x <- vapply(x, i_extract_date_string, character(1))

  ## check how successful we were
  na_before <- sum(is.na(x))
  na_after <- sum(is.na(new_x))
  prop_successful <- (length(x) - na_after) / (length(x) - na_before)

  ## shape result depending on whether conversion was successful
  if (prop_successful < (1 - error_tolerance)) {
    return(x)
  } else {
    return(as.Date(new_x))
  }
}
