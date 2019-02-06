#' Handle dates data
#'
#' This function detects variables of `data.frame` which are effectively
#' representing dates, and converts them to `Date` objects. When variables are
#' character strings or factors, the function will try to convert dates with
#' various pre-defined formats (see *details*). For each variable, the most
#' common date format is automatically detected, and dates not following it are
#' set to `NA` (i.e. missing). It uses a tolerance threshold for the amount of
#' entries which cannot be converted to date (`error_tolerance`). By default,
#' tolerance is set to `0.1`, meaning 10% of errors in dates entry is allowed
#' for a given variable. If there are more errors, this variable is assumed not
#' to be a date, and left untouched.
#'
#' @author Thibaut Jombart, Zhian N. Kamvar
#'
#' @param x a `data.frame`
#'
#' @param force_Date a `logical` indicating if `POSIXct` and `POSIXlt` objects
#'   should be converted to `Date` objects; defaults to `TRUE`; you should use
#'   this if your dates are only precise to the day (i.e. no time information
#'   within days).
#'
#' @param guess_dates a `logical` indicating if dates should be guessed in
#'   columns storing character strings or `factors`; this feature is
#'   experimental; see [guess_dates()] for more information.
#'
#' @inheritParams guess_dates
#' 
#' @param ... further arguments passed on to [guess_dates()]
#'
#' @seealso  [guess_dates()] to extract dates from a messy input vector
#' 
#' @export
#'
#' @return A `data.frame` with standardised dates.
#'
#' @examples
#'
#' ## make toy data
#' onsets <- as.POSIXct("2018-01-01") + sample(1:10, 20, replace = TRUE)
#' onsets2 <- format(as.Date(onsets), "%d/%m/%Y")
#' onsets3 <- format(as.Date(onsets), "%d %m %Y")
#' outcomes <- onsets + 1e7
#' admissions <- as.character(as.Date(onsets) + 1)
#' admissions[1:5] <- NA
#' discharges <- factor(as.Date(admissions) + 1)
#' onset_with_errors <- onsets2
#' onset_with_errors[c(1,20)] <- c("male", "confirmed")
#' mixed_info <- onsets3
#' mixed_info[1:10] <- sample(c("bleeding", "fever"), 10, replace = TRUE)
#' gender <- sample(c("male", "female"), 20, replace = TRUE)
#' case_type <- c("confirmed", "probable", "suspected", "not a case")
#' case <- sample(case_type, 20, replace = TRUE)
#' toy_data <- data.frame("Date of Onset." = onsets,
#'                        "onset 2" = onsets2,
#'                        "ONSET 3" = onsets3,
#'                        "onset_4" = onset_with_errors,
#'                        "date admission" = admissions,
#'                        "DATE.of.DISCHARGE" = discharges,
#'                        "GENDER_ " = gender,
#'                        "Épi.Case_définition" = case,
#'                        "date of admission" = admissions,
#'                        "Date-of_discharge" = discharges,
#'                        "extra" = mixed_info,
#'                        stringsAsFactors = FALSE)
#' ## show data
#' toy_data
#' str(toy_data)
#'
#' ## clean variable names, store in new object, show results
#' clean_data <- clean_variable_names(toy_data)
#' clean_data <- clean_dates(clean_data, first_date = as.Date("1950-01-01"))
#' clean_data

clean_dates <- function(x, force_Date = TRUE, guess_dates = TRUE, error_tolerance = 0.5, ... ) {
  if (!is.data.frame(x)) {
    stop("x must be a data frame or linelist object")
  }
  classes <- i_find_classes(x)
  are_POSIX <- i_find_POSIX(x)
  are_characters <- which(classes == "character")
  are_factors <- which(classes == "factor")

  if (force_Date) {
    for (i in are_POSIX) {
      x[[i]] <- as.Date(x[[i]])
    }
  }

  if (guess_dates) {
    for (i in c(are_characters, are_factors)) {
      x[[i]] <- guess_dates(x[[i]], error_tolerance = error_tolerance, ...)
    }
  }

  x
}

