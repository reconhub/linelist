#' Handle dates data
#'
#' This function detects variables of `data.frame` which are effectively
#' representing dates, and converts them to `Date` objects.
#'
#' @author Thibaut Jombart
#'
#' @param x a `data.frame`
#'
#' @param
#'
#' @export
#'
#' @return A `data.frame` with standardised dates.
#'
#' @examples
#'
#' ## make toy data
#' onsets <- as.POSIXct("2018-01-01") + sample(1:10, 20, replace = TRUE)
#' outcomes <- onsets + 1e7
#' admissions <- as.character(as.Date(onsets) + 1)
#' discharges <- factor(as.Date(admissions) + 1)
#' gender <- sample(c("male", "female"), 20, replace = TRUE)
#' case_type <- c("confirmed", "probable", "suspected", "not a case")
#' case <- sample(case_type, 20, replace = TRUE)
#' toy_data <- data.frame("Date of Onset." = onsets,
#'                        "_GENDER_ " = gender,
#'                        "Épi.Case_définition" = case,
#'                        "date of admission" = admissions,
#'                        "Date-of_discharge" = discharges,
#'                        stringsAsFactors = FALSE)
#' ## show data
#' toy_data
#' str(toy_data)
#'
#' ## clean variable names, store in new object, show results
#' clean_data <- clean_variable_names(toy_data)
#' clean_data <- clean_dates(clean_data)
#' clean_data

clean_dates <- function(x, ...) {
  classes <- i_find_classes(x)
  are_characters <- which(classes == "character")
  are_factors <- which(classes == "factor")

  x
}

