#' Clean variable labels
#'
#' This function standardises the labels of all characters of factors in a
#' `data.frame`. It uses the standardisation implemented by
#' [epitrix::clean_labels()] in the `epitrix` package. See
#' `?epitrix::clean_labels` for more information.
#'
#' @author Thibaut Jombart
#'
#' @param x a `data.frame`
#'
#' @param ... further arguments passed to [epitrix::clean_labels()]; the most
#'   important is `sep`, which refers to the separator used between words,
#'   and defaults to the underscore `_`.
#'
#' @export
#'
#' @return A `data.frame` with standardised labels for characters and
#'   factors.
#'
#' @examples
#'
#' ## make toy data
#' onsets <- as.Date("2018-01-01") + sample(1:10, 20, replace = TRUE)
#' genders <- c("male", "female", "FEMALE", "Male", "Female", "MALE")
#' gender <- sample(genders, 20, replace = TRUE)
#' case_types <- c("confirmed", "probable", "suspected", "not a case",
#'                 "Confirmed", "PROBABLE", "suspected  ", "Not.a.Case")
#' case <- factor(sample(case_types, 20, replace = TRUE))
#' toy_data <- data.frame("Date of Onset." = onsets,
#'                        "GENDER_ " = gender,
#'                        "Épi.Case_définition" = case)
#' ## show data
#' toy_data
#'
#'
#' ## clean variable names, store in new object, show results
#' clean_data <- clean_variable_names(toy_data)
#' clean_data

clean_variable_labels <- function(x, ...) {

  if (is.null(ncol(x)) || ncol(x)==0L) {
    stop("x has no columns")
  }

  classes <- i_find_classes(x)
  are_characters <- which(classes == "character")
  are_factors <- which(classes == "factor")

  out <- x
  for(e in are_characters) {
    out[[e]] <- epitrix::clean_labels(out[[e]], ...)
  }
  for(e in are_factors) {
    levels(out[[e]]) <- epitrix::clean_labels(levels(out[[e]]), ...)
  }

  out
}

