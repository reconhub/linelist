#' Clean variable names
#'
#' This function standardises the variable names in a
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
#' @param protect a logical or numeric vector specifying which columns to 
#'   protect from manipulation
#'
#' @export
#'
#' @return A `data.frame` with standardised variable names.
#'
#' @examples
#'
#' ## make toy data
#' onsets <- as.Date("2018-01-01") + sample(1:10, 20, replace = TRUE)
#' gender <- sample(c("male", "female"), 20, replace = TRUE)
#' case_type <- c("confirmed", "probable", "suspected", "not a case")
#' case <- sample(case_type, 20, replace = TRUE)
#' toy_data <- data.frame("Date of Onset." = onsets,
#'                        "_GENDER_ " = gender,
#'                        "Épi.Case_définition" = case)
#' ## show data
#' toy_data
#'
#'
#' ## clean variable names, store in new object, show results
#' clean_data <- clean_variable_names(toy_data)
#' clean_data

clean_variable_names <- function(x, protect = NULL, ...) {
  variable_names <- colnames(x)
  if (is.null(variable_names)) {
    stop('x has no column names')
  }

  protect <- if (is.null(protect)) FALSE else protect
  colnames(x)[!protect] <- epitrix::clean_labels(variable_names[!protect], ...)
  # preserving the original variable names in a comment
  names(variable_names) <- colnames(x)
  comment(x) <- c(comment(x), variable_names)
  x
}

