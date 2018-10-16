#' Clean variable names
#'
#' This function standardises the variable names in a
#' \code{data.frame}. It uses the standardisation implemented by
#' \code{\link[epitrix]{clean_labels}} in the \code{epitrix} package. See
#' \code{?epitrix::clean_labels} for more information.
#'
#' @author Thibaut Jombart
#'
#' @param x a \code{data.frame}
#'
#' @param ... further arguments passed to \code{epitrix::clean_labels}; the most
#'   important is \code{sep}, which refers to the separator used between words,
#'   and defaults to the underscore \code{_}.
#'
#' @export
#'
#' @return A \code{data.frame} with standardised variable names.
#'
#' @examples
#'
#' ## make toy data
#' onsets <- as.Date("2018-01-01") + sample(1:10, 20, replace = TRUE)
#' gender <- sample(c("male", "female"), 20, replace = TRUE)
#' case_type <- c("confirmed", "probable", "suspected", "not a case")
#' case <- sample(case_type, 20, replace = TRUE)
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

clean_variable_names <- function(x, ...) {
  variable_names <- colnames(x)
  if (is.null(variable_names)) {
    stop('x has no column names')
  }

  colnames(x) <- epitrix::clean_labels(variable_names, ...)
  x
}

