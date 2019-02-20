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
#' @param classes a vector of class definitions for each of the columns. If this
#'   is not provided, the classes will be read from the columns themselves. 
#'   Practically, this is used in [clean_data()] to mark columns as protected.
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
#' toy_data <- clean_variable_names(messy_data())
#'
#' ## clean variable labels, store in new object, show results
#' clean_data <- clean_variable_labels(toy_data)
#' clean_data

clean_variable_labels <- function(x, classes = NULL, ...) {

  if (is.null(ncol(x)) || ncol(x)==0L) {
    stop("x has no columns")
  }

  if (is.null(classes)) {
    classes <- i_find_classes(x)
  }
  are_characters <- which(classes == "character")
  are_factors    <- which(classes == "factor")

  out <- x
  for(e in are_characters) {
    out[[e]] <- epitrix::clean_labels(out[[e]], ...)
  }
  for(e in are_factors) {
    levels(out[[e]]) <- epitrix::clean_labels(levels(out[[e]]), ...)
  }

  out
}

