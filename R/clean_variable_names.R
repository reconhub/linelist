#' Clean variable names
#'
#' This function takes standardises the variable names in a
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
clean_variable_names <- function(x, ...) {
  variable_names <- colnames(x)
  if (is.null(variable_names)) {
    stop('x has no column names')
  }

  colnames(x) <- epitrix::clean_labels(variable_names, ...)
  x
}

