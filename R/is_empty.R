#' Check is an object is empty
#'
#' Auxiliary function, for internal use only
#' @param x an object
#' @noRd

is_empty <- function(x) {
  is.null(x) || (length(x) == 0L)
}
