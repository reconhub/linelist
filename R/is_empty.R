#' Check is an object is empty
#'
#' Auxiliary function, for internal use only

is_empty <- function(x) {
  is.null(x) || (length(x) == 0L)
}
