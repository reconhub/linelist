#' Find classes of the columns of a data.frame
#'
#' Internal function. Returns a vector of characters indicating the classes of
#' the different variables of a data.frame.
#'
#'
#' @author Thibaut Jombart
_find_classes.R <- function(x) {
  vapply(x, class, character(1))
}
