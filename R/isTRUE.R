#' For internal use only
#'
#' This is to make the package compatible with older versions of R, which did
#' not have `isTRUE`
#' @param x an object
#' @noRd

isTRUE <- function (x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}
