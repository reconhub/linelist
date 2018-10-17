#' Find the columns of a data.frame which are POSIXct or POSIXlt
#'
#' Internal function. Returns a vector of indices of columns of a data.frame
#' which are POSIXct or POSIXlt.
#'
#'
#' @author Thibaut Jombart
#'
i_find_POSIX <- function(x) {
  classes <- i_find_classes(x)
  which(i_find_classes(x) %in% c("POSIXct", "POSIXlt"))
}
