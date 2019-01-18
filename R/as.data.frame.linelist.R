#' Convert a linelist object to a data frame
#'
#' @param x a linelist object
#' @param ... unused
#' @return a data frame without the associated metadata
#' @export
#' @author Zhian N. Kamvar
#' @examples
#' md <- messy_data()
#' cd <- clean_data(md)
#' ll <- as_linelist(cd, 
#'                   id = "id", 
#'                   date_onset = "date_of_onset", 
#'                   gender = "gender",
#'                   geo = c("lon", "lat")
#'                  )
#' identical(cd, as.data.frame(ll))
as.data.frame.linelist <- function(x, ...) {
  class(x) <- "data.frame"
  attr(x, "epivars") <- NULL
  x
}
