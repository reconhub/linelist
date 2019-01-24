#' Define which columns are epivars in a linelist
#'
#' These functions can be used to change the `epivars` registred in a `linelist`
#' object. `set_epivars` replaces previous `epivars` with new
#' values. `add_epivars` adds new `epivars` to the existing ones.
#' 
#' @export

set_epivars <- function(x, ...) {
  dots <- valid_dots(list(...))
  attr(x, "epivars") <- dots
  x
}





#' @export
#' @rdname set_epivars
add_epivars <- function(x, ...) {
  dots <- valid_dots(list(...))
  epivars <- attr(x, "epivars")
  new_epivars <- c(epivars, dots)
  attr(x, "epivars") <- new_epivars
  x
}
