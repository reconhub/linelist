#' @rdname accessors
#' @param ... For `set_vars()`, any number of variables defined in
#'   [epivars()] that can be used for mapping or modelling. 
#' @export
set_vars <- function(x, ...) {
  UseMethod("set_vars")
}

#' @rdname accessors
#' @param name the name of the variable in [epivars()] to assign
#' @param value the name of the column in the locations data
#' @export
"set_vars<-" <- function(x, name, value) {
  UseMethod("set_vars<-")
}

#' @rdname accessors
#' @export
set_vars.linelist <- function(x, ...) {
  dots <- valid_dots(list(...))
  evars <- attr(x, "epivars")
  for (dot in names(dots)) {
    # This is necesarry so that NULL values can remove the element
    evars[[dot]]$name <- dots[[dot]]
  }
  evars <- if (length(evars) > 0) evars else list()
  attr(x, "epivars") <- evars
  x
}

#' @rdname accessors
#' @export
"set_vars<-.linelist" <- function(x, name, value) {
  if (missing(name)) {
    if (is.null(value)) {
      attr(x, "epivars") <- list()
      return(x)
    }
    the_call <- c(list(x), as.list(value))
  } else {
    value <- list(value)
    names(value) <- name
    the_call <- c(list(x), value)
  }
  do.call("set_vars.linelist", the_call)
}
