## #' @rdname accessors
## #' @export
## set_epivars <- function(x, ...) {
##   UseMethod("set_epivars")
## }

## #' @rdname accessors
## #' @param name the name of the variable in [get_dictionary()] to assign
## #' @param value the name of the column in the locations data
## #' @export
## "set_epivars<-" <- function(x, name, value) {
##   UseMethod("set_epivars<-")
## }

## #' @rdname accessors
## #' @export
## set_epivars.linelist <- function(x, ...) {
##   dots <- valid_dots(list(...))
##   evars <- attr(x, "epivars")$vars
##   for (dot in names(dots)) {
##     # This is necesarry so that NULL values can remove the element
##     evars[[dot]] <- dots[[dot]]
##   }
##   evars <- if (length(evars) > 0) evars else list()
##   attr(x, "epivars")$vars <- evars
##   meta <- list_epivars(x, epivars_only = TRUE)
##   attr(x, "epivars")$vars <- evars[unique(meta$epivar)]
##   x
## }

## #' @rdname accessors
## #' @export
## "set_epivars<-.linelist" <- function(x, name, value) {
##   if (missing(name)) {
##     if (is.null(value)) {
##       attr(x, "epivars") <- list(vars = list(), meta = data.frame())
##       return(x)
##     }
##     the_call <- c(list(x), as.list(value))
##   } else {
##     value <- list(value)
##     names(value) <- name
##     the_call <- c(list(x), value)
##   }
##   do.call("set_epivars.linelist", the_call)
## }
