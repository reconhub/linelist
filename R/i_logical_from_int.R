# converts an integer vector to a logical vector 
i_logical_from_int <- function(x, classes) {
  the_thing <- deparse(substitute(x))
  if (is.numeric(x)) {
    x <- seq_along(classes) %in% x
  } 
  if (!is.logical(x)) {
    stop(sprintf("%s must be a logical or integer vector.", the_thing))
  } 
  x
}
