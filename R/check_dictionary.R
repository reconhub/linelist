
## This function performs basic checks on a dictionary. It should be a
## data.frame with 3 columns named 'epivar', 'hxl', and 'description', in this
## order.

check_dictionary <- function(x) {
  if (!is.data.frame(x)) {
    msg <- sprintf("x is not a data.frame but a %s",
                   class(x))
    stop(msg)
  }

  if (ncol(x) != 3L) {
    msg <- sprintf("x does not have 3 columns but %d",
                   ncol(x))
    stop(x)
  }
}
