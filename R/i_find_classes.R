## Find classes of the columns of a data.frame
##
## Internal function. Returns a vector of characters indicating the classes of
## the different variables of a data.frame. In the case of multiple classes, we
## only return the first one.
##
##
## @author Thibaut Jombart
i_find_classes <- function(x) {
  class_1 <- function(e) class(e)[1]
  vapply(x, class_1, character(1))
}
