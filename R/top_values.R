#' Recode factors, keeping only most frequent levels
#'
#' This function is a generic, with methods for `factor` and `character`
#' objects. It lists all unique values in the input, ranks them from the most to
#' the least frequent, and keeps the top `n` values. Other values are replaced
#' by the chosen replacement.
#'
#' @author Thibaut Jombart
#'
#' @export
#'
#' @param x a `factor` or a `character` vector
#'
#' @param n the number of levels or values to keep
#'
#' @param replacement a single value to replace the less frequent values with
#'
#' @param ... further arguments passed to other methods
#' 
#' @examples
#' 
#' ## make toy data
#' x <- sample(letters[1:10], 100, replace = TRUE)
#' sort(table(x), decreasing = TRUE)
#' 
#' ## keep top values
#' top_values(x, 2) # top 2
#' top_values(x, 2, NA) # top 3, replace with NA
#' top_values(x, 0) # extreme case, keep nothing

top_values <-  function(x, n, ...) {
  UseMethod("top_values")
}



#' @export
#' @rdname top_values
top_values.default <- function(x, n, ...) {
  class_x <- paste(class(x), collapse = ", ")
  msg <- sprintf("top_values has no method for the class: %s",
                 class_x)
  stop(msg)
}




#' @export
#' @rdname top_values
top_values.character <- function(x, n, replacement = "other", ...) {

  n_values <- length(unique(x))
  n <- min(n, n_values)
  ranked_values <- names(sort(table(x), decreasing = TRUE))
  top_values <- ranked_values[seq_len(n)]

  out <- x
  to_replace <- !out %in% top_values
  out[to_replace] <- replacement

  out
}




top_values.factor <- function(x, n, replacement = "other", ...) {
  out <- top_values(as.character(x), n = n, replacement = replacement, ...)
  factor(out)
}
