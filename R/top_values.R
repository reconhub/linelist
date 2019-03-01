#' Recode factors, keeping only most frequent levels
#'
#' This function is a generic, with methods for `factor` and `character`
#' objects. It lists all unique values in the input, ranks them from the most to
#' the least frequent, and keeps the top `n` values. Other values are replaced
#' by the chosen replacement. Under the hood, this uses [forcats::fct_lump()]
#' and [forcats::fct_recode()].
#'
#' @author Thibaut Jombart, Zhian N. Kamvar
#'
#' @export
#'
#' @param x a `factor` or a `character` vector
#'
#' @param n the number of levels or values to keep
#'
#' @param replacement a single value to replace the less frequent values with
#'
#' @param ... further arguments passed to [forcats::fct_lump()].
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
#' @importFrom forcats fct_lump
top_values.factor <- function(x, n, replacement = "other", ...) {

  # check if the replacement is missing... fct_lump doesn't like other_level = NA
  other_is_missing <- is.na(replacement)

  # use a unique level for the other to avoid overwriting any levels.
  other <- if (other_is_missing) sprintf("other%s", Sys.time()) else replacement
  
  # do the work
  out <- forcats::fct_lump(x, n = n, other_level = other, ...) 

  # remove the "other" if other is missing
  if (other_is_missing) {
    out <- forcats::fct_recode(out, NULL = other)
  }

  out
  
}


#' @export
#' @rdname top_values
top_values.character <- function(x, n, replacement = "other", ...) {

  # convert to factor, filter, and return as a character again
  as.character(top_values(factor(x), n = n, replacement = replacement, ...))

}



