#' Compare structures of two datasets
#'
#' This function extracts the structures of two `data.frames` and compares them, issuing a series of diagnostics.
#'
#' @export
#'
#' @author Thibaut Jombart
#'
#' @param x a `data.frame` to be compared against a reference
#'
#' @param ref the reference `data.frame`
#'
#' @details
#' 
#' The comparison relies on checking differences in:
#'
#' - names of columns
#'
#' - classes of the columns (only the first class is used)
#'
#' - values of the categorical variables
#' 

compare_data <- function(ref, x, ...) {
  Usemethod("compare_data")
}



#' @export
#' @rdname compare_data
compare_data.default <- function(ref, x, ...) {
  class_x <- paste(class(x), collapse = ", ")
  msg <- sprintf("compare_data has no method for the class: %s",
                 class_x)
  stop(msg)
}




#' @export
#' @rdname compare_data
compare_data.data_structure <- function(ref, x, ...) {
  x_str <- get_structure(x)

  ## compare names
  out_dim <- compare_dim(ref$dim, x_str$dim)

  ## compare names
  out_names <- compare_names(ref$names, x_str$names)

  ## compare classes
  if (length(out_names$common) > 0) {
    out_classes <- compare_classes(ref$classes, x_str$classes)
  } else {
    out_classes <- "Cannot compare classes: no variable in common"
  }
}




#' @export
#' @rdname compare_data
compare_data.data.frame <- function(ref, x, ...) {
  ref_str(get_structure(ref))
  compare_data(ref_str, x)
}



#' Compare dimensions
#'
#' Returns `NULL` if dimensions are the same, and a named list otherwise.

compare_dim <- function(ref_dim, x_dim) {

  ## first case: identical dimensions
  if (identical(ref_dim, x_dim)) {
    return(NULL)
  }

  out <- list()
  
  ## different rows
  if (ref_dim[1] != x_dim[1]) {
    out$n_rows <- sprintf("Number of rows have changed from %d to %d",
                          ref_dim[1], x_dim[1])
  }

  ## different columns
  if (ref_dim[2] != x_dim[2]) {
    out$n_columns <- sprintf("Number of columns have changed from %d to %d",
                             ref_dim[2], x_dim[2])
  }

  out
}





#' Compare vectors of names
#'
#' The function returns `NULL` if the names are the same, and a named list of
#' character strings if there are differences.

compare_names <- function(ref_names, x_names) {

  ## first case: identical names
  if (identical(ref_names, x_names)) {
    return(NULL)
  }

  ## same names, different order
  if (identical(sort(ref_names), sort(x_names))) {
    return(list(
        order = "Names of columns are the same but in different order"
    ))
  }

  ## different names
  out <- list()
  out$missing <- setdiff(ref_names, x_names)
  out$new <- setdiff(x_names, ref_names)
  out$common <- intersect(x_names, ref_names)
  
  out
}
