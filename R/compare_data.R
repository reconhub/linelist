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

  out <- list()
  
  ## compare names
  out$dim <- compare_dim(ref, x_str)

  ## compare names
  out$names <- compare_names(ref, x_str)

  ## compare classes
  out$classes <- compare_classes(ref, x_str)

  ## compare values of categorical variables

  out
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

compare_dim <- function(ref, x) {
  ref_dim <- ref$dim
  x_dim <- x$dim
  
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

compare_names <- function(ref, x) {
  ref_names <- ref$names
  x_names <- x$names
  
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




#' Compare vectors of classes
#'
#' The function returns `NULL` if the classes are the same, and a named list of
#' character strings if there are differences.

compare_classes <- function(ref, x) {
  ref_names <- ref$names
  ref_classes <- ref$classes
  names(ref_classes) <- ref_names

  x_names <- x$names
  x_classes <- x$classes
  names(x_classes) <- x_names

    
  ## first case: identical classes
  if (identical(ref_classes, x_classes)) {
    return(NULL)
  }

  ## find common variables
  common_variables <- intersect(ref_names, x_names)
  n_common <- length(common_variables)
  
  ## no variable in common - get out of here
  if (length(common_variables) == 0) {
    out <- "Cannot compare classes: no variable in common"
    return(out)
  }

  
  ## general case: comparison for common variables
  out <- list()
  for (i in seq_len(n_common)) {
    current_variable <- common_variables[i]
    
    if (ref_classes[current_variable] !=
        x_classes[current_variable]) {
      out[[current_variable]] <- sprintf(
          "Class of `%s` has changed from `%s` to `%s`",
          current_variable,
          ref_classes[current_variable],
          x_classes[current_variable])
    }
  }

  if (length(out) == 0) {
    out <- NULL
  }
  out
}




#' Compare values of categorical variables
#'
#' The function returns `NULL` if the categories of categorical variables are
#' the same, and a named list of character strings if there are differences.

compare_values <- function(ref, x) {
  ref_names <- ref$names
  ref_classes <- ref$classes
  ref_values <- ref$values
  names(ref_classes) <- ref_names

  x_names <- x$names
  x_classes <- x$classes
  x_values <- x$values
  names(x_classes) <- x_names

    
  ## first case: identical values
  if (identical(ref_values, x_values)) {
    return(NULL)
  }

  ## find common variables, keeping only categorical variables, i.e. `factor`
  ## and `character`
  categories <- c("factor", "character")
  
  ref_names_to_keep <- ref_classes %in% categories
  ref_names <- ref_names[ref_names_to_keep]

  x_names_to_keep <- x_classes %in% categories
  x_names <- x_names[x_names_to_keep]

  common_variables <- intersect(ref_names, x_names)  
  n_common <- length(common_variables)
  
  ## no variable in common - get out of here
  if (length(common_variables) == 0) {
    out <- "Cannot compare values: no categorical variable in common"
    return(out)
  }

  
  ## general case: comparison for common variables
  out <- list()
  
  for (i in seq_len(n_common)) {
    current_variable <- common_variables[i]

    ref_values_current <- ref_values[current_variable]
    x_values_current <- x_values[current_variable]
    
    if (!identical(ref_values_current, x_values_current)) {
      
      out[[current_variable]] <- list(
          missing = setdiff(ref_values_current, x_values_current),
          new = setdiff(x_values_current, ref_values_current),
          common = intersect(x_values_current, ref_values_current))
    }
  }

  if (length(out) == 0) {
    out <- NULL
  }
  
  out
}
