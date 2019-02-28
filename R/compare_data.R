#' Compare structures of two datasets
#'
#' This function extracts the structures of two `data.frames` and compares them,
#' issuing a series of diagnostics.
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
#' @examples
#'
#' ## no differences
#' compare_data(iris, iris)
#'
#' ## different dimensions
#' compare_data(iris, iris[-1, -2])
#' compare_data(iris[-1, -2], iris) # inverse
#' 
#' ## one variable in common but different class and content
#' compare_data(iris,
#'              data.frame(Species = letters,
#'                         stringsAsFactors = FALSE))

compare_data <- function(ref, x, ...) {
  UseMethod("compare_data")
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
#'
#' All the sub-functions are internal, non-exported, and have the same
#' behaviour: they return `TRUE` if items are identical, and a named list of
#' informative messages otherwise.
#'
#' @param use_dim a `logical` indicating if dataset dimensions should be
#'   compared
#' 
#' @param use_names a `logical` indicating if names of the variables should be
#'   compared
#' 
#' @param use_classes a `logical` indicating if classes of the variables should be
#'   compared
#' 
#' @param use_values a `logical` indicating if values of matching
#'   categorical variables should be compared
#'
#' @param ... further arguments passed to other methods
#' 

compare_data.data_structure <- function(ref, x,
                                        use_dim = TRUE,
                                        use_names = TRUE,
                                        use_classes = TRUE,
                                        use_values = TRUE,
                                        ...) {
  x_str <- get_structure(x)

  out <- list()
  
  ## compare names
  if (use_dim) {
    out$dim <- compare_dim(ref, x_str)
  }

  ## compare names
  if (use_names) {
    out$names <- compare_names(ref, x_str)
  }

  ## compare classes
  if (use_classes) {
    out$classes <- compare_classes(ref, x_str)
  }

  ## compare values of categorical variables
  if (use_values) {
    out$values <- compare_values(ref, x_str)
  }

  class(out) <- c("data_comparison", "list")
  out
}




#' @export
#' @rdname compare_data
compare_data.data.frame <- function(ref, x, ...) {
  ref_str <- get_structure(ref)
  compare_data(ref_str, x)
}




#' Compare dimensions
#'
#' Returns `TRUE` if dimensions are the same, and a named list otherwise.

compare_dim <- function(ref, x) {
  ref_dim <- ref$dim
  x_dim <- x$dim
  
  ## first case: identical dimensions
  if (identical(ref_dim, x_dim)) {
    return(TRUE)
  }

  out <- list()
  
  ## different rows
  if (ref_dim[1] != x_dim[1]) {
    out$n_rows <- c(ref = ref_dim[1], new = x_dim[1])
  }

  ## different columns
  if (ref_dim[2] != x_dim[2]) {
    out$n_columns <- c(ref = ref_dim[2], new = x_dim[2])
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
    return(TRUE)
  }

  ## same names, different order
  if (identical(sort(ref_names), sort(x_names))) {
    return(list(different_order = TRUE))
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
    return(TRUE)
  }

  ## find common variables
  common_variables <- intersect(ref_names, x_names)
  n_common <- length(common_variables)
  
  ## no variable in common - get out of here
  if (n_common == 0) {
    return(list(cannot_compare = TRUE))
  }

  
  ## general case: comparison for common variables
  if (n_common > 0) {
    out <- list()
    for (i in seq_len(n_common)) {
      current_variable <- common_variables[i]
      
      if (ref_classes[current_variable] !=
          x_classes[current_variable]) {
        out[[current_variable]] <- c(
            variable = current_variable,
            ref_class = ref_classes[current_variable],
            new_class = x_classes[current_variable])
      } else {
        out[[current_variable]] <- TRUE
      }
    }
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
    return(TRUE)
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
    cannot_compare <- TRUE
    return(out)
  }

  
  ## general case: comparison for common variables
  if (n_common > 0) {
    out <- list()
    
    for (i in seq_len(n_common)) {
      current_variable <- common_variables[i]

      ref_values_current <- ref_values[current_variable]
      x_values_current <- x_values[current_variable]
      
      if (!identical(ref_values_current, x_values_current)) {
        
        out[[current_variable]] <- c(
            missing = setdiff(ref_values_current, x_values_current),
            new = setdiff(x_values_current, ref_values_current),
            common = intersect(x_values_current, ref_values_current))
      } else {
        out[[current_variable]] <- TRUE
      }
    }

  }
  
  out
}





#' @export
#' @rdname compare_data

print.data_comparison <- function(x, ...) {

  cat(
      crayon::bold("\n /// Comparisons of data content // \n")
  )

  
  ## dimension diagnostics
  if (!is_empty(x$dim)) {
    cat(
        crayon::bold("\n\n // Comparison of dimensions /")
    )
    if (isTRUE(x$dim)) {
      cat(
          crayon::green(
              "\nSame number of rows and columns")
      )
    } else {
      if (!is_empty(x$dim$n_rows)) {
        cat(sprintf("\n  * different numbers of rows: ref has %d, new data has %d",
                    x$dim$n_rows["ref"],
                    x$dim$n_rows["new"]
                    ))
      }
      if (!is_empty(x$dim$n_columns)) {
        cat(
            crayon::italic(
                sprintf("\n  * different numbers of columns: ref has %d, new data has %d",
                        x$dim$n_columns["ref"],
                        x$dim$n_columns["new"]
                        ))
        )
      }
      cat("\n")
    }
  }

  
  ## variable names diagnostics
  if (!is_empty(x$names)) {
    cat(
        crayon::bold("\n\n // Comparison of variable names /\n")
    )
    if (isTRUE(x$names)) {
      cat(
          crayon::green(
              "\nSame variable names, in the same order")
      )
    } else {
      if (!is_empty(x$names$missing)) {
        cat(
            crayon::italic(
                "\n  * variables missing in the new data:\n")
        )
        print(x$names$missing)
      }
      if (!is_empty(x$names$new)) {
        cat("\n  * new variables:\n")
        print(x$names$new)
      }
      if (!is_empty(x$names$common)) {
        cat("\n  * variables common to both datesets:\n")
        print(x$names$common)
      }
    }
  }

  
  ## variable classes diagnostics
  if (!is_empty(x$classes)) {
    cat(
        crayon::bold("\n\n // Comparison of variable classes /\n")
    )
    if (isTRUE(x$classes)) {
      cat(
          crayon::green(
              "\nSame variable classes")
      )
    } else {
      for (i in seq_along(x$classes)) {
        e <- x$classes[[i]]
        current_variable <- names(x$classes)[i]
        if (isTRUE(e)) {
          cat(
              crayon::green(
                  sprintf(
                      "`%s`: same class (%s) \n",
                      current_variable,
                      class(e))
              )
          )
        } else {
          cat(
              crayon::italic(
                  sprintf("`%s` has changed from `%s` to `%s`\n",
                          e[1],
                          e[2],
                          e[3]))
          )
        }
      }
    }
  }

  
  ## categorical variable values diagnostics
  if (!is_empty(x$values)) {
    cat(
        crayon::bold("\n\n // Comparison of values in categorical variables /\n")
    )
    if (isTRUE(x$values)) {
      cat(
          crayon::green(
              "\nSame values for categorical variables")
      )
    }
    for (i in seq_along(x$values)) {
      e <- x$values[[i]]
      current_variable <- names(x$values)[i]
      if (isTRUE(e)) {
        cat(
            crayon::green(
                sprintf("\n`%s`: same variable values",
                        current_variable))
        )
      } else {
        if (!is_empty(e$missing)) {
          cat(
              crayon::italic(
                  sprintf(
                      "\n  * Missing values in `%s`:\n",
                      current_variable))
          )
          print(e$missing)
        }
        if (!is_empty(e$new)) {
          cat(
              crayon::italic(
                  sprintf(
                      "\n  * New values in `%s`:\n",
                      current_variable))
          )
          print(e$new)
        }
        if (!is_empty(e$common)) {
          cat(
              crayon::italic(
                  sprintf(
                      "\n  * `%s`, values common to both datesets:\n",
                      current_variable))
          )
          print(e$common)
        }

      }
    }
  }
  
  cat("\n")  
}
