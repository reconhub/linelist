#' Compare structures of two datasets
#'
#' This function extracts the structures of two `data.frames` and compares them,
#' issuing a series of diagnostics.
#'
#' @export
#'
#' @author Thibaut Jombart
#'
#' @inheritParams get_structure
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
#' @return an object of class `data_comparison`. This is a named list for
#' each test 
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
#'
#' ## Comparing only specific columns
#'
#' iris1 <- iris2 <- iris
#' iris1$letter <- sample(letters[1:3], nrow(iris), replace = TRUE)
#' iris2$letter <- sample(letters[1:8], nrow(iris), replace = TRUE)
#' compare_data(iris1, iris2, columns = "Species")
#' compare_data(iris, iris2, columns = "Species")
#' compare_data(iris, iris1)
#' compare_data(iris1, iris2)
#' 

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
#' @export
#' @rdname compare_data
#'

compare_data.data_structure <- function(ref, x,
                                        use_dim = TRUE,
                                        use_names = TRUE,
                                        use_classes = TRUE,
                                        use_values = TRUE,
                                        columns = TRUE,
                                        ...) {
  x_str <- get_structure(x, columns = columns)

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

  class(out) <- c("data_comparison")
  out
}




#' @export
#' @rdname compare_data
compare_data.data.frame <- function(ref, x, ...) {
  dots <- list(...)
  cols <- if (is.null(dots$columns)) TRUE else dots$columns
  ref_str <- get_structure(ref, columns = cols)
  compare_data(ref_str, x, ...)
}




#' Compare dimensions
#'
#' Returns `TRUE` if dimensions are the same, and a named list otherwise.
#' @noRd

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
#' @noRd

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
#' @noRd

compare_classes <- function(ref, x) {
  ref_names   <- ref$names
  ref_classes <- ref$classes
  names(ref_classes) <- ref_names

  x_names   <- x$names
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
  out <- list()
  for (i in common_variables) {

    if (ref_classes[i] != x_classes[i]) {
      out[[i]] <- c(
                    variable  = i,
                    ref_class = ref_classes[i],
                    new_class = x_classes[i]
                    )
    } else {
      out[[i]] <- ref_classes[i] 
    }

  }

  out
}




#' Compare values of categorical variables
#'
#' The function returns `NULL` if the categories of categorical variables are
#' the same, and a named list of character strings if there are differences.
#' @noRd

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
    return(list(cannot_compare = TRUE))
  }


  ## general case: comparison for common variables
  out <- list()

  for (i in common_variables) {

    ref_values_current <- ref_values[[i]]
    x_values_current <- x_values[[i]]

    if (!identical(ref_values_current, x_values_current)) {

      out[[i]] <- list(
                    missing = setdiff(ref_values_current, x_values_current),
                    new = setdiff(x_values_current, ref_values_current),
                    common = intersect(x_values_current, ref_values_current))
    } else {
      out[[i]] <- TRUE
    }
  }


  out
}





