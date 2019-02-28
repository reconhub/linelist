#' Extract the structure of a dataset
#'
#' This function extracts the structure and content of a `data.frame`, storing
#' the following information:
#' 
#' - names and order of columns
#' - class of the columns
#' - values of the columns, for `factors` and `characters`
#' 
#'
#' @export
#'
#' @author Thibaut Jombart
#'
#' @param x a `data.frame` to be checked
#'
#' @return A list containing:
#' - `dim`: the dimensions of the dataset (rows, columns)
#' 
#' - `names`: the names of the dataset
#'
#' - `classes`: the corresponding classes (if a column has several classes, only
#' the first one is kept)
#'
#' - `values`: the list of sorted, unique values for each categorical column
#' (`factor` or `character`)
#' 
#' @examples
#' 
#' head(iris)
#' get_structure(iris)

get_structure <- function(x) {
  if (!is.data.frame(x)) {
    warning("converting 'x' to a data.frame")
  }
  
  x <- as.data.frame(x)
  
  out <- list()

  out$dim <- dim(x)
  out$names <- names(x)
  out$classes <- vapply(x, function(e) class(e)[1], character(1))
  categorical <- out$names[out$classes %in% c("factor", "character")]
  out$values <- lapply(
      x[categorical],
      function(e)
        if(is.factor(e)) levels(e) else unique(e))
  out$values <- lapply(out$values, sort)
  
  class(out) <- c("data_structure", "list")
  out
}
