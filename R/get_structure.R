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
    stop("get_structure only works with data frames")
  }
  
  x <- as.data.frame(x)
  
  out <- list()

  out$dim     <- dim(x)
  out$names   <- names(x)
  out$classes <- i_find_classes(x)
  categorical <- out$names[out$classes %in% c("factor", "character")]
  out$values  <- lapply(x[categorical], get_categories)
  
  class(out) <- c("data_structure")
  
  out
}
