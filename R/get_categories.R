#' List unique values of categorical data
#'
#' This function is for internal use only. It extracts and sorts levels of factors, and unique values of characters.
#'

get_categories <- function(x) {
  sort(unique(as.character(x)))
}
