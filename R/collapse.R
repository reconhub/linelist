#' Internal function to collapse text
#'
#' @param x a `character` vector
#'
#' @param sep a separator to be used to collate the text

collapse <- function(x, sep = ", ") {
  paste(x, collapse = ", ")
}

cc <- collapse

