#' Check and clean spelling of multiple variables in a data frame
#'
#' @param x a data frame
#' @param dicts a named list of dictionaries that define dictionaries for
#' specific columns.
#'
#' @return a data frame with re-defined data

clean_variable_spelling <- function(x = data.frame(), dicts = list()) {
  
  if (length(x) == 0 || !is.data.frame(x)) {
    stop("x must be a data frame")
  }
  
  if (length(dicts) == 0 || !is.list(dicts)) {
    stop("dicts must be a list of data frames")
  } 

  if (!all(vapply(dicts, is.data.frame, logical(1)))) {
    stop("everything in dicts must be a data frame")
  }

  if (any(names(dicts) == "")) {
    stop("all dictionaries must be named")
  }

  if (!all(names(dicts) %in% names(x))) {
    stop("all dictionaries must match a column in the data")
  }

  for (i in names(dicts)) {
    try(x[[i]] <- clean_spelling(x[[i]], dicts[[i]]))
  }

  x
}
