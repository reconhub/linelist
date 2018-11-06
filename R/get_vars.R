#' Access linelist metadata
#'
#' @rdname get_vars
#' @md
#' @param x A `linelist` object.
#'
#' @export
#'
#' @author Zhian Kamvar
#' @return A data frame with the variables requested
get_vars <- function(x, ...) {
  UseMethod("get_vars", x)
}

#' @rdname get_vars
#'
#' @export
#'
#' @param what a valid character string specifying the variable desired. If
#'   `NULL` (default), the names of the available vars will be returned.
#' @param id a logical. If `TRUE` (default), the `id` column of the locations
#'   will be the first column of the data frame. if `FALSE`, the variable will
#'   be returned with identifiers as row names.
#' @param vector if `TRUE` the result will be coerced into a vector (or a matrix in the case of coordinates)
get_vars.linelist <- function(x, what = NULL, id = FALSE, vector = TRUE, ...) {
  evars <- attr(x, "epivars")
  xname <- names(x)
  if (is.null(what)) {
    evars
  }
  if (!what %in% names(evars)) {
    if (!what %in% xname) {
      available_vars <- paste(xname[-1], collapse = " ")
      msg <- paste("%s does not appear to be in the locations data.\n",
                   "\nThe variables present are:\n%s")
      msg <- sprintf(msg, what, available_vars)
      stop(msg)
    }
  } else {
    what <- evars[[what]]
    what <- if (is.numeric(what)) xname[what] else what
  }
  if (id) {
    res <- x[c("id", what)]
  } else {
    res           <- x[what]
    rownames(res) <- x$id
  }
  if (vector) {
    if (id) {
      rownames(res) <- res$id
      res           <- res[, -1, drop = FALSE]
    }
    res <- as.matrix(res)
    res <- if (ncol(res) == 1) drop(res) else res
  }
  res
}
