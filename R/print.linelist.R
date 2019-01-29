#' Function for printing linelist objects
#'
#' The `print` method of `linelist` objects shows *epivars* below the names of
#' the variables.
#'
#' @export
#'
#' @param x a `linelist` object
#'
#' @param ... further arguments passed to `print.data.frame`
#'
#' @param show_epivars a logical indicating whether `epivars` should be shown
#'   after the `data.frame` (`TRUE`, default), or not (`FALSE`); note that
#'   `epivars` are only shown if present
#'
#' @examples
#'
#' x <- messy_data()
#' x <- clean_data(x)
#' x <- as_linelist(x, id = "id", gender = "gender",
#'                  geo = c("lon", "lat"))
#' x
#' print(x, show_epivars = TRUE)

print.linelist <- function(x, ..., show_epivars = FALSE) {
  cat("<linelist object>\n\n")
  epivars <- list_epivars(x)
  NextMethod(x, ...)

  if (!is.null(epivars) && show_epivars) {
    cat("\n // epivars:\n")
    print(epivars)
  }

  return(invisible(NULL))
}
