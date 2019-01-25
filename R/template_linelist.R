#' Generate a template for linelist construction
#'
#' @param x a data frame of your linelist (optional)
#' @return a printed template that you can use to define the linelist variables
#'   for your data 
#' @export
#' @examples
#' dat <- clean_data(messy_data())
#' template_linelist(dat)
template_linelist <- function(x = NULL) {
  d    <- get_dictionary()
  hd   <- "ll <- as_linelist(x = "
  hd   <- if (!is.null(x)) paste0(hd, deparse(substitute(x)), ",") else hd
  vars <- sprintf("  %s =    ,", format(d$epivar))
  foot <- "  NULL  # don't delete me\n)"
  cat(hd, vars, foot, sep = "\n")
}


