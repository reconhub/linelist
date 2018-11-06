.onLoad <- function(...) {
  op <- options()
  if (!"epiflows.vars" %in% names(op)) {
    invisible(epivars(reset = TRUE))
  } else {
    invisible(epivars(set = TRUE))
  }
}

.onAttach <- function(...) {
  msg <- sprintf("linelist is loaded with the following global variables in `epivars()`:\n%s",
                 paste(epivars(), collapse = ", ")
  )
  packageStartupMessage(msg)
}