.onLoad <- function(...) {
  op <- options()
  if (!"epiflows.vars" %in% names(op)) {
    invisible(get_dictionary(reset = TRUE))
  } else {
    invisible(get_dictionary(set = TRUE))
  }
}

.onAttach <- function(...) {
  msg <- sprintf("linelist is loaded with the following global variables in `get_dictionary()`:\n%s",
                 paste(get_dictionary(), collapse = ", ")
  )
  packageStartupMessage(msg)
}
