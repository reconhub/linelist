.onLoad <- function(...) {
  op <- options()
  if (!"epiflows.vars" %in% names(op)) {
    invisible(reset_dictionary())
  } else {
    invisible(set_dictionary())
  }
}

.onAttach <- function(...) {
  msg <- sprintf("linelist is loaded with the following global variables in `get_dictionary()`:\n%s",
                 paste(get_dictionary(), collapse = ", ")
  )
  packageStartupMessage(msg)
}
