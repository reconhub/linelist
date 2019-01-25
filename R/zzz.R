.onLoad <- function(...) {
  op <- options()
  if (!"linelist_dictionary" %in% names(op)) {
    invisible(reset_dictionary())
  }
  ## TODO: do something if the user has pre-loaded a custom dictionary
}

.onAttach <- function(...) {
  ## msg <- sprintf("linelist is loaded with the following global variables in `get_dictionary()`:\n%s",
  ##                paste(get_dictionary(), collapse = ", ")
  ## )
  ## packageStartupMessage(msg)
}
