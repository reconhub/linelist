.onLoad <- function(...) {
  ## TODO: do something if the user has pre-loaded a custom dictionary
}

.onAttach <- function(...) {
  op <- options()
  if (!"linelist_dictionary" %in% names(op)) {
    invisible(reset_dictionary())
  }
}
