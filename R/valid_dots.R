#' Terminates the workflow and throws an error
#'
#' @param dots a named list
#'
#' @return a named list
#' @noRd
valid_dots <- function(dots) {
  if (length(dots) == 0) return(dots)
  # These names can be expanded
  out <- dots[names(dots) %in% getOption("linelist_epivars")]
  #
  # TODO: Throw error if there are any unnamed arguments
  #
  if (length(out) < length(dots)) {
    diffnames <- paste(setdiff(names(dots), names(out)), collapse = "', '")
    msg <- paste0("Unknown variables were found: '", diffnames, "'\n\n",
                  "Please inspect them to make sure they are correct.",
                  "If they are correct, please add them using:\n\n",
                  "\tget_dictionary('", diffnames, "', set = TRUE)\n\n",
                  "Type ?epivars for details")
    stop(msg, call. = FALSE)
  }
  out
}
