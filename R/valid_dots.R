#' Terminates the workflow and throws an error
#'
#' @param dots a named list
#'
#' @return a named list
#' @noRd
valid_dots <- function(dots) {
  if (length(dots) == 0) return(dots)

  current_dict <- get_dictionary()
  
  # These names can be expanded
  out <- dots[names(dots) %in% current_dict$epivar]

  # TODO: FIX THIS PART
  #
  # TODO: Throw error if there are any unnamed arguments
  #
  # There were dots supplied that don't match what's in the dictionary
  if (length(out) < length(dots)) {
    diffnames <- paste(setdiff(names(dots), names(out)), collapse = "', '")
    msg <- paste0("Unknown variables were found: '", diffnames, "'\n\n",
                  "Please inspect them to make sure they are correct.",
                  "If they are correct, please add them using:\n\n",
                  "\tset_dictionary('", diffnames, "')\n\n",
                  "Type ?set_dictionary for details")
    stop(msg, call. = FALSE)
  }

  out
}
