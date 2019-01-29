#' Linelist Global Variables
#' 
#' This defines the legal variables that linelist will recognise.
#' 
#' 
#' @description These functions let the user define variables that are important
#' to a given analysis script. These functions add to or reset variables
#' contained in `getOption("linelist_epivars")`. 
#'
#'  - `set_dictionary()`  will **add** variables to the current defined epivars.
#'     **Use this function at the beginning of your analysis script or Rmd file.**
#'  - `get_dictionary()` shows the current defined epivars that linelist will
#'     recognise.
#'  - `reset_dictionary()` will reset the dictionary to those in 
#'    `default_dictionary()`.
#'
#' All of these functions are wrappers for `linelist_options()`, which itself
#' is a wrapper for `options(linelist_epivars = ...)`. 
#' @author Zhian N. Kamvar, Thibaut Jombart 
#' @seealso [add_epivar()], [add_description()]
#' @export
#' @rdname dictionary
#' @examples 
#' # see the default varaibles
#' print(od <- get_dictionary())
#' 
#' # Equivalent
#' getOption("linelist_dictionary")
#' 
#' 
#' # Set a new, one-column dictionary 
#' hosp <- data.frame(
#'   var  = "date_hospital",
#'   HXL  = "#date +start",
#'   desc = "date at which patient was hospitalized",
#'   stringsAsFactors = FALSE
#' )
#'
#' set_dictionary(hosp, epivar = "var", hxl = "HXL", description = "desc")
#' get_dictionary()
#'
#' # Use the default_dictionary_path() to read in the default dictionary from
#' # disk
#' default_dictionary_path()
#' set_dictionary(default_dictionary_path())
#' get_dictionary()
#'
#' # You can also reset the variables automatically
#' reset_dictionary()
#'
#' # reset dictionary to user's dictionary
#' set_dictionary(od)
default_dictionary <- function() {
  file <- default_dictionary_path()
  utils::read.delim(file, sep = "\t", stringsAsFactors = FALSE)
}

#' @export
#' @rdname dictionary
default_dictionary_path <- function() {
  system.file("default_dictionary.txt", package = "linelist", mustWork = TRUE)
}





#' @export
#' @rdname dictionary
get_dictionary <- function() {
  getOption("linelist_dictionary")
}





#' @export
#' @rdname dictionary
#' @param x either a data frame or the path to a that can produce a data frame.
#' @param epivar the name of the column to use as the epivar column
#' @param hxl the name of the column to use as the hxl column
#' @param description the name of the column to use as the comment column
#' @param ... parameters passed on to [rio::import()].
#' @details The expected format for these data is a`data.frame` with 3 columns
#' 'epivar', 'hxl' and 'description', representing the new dictionary to be
#' used; A template example can be found by using [default_dictionary()]. 
#' @importFrom rio import
set_dictionary <- function(x, epivar = "epivar", hxl = "hxl", description = "description", ...) {
  if (is.character(x)) {
    if (!file.exists(x)) {
      msg <- paste("The linelist dictionary must be either a data frame or a",
                   "path to a file. The file '%s' does not appear to exist.")
      stop(sprintf(msg, x))
    }
    x <- rio::import(x, ...)
  }
  x <- x[c(epivar, hxl, description)]
  names(x)[names(x) == epivar]      <- "epivar"
  names(x)[names(x) == hxl]         <- "hxl"
  names(x)[names(x) == description] <- "description"
  check_dictionary(x)
  for (i in seq_len(ncol(x))) {
    x[[i]] <- as.character(x[[i]])
  }
  options(linelist_dictionary = x)
}





#' @export
#' @rdname dictionary
reset_dictionary <- function() {
  defaults <- default_dictionary()
  options(linelist_dictionary = defaults)
}

## This function performs basic checks on a dictionary. It should be a
## data.frame with 3 columns named 'epivar', 'hxl', and 'description', in this
## order.

check_dictionary <- function(x) {
  if (!is.data.frame(x)) {
    msg <- sprintf("x is not a data frame but a %s",
                   class(x))
    stop(msg)
  }

  if (ncol(x) != 3L) {
    msg <- sprintf("x does not have 3 columns but %d",
                   ncol(x))
    stop(msg)
  }

  expected_names <- c("epivar", "hxl", "description")
  if (!identical(names(x), expected_names)) {
    msg <- sprintf(
        paste0(
            "dictionary does not have the expected column names;",
            "\nexpected: %s",
            "\nfound: %s"),
        paste(expected_names, collapse = ", "),
        paste(names(x), collapse = ", ")
        )
    stop(msg)
  }
  invisible(NULL)
}

#' Add or remove elements from the current dictionary
#'
#' These functions give a finer set of controls to the dictionary, allowing the
#' user to add rows to the dictionary.
#' @param epivar a character describing an epivar or a data frame containing a
#'   dictionary to append to the current dictionary.
#' @param hxl a hxl hashtag and metadata
#' @param description a character string giving a human-readable description of the 
#'   epivar.
#' @rdname add_epivar
#' @export
#' @return NULL, invisibly
#' @examples
#' # preserve current dictionary
#' od <- get_dictionary()
#'
#' # add a single row for "date_hospital"
#' hosp <- data.frame(
#'   epivar = "date_hospital",
#'   hxl    = "#date +start",
#'   description = "date at which patient was hospitalized",
#'   stringsAsFactors = FALSE
#' )
#'
#' add_epivar(hosp)
#' get_dictionary()
#' 
#' # Use add_description to update descriptions
#' add_description("date_hospital", "hospitalisation date")
#' get_dictionary()
#'
#' # You can export your dictionary for later use
#' tmp <- tempdir()
#' rio::export(get_dictionary(), file.path(tmp, "new_dictionary.csv"), format = "|")
#' head(rio::import(file.path(tmp, "new_dictionary.csv"), format = "|"))
#' unlink(file.path(tmp, "new_dictionary.csv")) # cleanup
#'
#' # reset dictionary
#' set_dictionary(od)
add_epivar <- function(epivar = NULL, hxl = "", description = "") {
  if (!is.data.frame(epivar) && !is.character(epivar)) {
    stop("epivar must be a character or a data frame")
  }
  the_dictionary <- get_dictionary()
  if (is.data.frame(epivar)) {
    check_dictionary(epivar)
    res <- vector(mode = "character", length = nrow(epivar))
    names(res) <- epivar$epivar
    for (e in seq(nrow(epivar))) {
      ev   <- epivar$epivar[e]
      hxl  <- epivar$hxl[e]
      desc <- epivar$description[e]
      the_try <- tryCatch(add_epivar(ev, hxl, desc), 
                          error = function(e) e)
      if (inherits(the_try, "simpleError")) {
        res[ev] <- the_try$message
      }
    }
    res <- res[res != ""]
    if (length(res) > 0) {
      msg <- paste("The following epivars already exist in the dictionary:  %s")
      stop(sprintf(msg, paste(names(res), collapse = ", ")))
    } else {
      return(invisible(NULL))
    }
  }
  if (epivar %in% the_dictionary$epivar) {
    stop(sprintf("%s already exists in the dictionary", epivar))
  }
  res <- rbind(the_dictionary, 
               data.frame(epivar, hxl, description, stringsAsFactors = FALSE))
  set_dictionary(res)
}

#' @export
#' @rdname add_epivar
add_description <- function(epivar = NULL, description = NULL) {
  stopifnot(is.character(epivar), is.character(description))
  the_dictionary <- get_dictionary()
  if (!epivar %in% the_dictionary$epivar) {
    tc <- match.call()
    tc[[1]] <- as.name("add_definition")
    tc$hxl  <- ""
    msg <- paste("\"%s\" is not found in the dictionary.",
                 "If you want to add this epivar, use the following command:\n\t%s")
    stop(sprintf(msg, epivar, deparse(tc)))
  }
  the_dictionary$description[the_dictionary$epivar == epivar] <- description
  set_dictionary(the_dictionary)
}
