#' Check and clean spelling or codes of multiple variables in a data frame
#'
#' @param wordlists a data frame or named list of data frames with at least two
#'   columns defining the word list to be used. If this is a data frame, a third
#'   column must be present to split the wordlists by column in `x` (see
#'   `spelling_vars`).
#'
#' @param spelling_vars character or integer. If `wordlists` is a data frame,
#'   then this column in defines the columns in `x` corresponding to each
#'   section of the `wordlists` data frame. This defaults to `3`, indicating the
#'   third column is to be used.
#'
#' @param sort_by a character the column to be used for sorting the values in
#'   each data frame. If the incoming variables are factors, this determines how
#'   the resulting factors will be sorted.
#' 
#' @inheritParams clean_variable_labels
#' 
#' @description This function allows you to clean your data according to 
#' pre-defined rules encapsulated in either a data frame or list of data frames.
#' It has application for addressing mis-spellings and recoding variables (e.g.
#' from electronic survey data). 
#'
#' By default, this applies the function [clean_spelling()] to all columns 
#' specified by the column names listed in `spelling_vars`, or, if a global
#' dictionary is used, this includes all `character` and `factor` columns as
#' well. 
#'
#' \subsection{Global wordlists}{
#' 
#' A global wordlist is a set of definitions applied to all valid columns of `x`
#' indiscriminantly. When a global dictioary is used, you might see several
#' warnings appear because some variables were not found in the data.
#'
#'  - **.global spelling_var**: If you want to apply a set of definitions to all
#'     valid columns in addition to specified columns, then you can include a
#'     `.global` group in the `spelling_var` column of your `wordlists` data
#'     frame. *NOTE: specific variable definitions will override global
#'     defintions.* For example: if you have a column for cardinal directions
#'     and a definiton for `N = North`, then the global variable `N = no` will
#'     not override that. See Example.
#'
#'  - **`spelling_var = NULL`**: If you want your data frame to be applied to
#'    all character/factor columns indiscriminantly, then setting 
#'    `spelling_var = NULL` will use that wordlist globally.
#'
#' }
#'
#' 
#' @note This function will only parse character and factor columns to protect
#'   numeric and Date columns from conversion to character. 
#'
#' @return a data frame with re-defined data based on the dictionary 
#'
#' @seealso [clean_spelling()], which this function wraps.
#'
#' @author Zhian N. Kamvar
#'
#' @export
#'
#' @examples
#' 
#' # Set up wordlist ------------------------------------------------ 
#'
#' yesno  <- c("Y", "N", "U", NA)
#' dyesno <- c("Yes", "No", "Unknown", "Missing")
#'
#' treatment_administered  <- c(0:1, NA)
#' dtreatment_administered <- c("Yes", "No", "Missing")
#'
#' facility  <- c(1:10, ".default") # define a .default key
#' dfacility <- c(sprintf("Facility %s", format(1:10)), "Unknown")
#'
#' age_group  <- c(0, 10, 20, 30, 40, 50)
#' dage_group <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50+")
#'
#' wordlist <- data.frame(
#'   options = c(yesno, treatment_administered, facility, age_group),
#'   values  = c(dyesno, dtreatment_administered, dfacility, dage_group),
#'   grp = rep(c("readmission", "treatment_administered", "facility", "age_group"),
#'             c(4, 3, 11, 6)),
#'   orders  = c(1:4, 1:3, 1:11, 1:6),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Assigning global values ----------------------------------------
#'
#' global_words <- data.frame(
#'   options = c("Y", "N", "U", "unk", "oui", NA),
#'   values  = c("yes", "no", "unknown", "unknown", "yes", "missing"),
#'   grp     = rep(".global", 6),
#'   orders  = rep(Inf, 6),
#'   stringsAsFactors = FALSE
#' )
#' 
#' wordlist <- rbind(wordlist, global_words, stringsAsFactors = FALSE)
#'
#' # Generate example data ------------------------------------------
#' dat <- data.frame(
#'   # these have been defined
#'   readmission = sample(yesno, 50, replace = TRUE),
#'   treatment_administered = sample(treatment_administered, 50, replace = TRUE),
#'   facility = sample(c(facility[-11], LETTERS[1:3]), 50, replace = TRUE),
#'   age_group = sample(age_group, 50, replace = TRUE),
#'   # global values will catch these
#'   has_symptoms = sample(c(yesno, "unk", "oui"), 50, replace = TRUE),
#'   followup = sample(c(yesno, "unk", "oui"), 50, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Clean spelling based on wordlist ------------------------------ 
#'
#' wordlist # show the wordlist
#' head(dat) # show the data
#' 
#' head(clean_variable_spelling(dat, wordlists = wordlist, spelling_vars = "grp"))
#' 
#' # You can ensure the order of the factors are correct by specifying 
#' # a column that defines order.
#'
#' dat[] <- lapply(dat, as.factor)
#' as.list(head(dat))
#' res <- clean_variable_spelling(dat, 
#'                                wordlists = wordlist, 
#'                                spelling_vars = "grp", 
#'                                sort_by = "orders")
#' head(res)
#' as.list(head(res))

clean_variable_spelling <- function(x = data.frame(), wordlists = list(), spelling_vars = 3, sort_by = NULL, classes = NULL) {

  if (length(x) == 0 || !is.data.frame(x)) {
    stop("x must be a data frame")
  }

  if (is.null(classes)) {
    classes <- i_find_classes(x)
  }

  # Define columns viable for manipulation ------------------------------------
  # Because this is a global manipulator, only work on characters or factors
  unprotected <- names(x)[classes %in% c("character", "factor")]

  if (length(wordlists) == 0 || !is.list(wordlists)) {
    stop("wordlists must be a list of data frames")
  } 

  # There is one big dictionary with spelling_varss -----------------------------------
  if (is.data.frame(wordlists)) {

    # There is a spelling_varsing column ----------------------------------------
    if (!is.null(spelling_vars) && length(spelling_vars) == 1) {
      is_number <- is.numeric(spelling_vars) &&          # spelling_vars is a number
                   as.integer(spelling_vars) == spelling_vars && # ... and an integer
                   spelling_vars <= ncol(wordlists)      # ... and is within the bounds

      is_name   <- is.character(spelling_vars) &&         # spelling_vars is a name
                   any(names(wordlists) == spelling_vars) # ... in the wordlists
      if (is_number || is_name) {
        wordlists <- split(wordlists, wordlists[[spelling_vars]])
      } else {
        stop("spelling_vars must be the name or position of a column in the wordlist")
      }
    } else {
      warning("Using wordlist globally across all character/factor columns.")
    }
  } else {
    # Not everything is a data frame :( ---------------------------------------
    if (!all(vapply(wordlists, is.data.frame, logical(1)))) {
      stop("everything in wordlists must be a data frame")
    }

    # Not all dictionaries are named ------------------------------------
    if (any(names(wordlists) == "")) {
      stop("all dictionaries must be named")
    }

    # Some dictionaries aren't in the data ------------------------------
    if (!all(names(wordlists) %in% unprotected)) {
      stop("all dictionaries must match a column in the data")
    }
  }

  ddf            <- is.data.frame(wordlists)
  exists_sort_by <- !is.null(sort_by)

  if (ddf) {
    # If there is one big dictionary ------------------------------------
    if (exists_sort_by && sort_by %in% names(wordlists)) {
      wordlists <- wordlists[order(wordlists[[sort_by]]), , drop = FALSE]
    }
    # Iterate over the names of the data -------------------
    to_iterate <- unprotected
  } else {
    # If there is a list of dictionaries --------------------------------
    if (exists_sort_by) {
      for (i in names(wordlists)) {
        di <- wordlists[[i]]
        # Only sort if there is something to sort by -------
        the_sorts  <- if (any(names(di) == sort_by)) order(di[[sort_by]]) else TRUE
        wordlists[[i]] <- wordlists[[i]][the_sorts, , drop = FALSE]
      }
    }
    global_words <- wordlists[[".global"]]
    wordlists    <- wordlists[names(wordlists) != ".global"]
    has_global   <- !is.null(global_words)
    if (has_global) {
      for (i in names(wordlists)) {
        # Append the global dictionary to the specific one -
        wordlists[[i]] <- rbind(wordlists[[i]], global_words, 
                                stringsAsFactors = FALSE,
                                make.row.names = FALSE)
        # The specific values override the global values --
        wordlists[[i]] <- wordlists[[i]][!duplicated(wordlists[[i]][1]), ,drop = FALSE]
      }
    }
    # Iterate over the names of the dictionaries -----------
    to_iterate <- names(wordlists)
    if (has_global) {
      to_iterate <- unique(c(to_iterate, unprotected))
    }
  }

  # Loop over the variables and clean spelling --------------------------------
  for (i in to_iterate) {
    d <- if (ddf) wordlists else wordlists[[i]] 
    d <- if (is.null(d)) global_words else d
    try(x[[i]] <- clean_spelling(x[[i]], d))
  }

  x
}
