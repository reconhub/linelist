#' Check and clean spelling or codes of multiple variables in a data frame
#'
#' @param wordlists a data frame or list of named data frames with at least two
#'   columns defining the word list to be used. If this is a data frame, a third
#'   column must be present to split the wordlists by column in `x` (see
#'   `spelling_vars`).
#'
#' @param spelling_vars character or integer. If `wordlists` is a data frame,
#'   then this column in defines the columns in `x` corresponding to each
#'   section of the `wordlists` data frame. This defaults to `3`, indicating the
#'   third column is to be used. _If you want to use a global dictionary, 
#'   use `spelling_vars = NULL`_, but be aware that you may end up with several
#'   warnings if variables do not appear in some columns of `x`. 
#'
#' @param sort_by a character the column to be used for sorting the values in
#'   each data frame. If the incoming variables are factors, this determines how
#'   the resulting factors will be sorted.
#' 
#' @inheritParams clean_variable_labels
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
#' yesno <- c("Y", "N", "U", NA)
#' dyesno <- c("Yes", "No", "Unknown", "missing")
#' treatment_administered <- c(0:1, NA)
#' dtreatment_administered <- c("Yes", "No", "missing")
#' facility <- 1:10
#' dfacility <- sprintf("Facility %s", format(1:10))
#' age_group <- c(0, 10, 20, 30, 40, 50)
#' dage_group <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50+")
#' 
#' wordlist <- data.frame(
#'   options = c(yesno, treatment_administered, facility, age_group),
#'   values  = c(dyesno, dtreatment_administered, dfacility, dage_group),
#'   grp = rep(c("readmission", "treatment_administered", "facility", "age_group"),
#'             c(4, 3, 10, 6)),
#'   orders  = c(1:4, 1:3, 1:10, 1:6),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Generate example data ------------------------------------------
#' dat <- data.frame(
#'   readmission = sample(yesno, 50, replace = TRUE),
#'   treatment_administered = sample(treatment_administered, 50, replace = TRUE),
#'   facility = sample(facility, 50, replace = TRUE),
#'   age_group = sample(age_group, 50, replace = TRUE),
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
#' dat[] <- lapply(dat, as.factor)
#' as.list(head(dat))
#' res <- clean_variable_spelling(dat, wordlists = wordlist, spelling_vars = "grp", sort_by = "orders")
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
    # Iterate over the names of the dictionaries -----------
    to_iterate <- names(wordlists)
  }

  # Loop over the variables and clean spelling --------------------------------
  for (i in to_iterate) {
    d <- if (ddf) wordlists else wordlists[[i]] 
    try(x[[i]] <- clean_spelling(x[[i]], d))
  }

  x
}
