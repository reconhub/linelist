#' Check and clean spelling or codes of multiple variables in a data frame
#'
#' @param wordlists a data frame with at least two columns defining the word
#' list to be used. This data frame should have a `group` column, ideally (see
#' below).
#' @param group a character or integer if `wordlists` is a data frame, this
#' defines the column to be used for splitting the data frame into groups. The
#' default column is the third column of  `wordlists`. *If you want to use a
#' global dictionary*, set `group = NULL`.
#' @param sort_by a character the column to be used for sorting the values in
#' each data frame
#' @inheritParams clean_variable_labels
#'
#' @note This function will only parse character and factor columns to protect
#'   numeric and Date columns from conversion to character. While it is possible,
#'   it is not recommended to use a dictionary without a grouping column 
#'   specifying the columns in the data to work on.
#'
#' @return a data frame with re-defined data based on the dictionary 
#' @author Zhian N. Kamvar
#' @export
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
#'   orders  = c(1:4, 1:3, 1:10, 1:6),
#'   stringsAsFactors = FALSE
#' )
#' wordlist$grp <- rep(c("readmission", "treatment_administered", "facility", "age_group"),
#'                     c(4, 3, 10, 6))
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
#' wordlist # show the wordlist
#' head(dat) # show the data
#' 
#' head(clean_variable_spelling(dat, wordlists = wordlist, group = "grp"))
#' 
#' # You can ensure the order of the factors are correct by specifying 
#' # a column that defines order.
#' dat[] <- lapply(dat, as.factor)
#' as.list(head(dat))
#' res <- clean_variable_spelling(dat, wordlists = wordlist, group = "grp", sort_by = "orders")
#' head(res)
#' as.list(head(res))

clean_variable_spelling <- function(x = data.frame(), wordlists = list(), group = 3, sort_by = NULL, classes = NULL) {

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

  # There is one big dictionary with groups -----------------------------------
  if (is.data.frame(wordlists)) {

    # There is a grouping column ----------------------------------------
    if (!is.null(group) && length(group) == 1) {
      is_number <- is.numeric(group) && as.integer(group) == group && group < ncol(x)
      is_name   <- is.character(group)
      if (is_number || is_name) {
        wordlists <- split(wordlists, wordlists[[group]])
      } else {
        stop("group must be the name or position of a column in the wordlist")
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
