#' Check and clean spelling or codes of multiple variables in a data frame
#'
#' @param dicts a named list of dictionaries that define dictionaries for
#' specific columns.
#' @param group a character if `dicts` is a data frame, this defines the column
#' to be used for splitting the data frame into groups.
#' @param sort_by a character if `dicts` is a data frame, this defines the column
#' to be used for sorting the values. 
#' @inheritParams clean_variable_labels
#'
#' @return a data frame with re-defined data based on the dictionary 
#' @author Zhian N. Kamvar
#' @export
#' @examples
#' 
#' yesno <- c("Y", "N", "U", NA)
#' dyesno <- c("Yes", "No", "Unknown", "missing")
#' treatment_administered <- c(0:1, NA)
#' dtreatment_administered <- c("Yes", "No", "missing")
#' facility <- 1:10
#' dfacility <- sprintf("Facility %s", format(1:10))
#' age_group <- c(0, 10, 20, 30, 40, 50)
#' dage_group <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50+")
#' 
#' dict <- data.frame(
#'   options = c(yesno, treatment_administered, facility, age_group),
#'   values  = c(dyesno, dtreatment_administered, dfacility, dage_group),
#'   stringsAsFactors = FALSE
#' )
#' dict$grp <- rep(c("readmission", "treatment_administered", "facility", "age_group"),
#'                 c(4, 3, 10, 6))
#'
#' 
#' dat <- data.frame(
#'   readmission = sample(yesno, 20, replace = TRUE),
#'   treatment_administered = sample(treatment_administered, 20, replace = TRUE),
#'   facility = sample(facility, 20, replace = TRUE),
#'   age_group = sample(age_group, 20, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#' 
#' clean_variable_spelling(dat, dicts = dict, group = "grp")

clean_variable_spelling <- function(x = data.frame(), dicts = list(), group = NULL, sort_by = NULL, classes = NULL) {

  if (length(x) == 0 || !is.data.frame(x)) {
    stop("x must be a data frame")
  }

  if (is.null(classes)) {
    classes <- i_find_classes(x)
  }

  if (length(dicts) == 0 || !is.list(dicts)) {
    stop("dicts must be a list of data frames")
  } 

  # There is one big dictionary with groups -----------------------------------
  if (is.data.frame(dicts)) {

    # There is a grouping column ----------------------------------------------
    if (!is.null(group) && group %in% names(dicts)) {
      dicts <- split(dicts, dicts[[group]])
    }

    # There is a column to sort by --------------------------------------------
    if (!is.null(sort_by) && sort_by %in% names(dicts)) {
      if (!is.null(group)) {
        for (i in names(dicts)) {
          dicts[[i]] <- dicts[[i]][order(dicts[[i]]), ]
        }
      } else {
        dicts <- dicts[order(dicts[[sort_by]]), ]
      }
    }
  } else {
    # Not everything is a data frame :( ---------------------------------------
    if (!all(vapply(dicts, is.data.frame, logical(1)))) {
      stop("everything in dicts must be a data frame")
    }

    # Not all dictionaries are named ------------------------------------------
    if (any(names(dicts) == "")) {
      stop("all dictionaries must be named")
    }

    # Some dictionaries aren't in the data ------------------------------------
    if (!all(names(dicts) %in% names(x))) {
      stop("all dictionaries must match a column in the data")
    }
  }
  ddf <- is.data.frame(dicts)
  to_iterate <- if (ddf) names(x) else names(dicts) 

  for (i in to_iterate) {
    d <- if (ddf) dicts else dicts[[i]] 
    tmp <- if (!inherits(x[[i]], c("character", "factor"))) as.character(x[[i]]) else x[[i]]
    try(x[[i]] <- clean_spelling(tmp, d))
  }

  x
}
