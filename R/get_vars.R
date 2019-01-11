#' Accessor functfons
#'
#' This set of fucntions defines helper functions for accessing pre-defined
#' epivars in your linelist object.
#'
#' @param x a linelist object
#' @param var the name of a variable as a string
#' @param vector if `TRUE` (default), the result will be a vector (or a matrix
#'   if it's in two columns). When `FALSE`, a data frame is returned.
#' @return a vector if the epivar can be represented by a single column, a data
#'   frame otherwise (e.g. a location column).
#' @export
#' @rdname accessors
#' @examples
#' 
#' toy_data <- messy_data()
#' cleaned_data <- clean_data(toy_data)
#' ll <- as_linelist(cleaned_data, 
#'                   id = "id", 
#'                   date_onset = "date_of_onset",
#'                   gender = "gender",
#'                   geo = c("lon", "lat"))
#' date_of_onset(ll)
#' id(ll)
#' gender(ll)
#' geo(ll)
#' get_var(ll, "geo", vector = FALSE)
#' # epivars that haven't been defined for the data set will return an error
#' try(date_report(ll))
get_var <- function(x, var, vector = TRUE) {
  evars <- attr(x, "epivars")$vars
  if (is.null(evars)) {
    stop("This object has no epivars attribute.")
  }
  if (is.null(evars[[var]])) {
    stop(paste(var, "not defined."))
  }
  doname <- evars[[var]]
  if (vector) {
    if (length(doname) == 1) {
      x[[doname]]
    } else {
      as.matrix(x[doname])
    }
  } else {
    as.data.frame(x[doname])
    
  }
}

#' @rdname accessors
#' @export
date_of_onset <- function(x) {
  get_var(x, "date_onset")
}

#' @rdname accessors
#' @export
date_onset <- date_of_onset

#' @rdname accessors
#' @export
id <- function(x) {
  get_var(x, "id")
}

#' @rdname accessors
#' @export
gender <- function(x) {
  get_var(x, "gender")
}

#' @rdname accessors
#' @export
date_report <- function(x) {
  get_var(x, "date_report")
}

#' @rdname accessors
#' @export
age <- function(x) {
  get_var(x, "age")
}

#' @rdname accessors
#' @export
age_group <- function(x) {
  get_var(x, "age_group")
}

#' @rdname accessors
#' @export
geo <- function(x) {
  get_var(x, "geo")
}
