#' Accessor functfons
#'
#' This set of fucntions defines helper functions for accessing pre-defined
#' epivars in your linelist object.
#'
#' @param x a linelist object
#' @param var the name of a variable as a string
#' @param simplify if `TRUE` (default) and if the result is a single column,
#'   then this variable will be returned as a `vector`; otherwise (`FALSE`), a
#'   `data.frame` is returned.
#' @return a vector if the epivar can be represented by a single column, a data
#'   frame otherwise (e.g. a location column).
#' @export
#' @rdname accessors
#' @examples
#'
#' ## make toy data
#' toy_data <- messy_data()
#' cleaned_data <- clean_data(toy_data)
#' ll <- as_linelist(cleaned_data, 
#'                   id = "id", 
#'                   date_onset = "date_of_onset",
#'                   gender = "gender",
#'                   geo = c("lon", "lat"))
#'
#' ## general purpose accessor
#' get_vars(ll) # no epi variable
#' get_vars(ll, 1:3) # first 3 epi variables
#' get_vars(ll, "id", "date_onset", "gender") # named epi variables
#' get_vars(ll, TRUE) # all epi variables
#' date_of_onset(ll)
#' id(ll)
#' gender(ll)
#' geo(ll)
#' get_var(ll, "geo", vector = FALSE)
#' # epivars that haven't been defined for the data set will return an error
#' try(date_report(ll))


#' @rdname accessors
#' @export
get_vars <- function(x, ..., simplify = TRUE) {
  ## TODO: this is lacking a validation step, which should throw informative
  ## errors if ... are not valid epivars subsets
  vars <- unlist(list(...))
  epivars <- attr(x, "epivars")$vars
  to_keep <- unlist(epivars[vars])
  out <- as.data.frame(x[to_keep])
  if (simplify && length(to_keep) == 1L) {
    out <- out[, 1, drop = TRUE]
  }
  out
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
