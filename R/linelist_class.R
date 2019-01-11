# It should also have accessor functions for standard columns:
#   
#  - date of onset
#  - date of report
#  - date of x where x in {death, hospitalization, ...}
#  - sex
#  - age
#  - age group
#  - a function that determines which concepts are available
#  - arbitrary query function based on HXL?

# Convert a data frame to a linelist via as_linelist

#' Create a linelist object
#'
#' @param dat a data frame
#' @param ... options passed to [set_vars()]
#' @export
#' @examples
#' md <- messy_data()
#' cd <- clean_data(md)
#' ll <- as_linelist(cd, 
#'                   id = "id", 
#'                   date_onset = "date_of_onset", 
#'                   gender = "gender",
#'                   geo = c("lon", "lat")
#'                  )
#' ll
#' class(ll)
as_linelist <- function(dat, ...) {
  UseMethod("as_linelist")
}

#' @rdname as_linelist
#' @export
#' @aliases as_linelist.default
as_linelist.default <- function(dat, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(dat), collapse = ", ")))
}

#' @rdname as_linelist
#' @export
#' @aliases as_linelist.default
as_linelist.data.frame <- function(dat, ...) {
  dots <- list(...)
  meta <- i_make_meta(dat)
  attr(dat, "epivars") <- list(vars = list(), meta = meta)
  class(dat) <- c("linelist", oldClass(dat))
  set_vars(dat) <- dots
  dat
}
