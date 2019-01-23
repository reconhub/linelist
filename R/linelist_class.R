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
#' @seealso [get_epivars()], [epivars()], [list_epivars()], [clean_data()]
#' @export
#' @examples
#' md <- messy_data(10)
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


#' @rdname as_linelist
#' @export
#' @param x a linelist object
#' @param i indicator for rows
#' @param j indicator for columns
#' @param drop indicator for whether the data frame should be dropped if reduced
#'   to one column (defaults to FALSE)
"[.linelist" <- function(x, i, j, drop = FALSE) {
  md <- list_epivars(x)
  ev <- attr(x, "epivars")
  x  <- NextMethod()
  newmd <- md[md$column %in% names(x), ]
  ev$vars <- ev$vars[names(ev$vars) %in% newmd$epivar[!is.na(newmd$epivar)]]
  ev$meta <- newmd[colnames(newmd) != "epivar"]
  attr(x, "epivars") <- ev
  x
}
