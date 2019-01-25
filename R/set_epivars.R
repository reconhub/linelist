#' Add epivars to a linelist object
#'
#' These functions can be used to change the `epivars` registred in a `linelist`
#' object. 
#'
#'  - `set_epivars` adds new `epivars` to the existing ones.
#'  - `reset_epivars`  replaces the existing `epivars` with new ones.
#'
#' @param x a `linelist` object
#' @param ... a series of column names named after epivars
#' @return a `linelist` object with new epivars ammended
#' 
#' @export
#' @examples
#' # define some toy data
#' cd <- clean_data(messy_data())
#'
#' # Use as_linelist to define your epivars ---------------------
#' ll <- as_linelist(cd,
#'   "gender"          = "gender",
#'   "id"              = "id",
#'   "geo"             = c("lon", "lat"),
#'   "case_definition" = "epi_case_definition"
#' )
#' list_epivars(ll)
#'
#' # add a new epivar -------------------------------------------
#' ll <- set_epivars(ll, "date_onset" = "date_of_onset")
#' list_epivars(ll)
#'
#' # change an epivar column ------------------------------------
#' # 
#' # in this case, we want to update the date of onset to reflect the number
#' # of days from the start 
#' ll$days_from_start <- as.integer(get_epivars(ll, "date_onset") - as.Date("2018-01-23"))
#' ll <- set_epivars(ll, "date_onset" = "days_from_start")
#' list_epivars(ll)
#'
#' # Remove an epivar label -------------------------------------
#' ll <- set_epivars(ll, "date_onset" = NULL)
#' list_epivars(ll)
set_epivars <- function(x, ...) {
  UseMethod("set_epivars")
}

#' @export
#' @rdname set_epivars
reset_epivars <- function(x, ...) {
  UseMethod("reset_epivars")
}

#' @export
#' @rdname set_epivars
set_epivars.linelist <- function(x, ...) {
  dots        <- valid_dots(list(...))
  epivars     <- update_epivars(attr(x, "epivars"), dots)
  new_epivars <- order_epivars(x, epivars)
  attr(x, "epivars") <- new_epivars
  x
}

#' @export
#' @rdname set_epivars
reset_epivars.linelist <- function(x, ...) {
  attr(x, "epivars") <- NULL
  set_epivars(x, ...)
}

#' @export
#' @rdname set_epivars
set_epivars.data.frame <- function(x, ...) {
  as_linelist(x, ...)
}

#' @export
#' @rdname set_epivars
reset_epivars.data.frame <- function(x, ...) {
  attr(x, "epivars") <- NULL
  as_linelist(x, ...)
}

#' Re-order the epivars in the same order as the data frame
#'
#' Individual linelists may be ordered in any way and it's important to respect
#' the ordering of the linelist. This function ensures that the order of the
#' incoming epivars are in the same order as the incoming linelist.
#'
#' The ONLY exception to this is in cases where there are multiple columns
#' defined by a single epivar such as the "geo" epivar. In this case, the order
#' of the columns in that particular epivar will be preserved, even if it 
#' contradicts the data frame.
#'
#' @param x a data frame
#' @param content a list of epivars that may or may not be in the
#'   same order as `x`
#' @return a reorderd list of epivars 
#' @keywords internal
#' @noRd
#' @importFrom utils unstack
#' @examples
#' dat <- clean_data(messy_data())
#' names(dat) # notice the order of the data
#' ev <- list(
#'   "gender" = "gender",
#'   "id" = "id",
#'   "geo" = c("lon", "lat")
#' )
#' order_epivars(dat, ev)
order_epivars <- function(x, content) {
  if (length(content) == 0) return(content)
  # create a data frame of the incoming epivars
  epivars <- stack(content, stringsAsFactors = FALSE)
  names(epivars) <- c("column", "epivar")
  # ensure the columns are in order
  epivars <- epivars[match(names(x), epivars$column, nomatch = 0), ]
  # Replace factor levels so that the order is preserved in unstacking
  epivars$epivar <- factor(epivars$epivar, unique(epivars$epivar))
  res <- unstack(epivars, column ~ epivar)
  if (is.data.frame(res)) {
    rnames     <- rownames(res)
    res        <- as.list(res[[1]])
    names(res) <- rnames
  }
  # For multi-column epivars, respect the user's decision about the order
  epi_order <- names(lengths(res) > 1)
  res[epi_order] <- content[epi_order]
  res
}

update_epivars <- function(epivars, new_epivars) {
  if (is.null(epivars)){
    epivars <- list()
  }
  for (i in names(new_epivars)) {
    epivars[[i]] <- new_epivars[[i]]
  }
  epivars
}
