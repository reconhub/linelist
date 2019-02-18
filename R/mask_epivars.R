#' Reversibly rename columns in your linelist with epivars
#'
#' @param x a linelist object
#' @return
#'  - `mask()` a linelist object with columns renamed by their 
#'    corresponding epivars and an attribute called "masked-linelist" containing
#'    a vector of the original column names named by their corresponding epivars
#'  - `unmask()` a linelist object
#'
#' @rdname mask
#' @export
#' @author Zhian N. Kamvar
#' @examples
#' md <- messy_data(10)
#' cd <- clean_data(md)
#' ll <- as_linelist(cd, 
#'                   id = "id", 
#'                   date_onset = "date_of_onset", 
#'                   gender = "gender",
#'                   case_definition = "epi_case_definition",
#'                   geo_lon = "lon",
#'                   geo_lat = "lat"
#'                  )
#' ll
#' 
#' # masking epivars changes the column names
#' print(llm <- mask(ll))
#' 
#' # unmasking reverses the operation
#' unmask(llm)
#' 
#' # you can safely drop columns
#' unmask(llm[grepl("gender|geo", names(llm))])
mask <- function(x) {
  UseMethod("mask")
}

#' @export
#' @rdname mask
mask.default <- function(x) {
  cls <- class(x)
  stop(sprintf("There is no mask method defined for an object of class %s", cls))
}

#' @export
#' @rdname mask
mask.linelist <- function(x) {
  if (!is.null(attr(x, "masked-linelist"))) return(x)
  ev <- list_epivars(x)
  ev$epivar <- as.character(ev$epivar)
  if (any(duplicated(ev$epivars))) {
    dups <- duplicated(ev$epivar) & duplicated(ev$epivar, fromLast = TRUE)
    culprits <- ev$columns[dups]
    the_evs  <- ev$epivars[dups]
    bad_thng <- "mask only works if each epivar has a unique column. These epivars had multiple matching columns"
    msg      <- sprintf("(%s, %s)", format(the_evs), format(culprits))
    msg      <- paste(msg, collapse = ", ")
    msg      <- paste(bad_thng, msg)
    stop(msg)
  }
  xnam <- names(x)
  matches <- match(ev$column, xnam, nomatch = 0)
  attr(x, "masked-linelist") <- setNames(xnam[matches], ev$epivar)
  attr(x, "epivars") <- as.list(setNames(ev$epivar, ev$epivar))
  xnam[matches] <- ev$epivar
  names(x) <- xnam
  x
}


#' @export
#' @rdname mask
unmask <- function(x) {
  UseMethod("unmask")
}

#' @export
#' @rdname mask
unmask.default <- function(x) {
  cls <- class(x)
  stop(sprintf("There is no unmask method defined for an object of class %s", cls))
}

#' @rdname mask
#' @export
unmask.linelist <- function(x) {
  ml <- attr(x, "masked-linelist")
  if (is.null(ml)) {
    stop(sprintf("%s has not been masked!", deparse(substitute(x))))
  }
  # get the current names 
  xnam <- names(x)
  
  # Find out where the original names should be
  matches    <- match(names(ml), xnam, nomatch = 0)
  ev_matches <- names(ml) %in% xnam
  
  # Trim out any missing columns
  new_epivars <- names(ml[ev_matches])

  # return the original names to their rightful palce
  xnam[matches] <- ml[ev_matches]
  names(x)      <- xnam

  # create the new epivars 
  attr(x, "epivars") <- as.list(setNames(ml[ev_matches], new_epivars))
  attr(x, "epivars") <- order_epivars(x, content = attr(x, "epivars"))

  # banish the mask
  attr(x, "masked-linelist") <- NULL
  x
}

#' @rdname mask
#' @export
unmask.data.frame <- function(x) {
  if (!is.null(attr(x, "masked-linelist")) && !is.null(attr(x, "epivars"))) {
    class(x) <- c("linelist", oldClass(x))
    return(unmask.linelist(x))
  } else {
    warning("Only masked linelist objects can be unmasked")
    return(x) 
  }
}
