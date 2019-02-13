#' Reversibly rename columns in your data frame with epivars
#'
#' @param x a linelist object
#' @return
#'  - `mask_epivars()` a linelist object with columns renamed by their 
#'    corresponding epivars and an attribute called "masked-linelist" containing
#'    a vector of the original column names named by their corresponding epivars
#'  - `unmask_epivars()` a linelist object
#'
#' @rdname mask_epivars
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
#' print(llm <- mask_epivars(ll))
#' 
#' # unmasking reverses the operation
#' unmask_epivars(llm)
#' 
#' # you can safely drop columns
#' unmask_epivars(llm[grepl("gender|geo", names(llm))])
mask_epivars <- function(x) {
  ev <- list_epivars(x)
  ev$epivar <- as.character(ev$epivar)
  if (any(duplicated(ev$epivars))) {
    dups <- duplicated(ev$epivar) & duplicated(ev$epivar, fromLast = TRUE)
    culprits <- ev$columns[dups]
    the_evs  <- ev$epivars[dups]
    msg      <- sprintf("(%s, %s)", format(the_evs), format(culprits))
    msg      <- paste(msg, collapse = ", ")
    stop(msg)
  }
  xnam <- names(x)
  matches <- match(ev$column, xnam, nomatch = 0)
  attr(x, "masked-linelist") <- setNames(ev$epivar, xnam[matches])
  attr(x, "epivars") <- as.list(setNames(ev$epivar, ev$epivar))
  xnam[matches] <- ev$epivar
  names(x) <- xnam
  x
}

#' @rdname mask_epivars
#' @export
unmask_epivars <- function(x) {
  ml <- attr(x, "masked-linelist")
  if (is.null(ml)) {
    stop(sprintf("%s has not been masked!", deparse(substitute(x))))
  }
  xnam <- names(x)
  matches <- match(ml, xnam, nomatch = 0)
  new_epivars <- ml[ml %in% xnam]
  attr(x, "epivars") <- as.list(setNames(names(new_epivars), new_epivars))
  xnam[matches] <- names(ml[ml %in% xnam])
  names(x) <- xnam
  attr(x, "masked-linelist") <- NULL
  x
}
