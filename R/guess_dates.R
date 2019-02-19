#' Try and guess dates from a characters
#'
#' Note that THIS FEATURE IS STILL EXPERIMENTAL: we strongly recommend checking
#' a few converted dates manually. This function tries to extract dates from a
#' `character` vector or a `factor`. It treats each entry independently, using
#' regular expressions to detect if a date is present, its format, and if
#' successful it converts that entry to a standard `Date` with the *Ymd* format
#' (e.g. `2018-01-21`). Entries which cannot be processed result in `NA`. An
#' error threshold can be used to define the maximum number of resulting `NA`
#' (i.e. entries without an identified date) that can be tolerated. If this
#' threshold is exceeded, the original vector is returned.
#'
#'
#' @author Thibaut Jombart
#'
#' @export
#'
#' @details Converting ambiguous character strings to dates is difficult for
#'     many reasons:
#'
#' - dates may not use the standard Ymd format
#' 
#' - within the same variable, dates may follow different formats
#' 
#' - dates may be mixed with things that are not dates
#' 
#' - the behaviour of `as.Date` in the presence of non-date is hard to predict,
#'   sometimes returning `NA`, sometimes issuing an error.
#'
#' This function tries to address all the above issues. Dates with the following
#' format should be automatically detected, irrespective of separators
#' (e.g. "-", " ", "/") and surrounding text:
#'
#' - "19 09 2018"
#' - "2018 09 19"
#' - "19 Sep 2018"
#' - "2018 Sep 19"
#'
#' Note that if a character string has multiple dates, it is currently hard to
#' predict which date will be returned.
#'
#' @param x a `character` vector or a `factor`
#'
#' @param error_tolerance a number between 0 and 1 indicating the proportion of
#'     entries which cannot be identified as dates to be tolerated; if this
#'     proportion is exceeded, the original vector is returned, and a message is
#'     issued; defaults to 0.1 (10 percent)
#'
#' @param first_date a Date object specifying the first valid date. Defaults to
#'   fifty years before the `last_date`.
#'
#' @param last_date a Date object specifying the last valid date. Defaults to the
#'   current date. 
#'
#' @param orders date codes for fine-grained parsing of dates. This allows for
#' parsing of mixed dates. If a list is supplied, that list will be used for
#' successive tries in parsing.  This is passed on to
#' [lubridate::parse_date_time()]. Default orders parse World dmy/dby dates 
#' before US mdy/bdy dates.
#' @param quiet a logical indicating if messages should be displayed to the
#'     console (`TRUE`, default); set to `FALSE` to silence messages
#'
#' @seealso [clean_dates()] for cleaning of data frames
#' 
#' @examples
#' 
#' x <- c("01-12-2001", "male", "female", "2018-10-18", NA, NA, "2018_10_17",
#'       "2018 10 19", "// 24/12/1989", "this is 24/12/1989!",
#'       "RECON NGO: 19 Sep 2018 :)", "6/9/11", "10/10/10")
#' FIRST_DATE <- as.Date("1969-11-11")
#' guess_dates(x, error_tolerance = 1, first_date = FIRST_DATE) # forced conversion
#' guess_dates(x, error_tolerance = 0.15, first_date = FIRST_DATE) # only 15% errors allowed

guess_dates <- function(x, error_tolerance = 0.1, first_date = NULL, 
                        last_date = Sys.Date(), 
                        orders = list(world = c("dby", "dmy", "Ybd", "Ymd"), 
                                      US = c("Omdy", "YOmd")),
                        quiet = TRUE) {

  ## This function tries converting a single character string into a
  ## well-formatted date, but still returning a character. If it can't convert
  ## it, it returns NA.

  # save the original x for later if nothing is converted
  ox <- x

  if (is.factor(x)) {
    x <- as.character(x)
  }

  iso_8601 <- "[0-9]{4}-(0|1(?=[0-2]))[0-9]-([0-2]|3(?=[0-1]))[0-9]"
  if (is.character(first_date) && 
      length(first_date) == 1 && 
      grepl(iso_8601, first_date, perl = TRUE)) {
    first_date <- as.Date(first_date, "%Y-%m-%d")
  }
  if (is.character(last_date) && 
      length(last_date) == 1 && 
      grepl(iso_8601, last_date, perl = TRUE)) {
    last_date <- as.Date(last_date, "%Y-%m-%d")
  }
  if (is.null(first_date) && inherits(last_date, "Date")) {
    first_date <- min(seq.Date(last_date, length.out = 2, by = "-50 years"))
  } 
  if (!inherits(first_date, "Date") || !inherits(last_date, "Date")) {
    stop("first_date and last_date must be Date objects or characters in yyyy-mm-dd format.")
  }
  stopifnot(inherits(first_date, "Date"), inherits(last_date, "Date"))

  if (!is.list(orders) && is.character(orders)) {
    orders <- list(orders)
  } 
  if (!is.list(orders)) {
    stop("orders must be a list of character vectors")
  }
  ## convert all entries to character strings
  x_test <- data.frame(lapply(orders, find_and_constrain_date, x), stringsAsFactors = FALSE)
  good_and_bad <- constrain_date(x_test, first_date, last_date, x)
  bad_dates    <- good_and_bad$bad_dates
  bd <- do.call("c", unname(bad_dates))
  if (!all(is.na(bd))) {
    bd <- utils::stack(bd)
    bd$ind <- as.character(bd$ind)
    bd <- unique(bd)
    misses <- sprintf("  %s  |  %s", 
                      format(c("original", "-----   ", bd$values)), 
                      format(c("parsed", "-----   ", bd$ind))
                      )
    misses <- paste(misses, collapse = "\n")
    msg    <- paste0("\nThe following dates were not in the correct timeframe",
                    " (%s -- %s):\n\n",
                    misses
                   )
    warning(sprintf(msg, first_date, last_date))
  }
  new_x        <- choose_first_good_date(good_and_bad$good_dates)

  ## check how successful we were
  na_before <- sum(is.na(x))
  na_after <- sum(is.na(new_x))
  prop_successful <- (length(x) - na_after) / (length(x) - na_before)

  ## shape result depending on whether conversion was successful
  if (prop_successful < (1 - error_tolerance)) {
    return(ox)
  } else {
    return(as.Date(new_x))
  }
}

find_and_constrain_date <- function(orders = NULL, x) {
  suppressWarnings(as.Date(lubridate::parse_date_time(x, orders = orders)))
}

constrain_date <- function(date_a_frame, dmin, dmax, original_dates) {
  bad_date_list <- lapply(date_a_frame, function(i) setNames(original_dates, as.character(i))[i < dmin | i > dmax])
  for (i in names(date_a_frame)) {
    tmp <- date_a_frame[[i]]
    date_a_frame[[i]][tmp < dmin | tmp > dmax] <- NA 
  }
  list(good_dates = date_a_frame, bad_dates = bad_date_list)
}

choose_first_good_date <- function(date_a_frame) {
  res <- rep(as.Date(NA), length = nrow(date_a_frame))
  for (i in seq_len(nrow(date_a_frame))) {
    tmp <- date_a_frame[i, ]
    res[i] <- tmp[!is.na(tmp)][1]
  }
  res
}
