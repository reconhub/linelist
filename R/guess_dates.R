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
#' @author Thibaut Jombart, Zhian N. Kamvar
#'
#' @export
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
#'   parsing of mixed dates. If a list is supplied, that list will be used for
#'   successive tries in parsing.  This is passed on to
#'   [lubridate::parse_date_time()]. Default orders
#'   (`getOption("linelist_guess_orders")`) parse World dmy/dby dates before US
#'   mdy/bdy dates.
#'
#' @param quiet a logical indicating if messages should be displayed to the
#'     console (`TRUE`, default); set to `FALSE` to silence messages
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
#' - "Sep 19 2018"
#' 
#' \subsection{How it works}{
#' 
#' This function relies heavily on [lubridate::parse_date_time()], which is an
#' extremely flexible date parser that works well for consistent date formats,
#' but can quickly become unweildy and may produce spurious results. 
#' `guess_dates()` will use a list of formats in the `orders` argument to run
#' `parse_date_time()` with each format vector separately and take the first
#' correctly parsed date from all the trials. By default, the orders are in
#' `getOption("linelist_guess_orders")`:
#' 
#' ```
#' list(
#'   world_named_months = c("Ybd", "dby"),
#'   world_digit_months = c("dmy", "Ymd"), 
#'   US_formats         = c("Omdy", "YOmd")
#' )
#' ```
#'
#' In this case, the dates 03 Jan 2018, 07/03/1982, and 08/20/85 are correctly
#' intepreted as 2018-01-03, 1982-03-07, and 1985-08-20. The examples section
#' will show how you can manipulate the `orders` to be customised for your
#' situation.
#' }
#'
#' @seealso [clean_dates()] for cleaning of data frames
#' 
#' @examples
#' 
#' guess_dates(c("03 Jan 2018", "07/03/1982", "08/20/85")) # default
#' 
#' # The default orders prioritize world date ordering over American-style.
#' print(ord <- getOption("linelist_guess_orders"))
#'
#' # if you want to prioritize American-style dates with numeric months, you
#' # can switch the second and third elements of the default orders
#' ord[c(1, 3, 2)]
#' guess_dates(c("03 Jan 2018", "07/03/1982", "08/20/85"), orders = ord[c(1, 3, 2)])
#'
#' # If you have a format with hours, minutes and seconds, you can add that to
#' # the list
#' ord$ymdhms <- c("Ymdhms", "Ymdhm")
#' 
#' guess_dates(c("2014_04_05_23:15:43", "03 Jan 2018", "07/03/1982", "08/20/85"), orders = ord)
#' 
#' # guess_dates can handle messy dates and tolerate missing data
#' x <- c("01-12-2001", "male", "female", "2018-10-18", NA, NA, "2018_10_17",
#'       "2018 10 19", "// 24/12/1989", "this is 24/12/1989!",
#'       "RECON NGO: 19 Sep 2018 :)", "6/9/11", "10/10/10")
#'
#' guess_dates(x, error_tolerance = 1) # forced conversion
#' 
#' guess_dates(x, error_tolerance = 0.15) # only 15% errors allowed
#' 
guess_dates <- function(x, error_tolerance = 0.1, first_date = NULL, 
                        last_date = Sys.Date(), 
                        orders = getOption("linelist_guess_orders"),
                        quiet = TRUE) {

  ## This function tries converting a single character string into a
  ## well-formatted date, but still returning a character. If it can't convert
  ## it, it returns NA.

  # If the input is a date already: no guessing needed!
  if (inherits(x, c("Date", "POSIXt", "aweek"))) {
    return(as.Date(x))
  }

  # save the original x for later if nothing is converted
  ox <- x

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (!is.character(x)) {
    stop("guess dates will only work for characters and factors")
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
  x_test <- data.frame(lapply(orders, find_lubridate, x), stringsAsFactors = FALSE)
  
  ## if lubridate fails to do the job, then we should use thibaut's parser.  
  first_constraint <- constrain_date(x_test, first_date, last_date)$good_dates
  x_rescued        <- rescue_lubridate_failures(first_constraint, x)

  good_and_bad <- constrain_date(x_rescued, first_date, last_date, x)
  bad_dates    <- good_and_bad$bad_dates
  bd           <- do.call("c", unname(bad_dates))
  
  if (!all(is.na(bd))) {
    bd     <- utils::stack(bd)
    bd$ind <- as.character(bd$ind)
    bd     <- unique(bd)
    # remove the rows that have NA values 
    bd     <- bd[!is.na(bd[[1]]) | !is.na(bd[[2]]), ]
    bd     <- bd[order(bd[[1]]), ]
    misses <- sprintf("  %s  |  %s", 
                      format(c("original", "--------", bd$values)), 
                      format(c("parsed",   "------  ", bd$ind))
                      )
    misses <- paste(misses, collapse = "\n")
    msg    <- paste0("\nThe following dates were not in the correct timeframe",
                     " (%s -- %s):\n\n",
                     misses
                    )
    warning(sprintf(msg, first_date, last_date))
  }
  
  new_x <- choose_first_good_date(good_and_bad$good_dates)

  ## check how successful we were
  na_before       <- sum(is.na(x))
  na_after        <- sum(is.na(new_x))
  prop_successful <- (length(x) - na_after) / (length(x) - na_before)

  ## shape result depending on whether conversion was successful
  if (prop_successful < (1 - error_tolerance)) {
    return(ox)
  } else {
    return(as.Date(new_x))
  }
}


#' lappy-friendly wrapper of parse_date_time
#'
#' @param orders, a vector of orders to consider
#' @param x the data
#' @keywords internal
#' @noRd
find_lubridate <- function(orders = NULL, x) {
  suppressWarnings(as.Date(lubridate::parse_date_time(x, orders = orders)))
}


#' Trim dates outside of the defined boundaries
#'
#' @param date_a_frame a data frame where each column represents several
#'   different parsings of the original date vector.
#' @param dmin the minimum date
#' @param dmax the maximum date
#' @param original_dates the vector of original dates (to be collected for errors)
#' @keywords internal
#' @noRd
constrain_date <- function(date_a_frame, dmin, dmax, original_dates = NULL) {
  if (!is.null(original_dates)) {
    bad_date_list <- lapply(date_a_frame, function(i) {
      setNames(original_dates, as.character(i))[i < dmin | i > dmax]
    })
  } else {
    bad_date_list <- NULL
  }
  for (i in names(date_a_frame)) {
    tmp <- date_a_frame[[i]]
    date_a_frame[[i]][tmp < dmin | tmp > dmax] <- NA 
  }
  list(good_dates = date_a_frame, bad_dates = bad_date_list)
}


#' Choose the first non-missing date from a data frame of dates
#'
#' @param date_a_frame a data frame where each column contains a different
#'   parsing of the same date vector
#' @keywords internal
#' @noRd
choose_first_good_date <- function(date_a_frame, original_dates) {
  n   <- nrow(date_a_frame)
  res <- rep(as.Date(NA), length = n)
  for (i in seq_len(n)) {
    tmp <- date_a_frame[i, ]
    res[i] <- tmp[!is.na(tmp)][1]
  }
  res
}

#' Find the dates that lubridate couldn't
#' 
#' @param date_a_frame a data frame where each column contains a different
#'   parsing of the same date vector
#' @param original_dates the vector of original dates.
#' @keywords internal
#' @noRd
rescue_lubridate_failures <- function(date_a_frame, original_dates) {
  nas     <- is.na(date_a_frame)
  # Find places where all rows are missing
  all_nas <- which(apply(nas, 1, all))
  # Use Thibaut's guesser instead
  for (i in all_nas) {
    date_a_frame[i, 1] <- i_extract_date_string(original_dates[i])
  }
  date_a_frame
}
