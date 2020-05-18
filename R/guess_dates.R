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
#' @param modern_excel When parsing dates from excel, some dates are stored as
#'   integers. Modern versions of Excel represent dates as the number of days
#'   since 1900-01-01, but pre-2011 Excel for OSX have the origin set at 
#'   1904-01-01. If this parameter is `TRUE` (default), then this assumes that
#'   all numeric values represent dates from either a Windows version of Excel
#'   or a 2011 or later version of Excel for OSX. Set this parameter to `FALSE`
#'   if the data came from an OSX version of Excel before 2011. 
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
#' # Mixed format date -----------------------------------------
#' 
#' guess_dates(c("03 Jan 2018", "07/03/1982", "08/20/85")) # default
#' 
#' # Prioritizing specific date formats ------------------------
#' # 
#' # The default orders prioritize world date ordering over American-style.
#' 
#' print(ord <- getOption("linelist_guess_orders"))
#'
#' # if you want to prioritize American-style dates with numeric months, you
#' # can switch the second and third elements of the default orders
#' 
#' print(ord <- getOption("linelist_guess_orders"))
#' print(us_ord <- ord[c(1, 3, 2)])
#' guess_dates(c("03 Jan 2018", "07/03/1982", "08/20/85"), orders = us_ord)
#'
#' # Handling dates with time formats --------------------------
#' #
#' # If you have a format with hours, minutes and seconds, you can also add that
#' # to the list of formats. Note, however, that this function will drop levels
#' # below day.
#' 
#' print(ord$ymdhms <- c("Ymdhms", "Ymdhm"))
#' 
#' guess_dates(c("2014_04_05_23:15:43", "03 Jan 2018", "07/03/1982", "08/20/85"), orders = ord)
#' 
#' # Handling  missing and nonsense data -----------------------
#' # 
#' # guess_dates can handle messy dates and tolerate missing data
#' 
#' x <- c("01-12-2001", "male", "female", "2018-10-18", NA, NA, "2018_10_17",
#'        "43391", "2018 10 19", "// 24/12/1989", "this is 24/12/1989!",
#'        "RECON NGO: 19 Sep 2018 :)", "6/9/11", "10/10/10")
#'
#' guess_dates(x, error_tolerance = 1) # forced conversion
#' 
#' guess_dates(x, error_tolerance = 0.15) # only 15% errors allowed
#' 
guess_dates <- function(x, error_tolerance = 0.1, first_date = NULL, 
                        last_date = Sys.Date(), 
                        orders = getOption("linelist_guess_orders"),
                        quiet = TRUE,
                        modern_excel = TRUE) {

  ## This function tries converting a single character string into a
  ## well-formatted date, but still returning a character. If it can't convert
  ## it, it returns NA.

  ## The conversion process uses `lubridate::parse_date_time()` under the hood,
  ## but attempts to avoid the specificity problems that lubridate introduces
  ## when you have several date formats you want to test. For example,
  ## lubridate will somtimes parse 04 Feb 1982 as 1982-04-19 because it thinks
  ## that "Feb" is a separator.
  ##
  ## To prevent this, `guess_dates()` takes a list of possible date formats and
  ## parses the entire vector of `x` using each element separately.
  ## 
  ## There are three default formats:
  ##
  ## $world_named_months
  ## [1] "Ybd" "dby"
  ## 
  ## $world_digit_months
  ## [1] "dmy" "Ymd"
  ## 
  ## $US_formats
  ## [1] "Omdy" "YOmd"
  ## 


  # Process first and last dates -----------------------------------------------
  
  # make sure that they are single character strings in ISO 8601 format.
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
  
  # Set the first date to 50 years before the last date if it's not set
  if (is.null(first_date) && inherits(last_date, "Date")) {
    first_date <- min(seq.Date(last_date, length.out = 2, by = "-50 years"))
  } 
  
  if (!inherits(first_date, "Date") || !inherits(last_date, "Date")) {
    stop("first_date and last_date must be Date objects or characters in yyyy-mm-dd format.")
  }
  
  # Process dates --------------------------------------------------------------

  # If the input is a date already: no guessing needed!
  if (inherits(x, c("Date", "POSIXt", "aweek"))) {
    x <- as.Date(x)
    x[x < first_date | x > last_date] <- as.Date(NA_character_)
    return(x)
  }

  # save the original x for later if nothing is converted
  ox <- x

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (!is.character(x)) {
    stop("guess dates will only work for characters and factors")
  }

  # Process lubridate order list -----------------------------------------------

  if (!is.list(orders) && is.character(orders)) {
    orders <- list(orders)
  } 
  
  if (!is.list(orders)) {
    stop("orders must be a list of character vectors")
  }
  
  # Guess dates ----------------------------------------------------------------

  # create a new environment for the out of bounds dates to live
  baddies <- new.env()

  # creat output data frame for dates
  res        <- list(rep(as.Date(NA_character_), length(x)))
  res        <- rep(res, length(orders))
  names(res) <- names(orders)

  # loop over each set of lubridate orders and find the dates
  for (i in seq_along(orders)) {

    # only test the dates if the previous run wasn't successful or the user doesn't want to
    # keep <- if (!fast || i == 1) TRUE else keep & is.na(res[[i - 1]])

    res[[i]] <- find_and_constrain_date(x, orders[[i]], keep = TRUE, first_date, last_date, baddies)

  }

  ## if lubridate fails to do the job, then we should use thibaut's parser.  
  x_rescued <- rescue_lubridate_failures(data.frame(res),
                                         original_dates = x, 
                                         mxl = modern_excel,
                                         dmin = first_date,
                                         dmax = last_date,
                                         baddies = baddies
                                         )

  # process dates that were not parsed -----------------------------------------

  bd <- as.list(baddies) # convert the environment to a list
  
  if (length(bd) > 0) {
    bd     <- utils::stack(bd)     # make a data frame with ind and values
    bd$ind <- as.character(bd$ind) # convert ind to char
    bd     <- unique(bd)           # only consider unique values
    bd     <- bd[!is.na(bd[[1]]) | !is.na(bd[[2]]), ] # remove NA rows
    bd     <- bd[order(bd[[1]]), ] # sort by value
    misses <- sprintf("  %s  |  %s", 
                      format(c("original", "--------", bd$values)), 
                      format(c("parsed",   "------  ", bd$ind))
                      )
    misses <- paste(misses, collapse = "\n")
    msg    <- paste0("\nThe following %s dates were not in the correct timeframe",
                     " (%s -- %s):\n\n",
                     misses
                    )
    warning(sprintf(msg, length(unique(bd$values)), first_date, last_date))
  }
  
  # Select the correct dates and test if we were successful --------------------

  new_x           <- choose_first_good_date(x_rescued)
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

#' Finds dates with lubridate and constrains them to a date range
#'
#' This takes a character vector and returns a vector of successfully translated
#' dates within the specified range.
#'
#' It will also have the side-effect of populating an environment of bad dates
#' that can be used for warning the user
#'
#' @noRd
#' @param x a character vector that can be converted to dates
#' @param orders a vector of lubridate orders to consider
#' @param keep a logical vector indicating the dates to test from `x`
#' @param dmin the minimum dates
#' @param dmax the maximum dates
#' @param baddies an environment that will act as a list of bad dates. 
#' @keywords internal
find_and_constrain_date <- function(x, orders = NULL, keep = TRUE, dmin, dmax, baddies) {

  # create an empty date vector
  res <- rep(as.Date(NA_character_), length(x))
  
  # guess at only the subset of dates
  suppressWarnings(res[keep] <- as.Date(lubridate::parse_date_time(x[keep], orders = orders)))
  
  res[keep] <- constrain_dates(res[keep], x[keep], dmin, dmax, baddies)
  res

}


#' Trim dates outside of the defined boundaries
#'
#' @noRd
#' @param date_a_frame a data frame where each column represents several
#'   different parsings of the original date vector.
#' @param dmin the minimum date
#' @param dmax the maximum date
#' @param original_dates the vector of original dates (to be collected for errors)
#' @keywords internal
constrain_dates <- function(new_dates, original_dates, dmin, dmax, baddies) {

  # filter out the dates that are below the threshold
  outsiders <- new_dates < dmin | new_dates > dmax
  outsiders[is.na(outsiders)] <- FALSE

  # record the bad dates in the environment
  if (any(outsiders)) {
    for (i in which(outsiders)) {
      bad <- as.character(new_dates[i])
      baddies[[bad]] <- c(baddies[[bad]], original_dates[i])
    }
  } 
  # mark the bad dates as NA
  new_dates[outsiders] <- as.Date(NA_character_)
  
  new_dates

}



#' Choose the first non-missing date from a data frame of dates
#'
#' @noRd
#' @param date_a_frame a data frame where each column contains a different
#'   parsing of the same date vector
#' @keywords internal
choose_first_good_date <- function(date_a_frame) {
  n   <- nrow(date_a_frame)
  date_a_frame <- as.matrix(date_a_frame)
  res <- rep(as.Date(NA), length.out = n)
  for (i in seq_len(n)) {
    tmp    <- date_a_frame[i, , drop = TRUE]
    res[i] <- as.Date(tmp[!is.na(tmp)][1])
  }
  res
}


#' Find the dates that lubridate couldn't
#' 
#' @noRd
#' @param date_a_frame a data frame where each column contains a different
#'   parsing of the same date vector
#' @param original_dates the vector of original dates.
#' @param mxl "modern excel" if TRUE, then it uses 1900 as the origin, otherwise
#'   1904 is used as the origin.
#' @param dmin the minimum dates
#' @param dmax the maximum dates
#' @param baddies an environment that will act as a list of bad dates. 
#' @keywords internal
rescue_lubridate_failures <- function(date_a_frame, original_dates, mxl = TRUE, dmin, dmax, baddies) {

  # Find places where all rows are missing
  nas     <- is.na(date_a_frame)
  all_nas <- apply(nas, 1, all)
  numbers <- suppressWarnings(!is.na(o_num <- as.integer(original_dates)))
  go_tibo <- which(all_nas & !numbers)
  go_exel <- all_nas & numbers

  # Use Thibaut's guesser 
  tmpbo   <- rep(as.Date(NA_character_), length(go_tibo))
  for (i in go_tibo) {
    tmpbo[go_tibo == i] <- i_extract_date_string(original_dates[i])
    tmpbo <- constrain_dates(tmpbo, original_dates[go_tibo], dmin, dmax, baddies)
  }
  date_a_frame[[1]][go_tibo] <- tmpbo

  # Use the excel guesser
  if (sum(go_exel)) {
    origin <- if (mxl) as.Date("1899-12-30") else as.Date("1904-01-01")
    tmpxl  <- as.Date(o_num[go_exel], origin = origin)
    date_a_frame[[1]][go_exel] <- constrain_dates(tmpxl, original_dates[go_exel], dmin, dmax, baddies)
  }

  date_a_frame
}
