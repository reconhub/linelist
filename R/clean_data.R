#' Clean a data.frame
#'
#' This function applies several cleaning procedures to an input `data.frame`,
#' by standardising variable names, labels used categorical variables
#' (characters of factors), and setting dates to `Date` objects. Optionally, an
#' intelligent date search can be used on character strings to extract dates
#' from various formats mixed with other text. See details for more information.
#'
#' @author Thibaut Jombart, Zhian N. Kamvar
#'
#' @inheritParams clean_variables
#' @inheritParams clean_dates
#' 
#' @export
#'
#' @return A `data.frame` with standardised labels for characters and
#'   factors.
#'
#' @examples
#'
#' ## make toy data
#' toy_data <- messy_data(20)
#' 
#' ## show data
#' toy_data
#'
#'
#' ## clean variable names, store in new object, show results
#' clean_data <- clean_data(toy_data, error_tolerance = 0.1)
#' clean_data
#'
#' clean_data2 <- clean_data(toy_data, error_tolerance = 0.8)
#' clean_data2
#' 
#' ## clean variable names, but keep our "messy/dates" column
#' to_protect <- names(toy_data) %in% "messy/dates"
#' clean_data3 <- clean_data(toy_data, 
#'                           error_tolerance = 0.8,
#'                           protect = to_protect
#'                          )
#' clean_data3
#'
#' ## Using a wordlist  -------------------------------
#'
#' # location data with mis-spellings, French, and English.
#' messy_locations <- c("hopsital", "h\u00f4pital", "hospital", 
#'                      "m\u00e9dical", "clinic", 
#'                      "feild", "field")
#' toy_data$location <- factor(sample(messy_locations, 20, replace = TRUE))
#'
#' # show data 
#' toy_data$location
#'
#'
#' # add a wordlist
#' wordlist <- data.frame(
#'   from  = c("hopsital", "hopital",  "medical", "feild"),
#'   to    = c("hospital", "hospital", "clinic",  "field"),
#'   var_shortname = rep("location", 4),
#'   stringsAsFactors = FALSE
#' )
#' 
#' clean_data4 <- clean_data(toy_data, 
#'                           wordlists = wordlist,
#'                           group     = "var_shortname"
#'                          )
#' clean_data4
#' clean_data4$location


clean_data <- function(x, sep = "_", force_Date = TRUE, guess_dates = TRUE, 
                       error_tolerance = 0.5, wordlists = NULL, group = NULL, 
                       sort_by = NULL, protect = FALSE, ...) {

  xname <- deparse(substitute(x))
  if (!is.data.frame(x)) {
    stop(sprintf("%s is not a data frame", xname))
  } 

  if (ncol(x)==0L) {
    stop(sprintf("%s has no columns", xname))
  }

  # Find classes and protect the ones that should not be manipulated -----------
  classes <- i_find_classes(x)
  protect <- i_logical_from_int(protect, classes)
  classes[protect] <- "protected"

  # Cleaning column names ------------------------------------------------------
  out <- clean_variable_names(x, protect = protect, sep = sep)

  # Cleaning variables ---------------------------------------------------------
  out <- clean_variables(out, 
                         sep = sep, 
                         wordlists = wordlists,
                         group = group,
                         sort_by = sort_by,
                         classes = classes)
  
  # Cleaning and guessing dates ------------------------------------------------
  out <- clean_dates(out,
                     force_Date = force_Date,
                     guess_dates = guess_dates,
                     error_tolerance = error_tolerance,
                     ...,
                     classes = classes
                    )
  out
}

