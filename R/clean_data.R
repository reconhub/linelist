#' Clean a data.frame
#'
#' This function applies several cleaning procedures to an input `data.frame`,
#' by standardising variable names, labels used categorical variables
#' (characters of factors), and setting dates to `Date` objects. Optionally, an
#' intelligent date search can be used on character strings to extract dates
#' from various formats mixed with other text. See details for more information.
#'
#' @author Thibaut Jombart
#'
#' @param x a `data.frame`
#'
#' @param sep The separator used between words, and defaults to the underscore
#'   `_`.
#'
#' @param protect a logical or numeric vector defining the columns to protect
#'   from any manipulation. Note: columns in `protect` will override any columns
#'   in either `force_Date` or `guess_dates`.
#'   
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
#' toy_data <- messy_data()
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


clean_data <- function(x, sep = "_", force_Date = TRUE, guess_dates = TRUE, 
                       error_tolerance = 0.5, protect = FALSE, ...) {

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
  out <- clean_variable_labels(out, sep = sep, classes = classes)
  
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

