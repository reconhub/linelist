#' Clean variable labels and fix spelling according to a wordlist
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
#' @param warn_spelling if `TRUE`, errors and warnings from [clean_spelling()]
#'   will be aggregated and presented for each column that issues them. The
#'   default value is `FALSE`, which means that all errors and warnings will be
#'   ignored. 
#'
#' @export 
#' @author Zhian N. Kamvar
#' @seealso [clean_variable_labels()] to standardise text, 
#'   [clean_variable_spelling()] to correct spelling with a wordlist.
#'
#' @inheritParams clean_variable_spelling
#'
#' @examples
#'
#' ## make toy data
#' toy_data <- messy_data(20)
#' 
#' # location data with mis-spellings, French, and English.
#' messy_locations <- c("hopsital", "h\u00f4pital", "hospital", 
#'                      "m\u00e9dical", "clinic", 
#'                      "feild", "field")
#' toy_data$location <- sample(messy_locations, 20, replace = TRUE)
#' 
#' ## show data
#' toy_data
#' 
#' # clean labels
#' clean_variables(toy_data) # by default, it's the same as clean_variable_lables
#' 
#' # add a wordlist
#' wordlist <- data.frame(
#'   from  = c("hopsital", "hopital",  "medical", "feild"),
#'   to    = c("hospital", "hospital", "clinic",  "field"),
#'   variable = rep("location", 4),
#'   stringsAsFactors = FALSE
#' )
#' 
#' clean_variables(toy_data, 
#'                 wordlists     = wordlist,
#'                 spelling_vars = "variable"
#'                )
clean_variables <- function(x, sep = "_", wordlists = NULL, spelling_vars = 3, 
                            sort_by = NULL, protect = FALSE, classes = NULL,
                            warn_spelling = FALSE) {

  xname <- deparse(substitute(x))
  if (!is.data.frame(x)) {
    stop(sprintf("%s is not a data frame", xname))
  } 

  if (ncol(x)==0L) {
    stop(sprintf("%s has no columns", xname))
  }
  if (is.null(classes)) {
    # Find classes and protect the ones that should not be manipulated ---------
    classes <- i_find_classes(x)
    protect <- i_logical_from_int(protect, classes)
    classes[protect] <- "protected"
  }

  # Clean the variable labels -------------------------------------------------
  out <- clean_variable_labels(x, sep = sep, classes = classes)

  # Clean the spelling if there is a dictionary -------------------------------
  if (!is.null(wordlists)) {
    out <- clean_variable_spelling(out, 
                                   wordlists = wordlists, 
                                   spelling_vars = spelling_vars, 
                                   sort_by = sort_by,
                                   warn    = warn_spelling,
                                   classes = classes)
  }
  
  out
}
