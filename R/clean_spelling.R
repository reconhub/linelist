#' Rename values in a vector based on a dictionary
#'
#' @param x a character vector
#' @param dictionary a two-column matrix or data frame defining mis-spelled words in
#'   the first column and replacements in the second column
#'
#' @author Zhian N. Kamvar
#' @export
#' @examples
#'
#' corrections <- data.frame(
#'   bad = c("foubar", "foobr", "fubar", NA, "unknown"),
#'   good = c("foobar", "foobar", "foobar", "missing", "missing"),
#'   stringsAsFactors = FALSE
#' )
#' head(corrections)
#' my_data <- c(letters[1:5], sample(corrections$bad, 10, replace = TRUE))
#' clean_spelling(my_data, corrections)
#'
#' # The function will give you a warning if the dictionary does not
#' # match the data
#' clean_spelling(letters, corrections)
#'
#' @importFrom forcats fct_recode
#' @importFrom rlang "!!!"

clean_spelling <- function(x = character(), dictionary = data.frame()) {

  if (length(x) == 0 || !is.character(x) && !is.factor(x)) {
    stop("x must be a character or factor")
  }

  if (length(dictionary) == 0 || !is.data.frame(dictionary)) {
    stop("dictionary must be a data frame")
  }

  x_is_factor <- is.factor(x)

  no_keys   <- !any(x %in% dictionary[[1]], na.rm = TRUE)
  no_values <- !any(x %in% dictionary[[2]], na.rm = TRUE)
  no_nas    <- !any(is.na(dictionary[[1]]))

  if (no_keys && no_values) {
    msg <- "None of the variables in %s were found in %s. Did you use the correct dictionary?" 
    msg <- sprintf(msg, deparse(substitute(x)), deparse(substitute(dictionary)))
    warning(msg)
  }

  dict <- setNames(dictionary[[1]], dictionary[[2]])
  nas  <- dict[is.na(dict)]
  dict <- dict[!is.na(dict)]
  suppressWarnings(x <- forcats::fct_recode(x, !!!dict))
  if (length(nas) > 0) {
    x <- forcats::fct_explicit_na(x, na_level = names(nas))
  }
  if (!x_is_factor) {
    x <- as.character(x)
  }
  x
}
