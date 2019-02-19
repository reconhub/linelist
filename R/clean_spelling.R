#' Rename values in a vector based on a wordlist
#'
#' This function provides an interface for [forcats::fct_recode()] and 
#' [forcats::fct_explicit_na()] in such a way that a data wordlist can be
#' imported from a data frame. 
#'
#' @param x a character or factor vector
#' @param wordlist a two-column matrix or data frame defining mis-spelled
#'   words in the first column and replacements in the second column.
#' 
#' @return a vector of the same type as `x` with mis-spelled labels cleaned. 
#'   Note that factors will be arranged by the order presented in the data 
#'   wordlist. 
#'
#' @author Zhian N. Kamvar
#' @seealso [clean_variable_spelling()] for an implementation that acts across
#'   multiple variables in a data frame.
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
#' # The function will give you a warning if the wordlist does not
#' # match the data
#' clean_spelling(letters, corrections)
#'
#' # The can be used for translating survey output
#'
#' dict <- data.frame(
#'   option_code = c("Y", "N", "U", NA),
#'   option_name = c("Yes", "No", "Unknown", "Missing"),
#'   stringsAsFactors = FALSE
#' )
#' clean_spelling(c("Y", "Y", NA, "N", "U", "U", "N"), dict)
#'
#' @importFrom forcats fct_recode
#' @importFrom rlang "!!!"

clean_spelling <- function(x = character(), wordlist = data.frame()) {

  if (length(x) == 0 || !is.atomic(x)) {
    stop("x must be coerceable to a character")
  } else if (!is.factor(x)) {
    x <- as.character(x)
  }

  if (length(wordlist) < 2 || !is.data.frame(wordlist)) {
    stop("wordlist must be a data frame with at least two columns")
  } else {
    if (!is.atomic(wordlist[[1]]) || !is.atomic(wordlist[[2]])) {
      stop("wordlist must have two columns coerceable to a character")
    }
    wordlist[[1]] <- as.character(wordlist[[1]])
    wordlist[[2]] <- as.character(wordlist[[2]])
  }

  x_is_factor <- is.factor(x)

  no_keys   <- !any(x %in% wordlist[[1]], na.rm = TRUE)
  no_values <- !any(x %in% wordlist[[2]], na.rm = TRUE)

  if (no_keys && no_values) {
    the_call <- match.call()
    msg <- "None of the variables in %s were found in %s. Did you use the correct wordlist?" 
    msg <- sprintf(msg, deparse(the_call[["x"]]), deparse(the_call[["wordlist"]]))
    warning(msg)
  }

  dict <- setNames(wordlist[[1]], wordlist[[2]])
  nas  <- dict[is.na(dict)]
  dict <- dict[!is.na(dict)]
  
  # Recode data with forcats --------------------------------------------------
  suppressWarnings(x <- forcats::fct_recode(x, !!!dict))

  # Replace NAs if there are any ----------------------------------------------
  if (length(nas) > 0) {
    x <- forcats::fct_explicit_na(x, na_level = names(nas))
  }

  # Make sure order is preserved if it's a factor -----------------------------
  if (x_is_factor) {
    x <- forcats::fct_relevel(x, unique(wordlist[[2]]))
  } else {
    x <- as.character(x)
  }

  x
}
