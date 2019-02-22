#' Rename values in a vector based on a wordlist
#'
#' This function provides an interface for [forcats::fct_recode()], 
#' [forcats::fct_explicit_na()], and [forcats::fct_relevel()] in such a way that
#' a data wordlist can be imported from a data frame. 
#'
#' @param x a character or factor vector
#'
#' @param wordlist a two-column matrix or data frame defining mis-spelled
#'   words in the first column and replacements in the second column. There
#'   are keywords that can be appended to the first column for cleaning missing
#'   or default values.
#'
#' @param quiet a `logical` indicating if warnings should be issued if no
#'   replacement is made; if `FALSE`, these warnings will be disabled
#' 
#'
#' @details 
#'
#' \subsection{Keywords}{
#'
#' There are currently two keywords that can be placed in the first column of
#' your wordlist:
#'
#'  - `.missing`: replaces any missing values
#'  - `.default`: replaces **ALL** values that are not defined in the wordlist
#'                and are not missing. 
#'
#' }
#'
#' @return a vector of the same type as `x` with mis-spelled labels cleaned. 
#'   Note that factors will be arranged by the order presented in the data 
#'   wordlist; other levels will appear afterwards.  
#'
#' @author Zhian N. Kamvar
#'
#' @seealso [clean_variable_spelling()] for an implementation that acts across
#'   multiple variables in a data frame.
#'
#' @export
#'
#' @examples
#'
#' corrections <- data.frame(
#'   bad = c("foubar", "foobr", "fubar", "unknown", ".missing"), 
#'   good = c("foobar", "foobar", "foobar", "missing", "missing"),
#'   stringsAsFactors = FALSE
#' )
#' corrections
#' 
#' # create some fake data
#' my_data <- c(letters[1:5], sample(corrections$bad[-5], 10, replace = TRUE))
#' my_data[sample(6:15, 2)] <- NA  # with missing elements
#'
#' clean_spelling(my_data, corrections)
#'
#' # You can also set a default value
#' corrections_with_default <- rbind(corrections, c(bad = ".default", good = "unknown"))
#' corrections_with_default
#' clean_spelling(my_data, corrections_with_default)
#'
#' # The function will give you a warning if the wordlist does not
#' # match the data
#' clean_spelling(letters, corrections)
#'
#' # The can be used for translating survey output
#'
#' words <- data.frame(
#'   option_code = c("Y", "N", "U", ".missing"),
#'   option_name = c("Yes", "No", "Unknown", "Missing"),
#'   stringsAsFactors = FALSE
#' )
#' clean_spelling(c("Y", "Y", NA, "N", "U", "U", "N"), words)
#'
#' @importFrom forcats fct_recode fct_explicit_na fct_relevel
#' @importFrom rlang "!!!"

clean_spelling <- function(x = character(), wordlist = data.frame(),
                           quiet = FALSE) {

  if (length(x) == 0 || !is.atomic(x)) {
    stop("x must be coerceable to a character")
  } else if (!is.factor(x)) {
    x <- as.character(x)
  }

  wl_is_data_frame  <- is.data.frame(wordlist)
  
  wl_is_rectangular <- (wl_is_data_frame || is.matrix(wordlist)) &&
                       ncol(wordlist) >= 2
 
  if (!wl_is_rectangular) {
    stop("wordlist must be a data frame with at least two columns")
  } 
  
  if (!wl_is_data_frame) {
    wordlist <- as.data.frame(wordlist, stringsAsFactors = FALSE)
  }

  keys   <- wordlist[[1]]
  values <- wordlist[[2]]

  if (!is.atomic(keys) || !is.atomic(values)) {
    stop("wordlist must have two columns coerceable to a character")
  }

  keys <- as.character(keys)
  values <- as.character(values)


  x_is_factor <- is.factor(x)

  # replace missing with "NA" if NA is present in data
  na_present <- is.na(keys)
  keys[na_present] <- "NA"

  # replace missing keyword with NA
  missing_kw       <- keys == ".missing" | keys == ""
  keys[missing_kw] <- NA_character_

  # removing duplicated keys
  duplikeys <- duplicated(keys)
  dkeys     <- keys[duplikeys]
  keys      <- keys[!duplikeys]
  values    <- values[!duplikeys]

  if (!quiet) {
    the_call  <- match.call()
    no_keys   <- !any(x %in% keys, na.rm = TRUE)
    no_values <- !any(x %in% values, na.rm = TRUE)
    the_x     <- deparse(the_call[["x"]])
    the_words <- deparse(the_call[["wordlist"]])

    if (no_keys && no_values) {
      msg <- "None of the variables in %s were found in %s. Did you use the correct wordlist?" 
      msg <- sprintf(msg, the_x, the_words)
      warning(msg)
    }

    if (any(na_present)) {
      msg <- "NA was present in the first column of %s; replacing with the character 'NA'"
      msg <- sprintf(msg, the_words)
      warning(msg)
    }

    if (length(dkeys) > 0) {
      msg <- 'Duplicate keys were found in the first column of %s: "%s"\nonly the first instance will be used.'
      msg <- sprintf(msg, the_words, paste(dkeys, collapse = '", "'))
      warning(msg)
    }
    
  }

  dict        <- keys
  names(dict) <- values
  
  na_posi      <- is.na(dict)
  default_posi <- dict == ".default" 

  default <- dict[!na_posi & default_posi]
  nas     <- dict[na_posi]
  dict    <- dict[!na_posi & !default_posi]

  # Making "" explicitly NA ---------------------------------------------------
  x <- forcats::fct_recode(x, NULL = "")

  # Recode data with forcats --------------------------------------------------
  suppressWarnings(x <- forcats::fct_recode(x, !!! dict))

  # Replace NAs if there are any ----------------------------------------------
  if (length(nas) > 0) {
    x <- forcats::fct_explicit_na(x, na_level = names(nas))
  }

  # Replace any untranslated variables if .default is defined -----------------
  if (length(default) > 0) {
    x <- forcats::fct_other(x, keep = c(names(dict), names(nas)), other = names(default))
  }

  # Make sure order is preserved if it's a factor -----------------------------
  if (x_is_factor) {
    suppressWarnings(x <- forcats::fct_relevel(x, unique(values)))
  } else {
    x <- as.character(x)
  }

  x
}
