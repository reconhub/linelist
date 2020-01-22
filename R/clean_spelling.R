#' Rename values in a vector based on a wordlist
#'
#' This function provides an interface for [forcats::fct_recode()], 
#' [forcats::fct_explicit_na()], and [forcats::fct_relevel()] in such a way that
#' a data wordlist can be imported from a data frame. 
#'
#' @param x a character or factor vector
#'
#' @param wordlist a matrix or data frame defining mis-spelled words or keys
#' in one column (`from`) and replacement values (`to`) in another
#' column. There are keywords that can be appended to the `from` column for
#' addressing default values and missing data.
#'
#' @param from a column name or position defining words or keys to be replaced
#'
#' @param to a column name or position defining replacement values
#'
#' @param quiet a `logical` indicating if warnings should be issued if no
#'   replacement is made; if `FALSE`, these warnings will be disabled
#' 
#' @param warn_default a `logical`. When a `.default` keyword is set and 
#'   `warn_default = TRUE`, a warning will be issued listing the variables
#'   that were changed to the default value. This can be used to update your
#'   wordlist.
#' 
#' @param anchor_regex a `logical`. When `TRUE` (default), any regex within
#'   the keywork 
#' 
#'
#' @details 
#'
#' \subsection{Keys (`from` column)}{
#' 
#' The `from` column of the wordlist will contain the keys that you want to
#' match in your current data set. These are expected to match exactly with
#' the exception of three reserved keywords that start with a full stop:
#'
#'  - `.regex [pattern]`: will replace anything matching `[pattern]`. **This
#'    is executed before any other replacements are made**. The `[pattern]`
#'    should be an unquoted, valid, PERL-flavored regular expression. Any
#'    whitespace padding the regular expression is discarded.
#'  - `.missing`: replaces any missing values (see NOTE)
#'  - `.default`: replaces **ALL** values that are not defined in the wordlist
#'                and are not missing. 
#'
#' }
#' \subsection{Values (second column)}{
#' 
#' The values will replace their respective keys exactly as they are presented.
#'
#' There is currently one recognised keyword that can be placed in the `to` 
#' column of your wordlist:
#'
#'  - `.na`: Replace keys with missing data. When used in combination with the
#'    `.missing` keyword (in column 1), it can allow you to differentiate
#'    between explicit and implicit missing data.
#' 
#' }
#'
#' @note If there are any missing values in the `from` column (keys), then they
#' are automatically converted to the character "NA" with a warning. If you want
#' to target missing data with your wordlist, use the `.missing` keyword. The
#' `.regex` keyword uses [gsub()] with the `perl = TRUE` option for replacement.
#'
#' @return a vector of the same type as `x` with mis-spelled labels cleaned. 
#'   Note that factors will be arranged by the order presented in the data 
#'   wordlist; other levels will appear afterwards.  
#'
#' @author Zhian N. Kamvar
#'
#' @seealso [matchmaker::match_vec()], which this function wraps and
#'   [matchmaker::match_df()] for an implementation that acts across
#'   multiple variables in a data frame.
#'
#' @export
#'
#' @examples
#'
#' corrections <- data.frame(
#'   bad = c("foubar", "foobr", "fubar", "unknown", ".missing"), 
#'   good = c("foobar", "foobar", "foobar", ".na", "missing"),
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
#' # You can use regular expressions to simplify your list
#' corrections <- data.frame(
#'   bad =  c(".regex f[ou][^m].+?r$", "unknown", ".missing"), 
#'   good = c("foobar",                ".na",     "missing"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # You can also set a default value
#' corrections_with_default <- rbind(corrections, c(bad = ".default", good = "unknown"))
#' corrections_with_default
#' 
#' # a warning will be issued about the data that were converted
#' clean_spelling(my_data, corrections_with_default)
#'
#' # use the warn_default = FALSE, if you are absolutely sure you don't want it.
#' clean_spelling(my_data, corrections_with_default, warn_default = FALSE)
#'
#' # The function will give you a warning if the wordlist does not
#' # match the data
#' clean_spelling(letters, corrections)
#'
#' # The can be used for translating survey output
#'
#' words <- data.frame(
#'   option_code = c(".regex ^[yY][eE]?[sS]?", 
#'                   ".regex ^[nN][oO]?", 
#'                   ".regex ^[uU][nN]?[kK]?", 
#'                   ".missing"),
#'   option_name = c("Yes", "No", ".na", "Missing"),
#'   stringsAsFactors = FALSE
#' )
#' clean_spelling(c("Y", "Y", NA, "No", "U", "UNK", "N"), words)
#'
clean_spelling <- function(x = character(), wordlist = data.frame(),
                           from = 1, to = 2,
                           quiet = FALSE, warn_default = TRUE,
                           anchor_regex = TRUE) {
  matchmaker::match_vec(
    x = x,
    dictionary = wordlist,
    from = from,
    to = to,
    warn_default = warn_default,
    anchor_regex = anchor_regex
  )
}
