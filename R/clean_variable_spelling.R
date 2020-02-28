#' Check and clean spelling or codes of multiple variables in a data frame
#'
#' @description This function allows you to clean your data according to 
#' pre-defined rules encapsulated in either a data frame or list of data frames.
#' It has application for addressing mis-spellings and recoding variables (e.g.
#' from electronic survey data). 
#'
#' @param wordlists a data frame or named list of data frames with at least two
#'   columns defining the word list to be used. If this is a data frame, a third
#'   column must be present to split the wordlists by column in `x` (see
#'   `spelling_vars`).
#'
#' @param spelling_vars character or integer. If `wordlists` is a data frame,
#'   then this column in defines the columns in `x` corresponding to each
#'   section of the `wordlists` data frame. This defaults to `3`, indicating the
#'   third column is to be used.
#'
#' @param sort_by a character the column to be used for sorting the values in
#'   each data frame. If the incoming variables are factors, this determines how
#'   the resulting factors will be sorted.
#' 
#' @param warn if `TRUE`, warnings and errors from [clean_spelling()] will be 
#'   shown as a single warning. Defaults to `FALSE`, which shows nothing.
#'
#' @inheritParams clean_variable_labels
#' @inheritParams clean_spelling
#'
#' @details By default, this applies the function [clean_spelling()] to all
#'   columns specified by the column names listed in `spelling_vars`, or, if a
#'   global dictionary is used, this includes all `character` and `factor`
#'   columns as well.
#'
#' \subsection{spelling_vars}{
#' 
#' Spelling variables within `wordlists` represent keys that you want to match
#' to column names in `x` (the data set). These are expected to match exactly
#' with the exception of two reserved keywords that starts with a full stop:
#'
#'  - `.regex [pattern]`: any column whose name is matched by `[pattern]`. The
#'  `[pattern]` should be an unquoted, valid, PERL-flavored regular expression.
#'  - `.global`: any column (see Section *Global wordlists*)
#'
#' }
#'
#' \subsection{Global wordlists}{
#' 
#' A global wordlist is a set of definitions applied to all valid columns of `x`
#' indiscriminantly.
#'
#'  - **.global spelling_var**: If you want to apply a set of definitions to all
#'     valid columns in addition to specified columns, then you can include a
#'     `.global` group in the `spelling_var` column of your `wordlists` data
#'     frame. This is useful for setting up a dictionary of common spelling 
#'     errors. *NOTE: specific variable definitions will override global
#'     defintions.* For example: if you have a column for cardinal directions
#'     and a definiton for `N = North`, then the global variable `N = no` will
#'     not override that. See Example.
#'
#'  - **`spelling_vars = NULL`**: If you want your data frame to be applied to
#'    all character/factor columns indiscriminantly, then setting 
#'    `spelling_vars = NULL` will use that wordlist globally.
#'
#' }
#'
#' 
#' @note This function will only parse character and factor columns to protect
#'   numeric and Date columns from conversion to character. 
#'
#' @return a data frame with re-defined data based on the dictionary 
#'
#' @seealso [matchmaker::match_df()], which this function wraps.
#'
#' @author Zhian N. Kamvar
#' @author Patrick Barks
#'
#' @export
#'
#' @examples
#' 
#' # Read in dictionary and coded date examples --------------------
#'
#' wordlist <- read.csv(linelist_example("spelling-dictionary.csv"), 
#'                      stringsAsFactors = FALSE)
#' dat      <- read.csv(linelist_example("coded-data.csv"), 
#'                      stringsAsFactors = FALSE)
#' dat$date <- as.Date(dat$date)
#'
#' # Clean spelling based on wordlist ------------------------------ 
#'
#' wordlist # show the wordlist
#' head(dat) # show the data
#' 
#' res1 <- clean_variable_spelling(dat,
#'                                 wordlists = wordlist,
#'                                 from = "options",
#'                                 to = "values",
#'                                 spelling_vars = "grp")
#' head(res1)
#' 
#' # You can ensure the order of the factors are correct by specifying 
#' # a column that defines order.
#'
#' dat[] <- lapply(dat, as.factor)
#' as.list(head(dat))
#' res2 <- clean_variable_spelling(dat, 
#'                                 wordlists = wordlist, 
#'                                 from = "options",
#'                                 to = "values",
#'                                 spelling_vars = "grp", 
#'                                 sort_by = "orders")
#' head(res2)
#' as.list(head(res2))
#' 
clean_variable_spelling <- function(x = data.frame(), wordlists = list(),
                                    from = 1, to = 2, spelling_vars = 3,
                                    sort_by = NULL, classes = NULL, 
                                    warn = FALSE) {
  matchmaker::match_df(
    x = x,
    dictionary = wordlists,
    from = from,
    to = to,
    by = spelling_vars,
    order = sort_by,
    warn = warn
  )  
}
