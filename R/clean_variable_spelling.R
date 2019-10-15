#' Check and clean spelling or codes of multiple variables in a data frame
#'
#' @description This function allows you to clean your data according to 
#' pre-defined rules encapsulated in either a data frame or list of data frames.
#' It has application for addressing mis-spellings and recoding variables (e.g.
#' from electronic survey data). 
#'
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
#' 
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
#' @seealso [clean_spelling()], which this function wraps.
#'
#' @author Zhian N. Kamvar
#'
#' @export
#'
#' @examples
#' 
#' # Set up wordlist ------------------------------------------------ 
#'
#' yesno  <- c("y", "n", "u", ".missing")
#' dyesno <- c("Yes", "No", "Unknown", "Missing")
#'
#' treated  <- c(0:1, ".missing")
#' dtreated <- c("Yes", "No", "Missing")
#'
#' facility  <- c(1:10, ".default") # define a .default key
#' dfacility <- c(sprintf("Facility %s", format(1:10)), "Unknown")
#'
#' age_group  <- c(0, 10, 20, 30, 40, 50)
#' dage_group <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50+")
#' 
#' lab_result <- c("neg", "pos", "inc")
#' dlab_result <- c("Negative", "Positive", "Inconclusive")
#'
#' wordlist <- data.frame(
#'   options = c(yesno, treated, facility, age_group, lab_result),
#'   values  = c(dyesno, dtreated, dfacility, dage_group, dlab_result),
#'   grp = rep(c("readmission", "treated", "facility", "age_group",
#'               ".regex ^lab_result_"),
#'             c(4, 3, 11, 6, 3)),
#'   orders  = c(1:4, 1:3, 1:11, 1:6, 1:3),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Assigning global values ----------------------------------------
#'
#' global_words <- data.frame(
#'   options = c("y", "n", "u", "unk", "oui", ".missing"),
#'   values  = c("yes", "no", "unknown", "unknown", "yes", "missing"),
#'   grp     = rep(".global", 6),
#'   orders  = rep(Inf, 6),
#'   stringsAsFactors = FALSE
#' )
#' 
#' wordlist <- rbind(wordlist, global_words, stringsAsFactors = FALSE)
#'
#' # Generate example data ------------------------------------------
#' dat <- data.frame(
#'   # these have been defined
#'   readmission = sample(yesno, 50, replace = TRUE),
#'   treated = sample(treated, 50, replace = TRUE),
#'   facility = sample(c(facility[-11], LETTERS[1:3]), 50, replace = TRUE),
#'   age_group = sample(age_group, 50, replace = TRUE),
#'   # global values will catch these
#'   has_symptoms = sample(c(yesno, "unk", "oui"), 50, replace = TRUE),
#'   followup = sample(c(yesno, "unk", "oui"), 50, replace = TRUE),
#'   lab_result_01 = sample(c(lab_result, "unk"), 50, replace = TRUE),
#'   lab_result_02 = sample(c(lab_result, "unk"), 50, replace = TRUE),
#'   lab_result_03 = sample(c(lab_result, "unk"), 50, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#' 
#' missing_data <- dat == ".missing"
#' dat[missing_data] <- sample(c("", NA), sum(missing_data), prob = c(0.1, 0.9), replace = TRUE)
#'
#' # Clean spelling based on wordlist ------------------------------ 
#'
#' wordlist # show the wordlist
#' head(dat) # show the data
#' 
#' res1 <- clean_variable_spelling(dat,
#'                                 wordlists = wordlist,
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
#'                                 spelling_vars = "grp", 
#'                                 sort_by = "orders")
#' head(res2)
#' as.list(head(res2))
#' 
clean_variable_spelling <- function(x = data.frame(), wordlists = list(),
                                    spelling_vars = 3, sort_by = NULL,
                                    classes = NULL, warn = FALSE) {
  
  if (length(x) == 0 || !is.data.frame(x)) {
    stop("x must be a data frame")
  }
  if (is.null(classes)) {
    classes <- i_find_classes(x)
  }

  # Define columns viable for manipulation ------------------------------------
  # Because this is a global manipulator, only work on characters or factors
  unprotected <- names(x)[classes %in% c("character", "factor")]
  
  
  if (length(wordlists) == 0 || !is.list(wordlists)) {
    stop("wordlists must be a list of data frames")
  } 

  # There is one big dictionary with spelling_varss -----------------------------------
  if (is.data.frame(wordlists)) {

    # There is a spelling_vars column ----------------------------------------
    if (!is.null(spelling_vars) && length(spelling_vars) == 1) {
      is_number <- is.numeric(spelling_vars) &&          # spelling_vars is a number
        as.integer(spelling_vars) == spelling_vars && # ... and an integer
          spelling_vars <= ncol(wordlists)      # ... and is within the bounds

      is_name   <- is.character(spelling_vars) &&         # spelling_vars is a name
        any(names(wordlists) == spelling_vars) # ... in the wordlists
      if (is_number || is_name) {
        wordlists <- split(wordlists, wordlists[[spelling_vars]])
      } else {
        stop("spelling_vars must be the name or position of a column in the wordlist")
      }
    } else {
      warning("Using wordlist globally across all character/factor columns.")
    }
  } else {
    # Not everything is a data frame :( ---------------------------------------
    if (!all(vapply(wordlists, is.data.frame, logical(1)))) {
      stop("everything in wordlists must be a data frame")
    }

    # Not all dictionaries are named ------------------------------------
    if (any(names(wordlists) == "")) {
      stop("all dictionaries must be named")
    }
  }

  one_big_dictionary <- is.data.frame(wordlists)
  exists_sort_by     <- !is.null(sort_by)

  
  if (one_big_dictionary) {
    # If there is one big dictionary ------------------------------------
    if (exists_sort_by && sort_by %in% names(wordlists)) {
      wordlists <- wordlists[order(wordlists[[sort_by]]), , drop = FALSE]
    }
    # Iterate over the names of the data -------------------
    to_iterate_x <- unprotected
    to_iterate_wordlist <- unprotected
  } else {
    # If there is a list of dictionaries --------------------------------
    if (exists_sort_by) {
      for (i in names(wordlists)) {
        di <- wordlists[[i]]
        # Only sort if there is something to sort by -------
        the_sorts  <- if (any(names(di) == sort_by)) order(di[[sort_by]]) else TRUE
        wordlists[[i]] <- wordlists[[i]][the_sorts, , drop = FALSE]
      }
    }
    global_words <- wordlists[[".global"]]
    wordlists    <- wordlists[names(wordlists) != ".global"]
    has_global   <- !is.null(global_words)
    # Iterate over the names of the dictionaries -----------
    
    
    # extract vars to check from wordlists (both plain and regex)
    vars_check <- names(wordlists)
    is_var_regex <- grepl("^\\.regex", vars_check)
    vars_check_plain <- vars_check[!is_var_regex]
    vars_check_regex <- vars_check[is_var_regex]
    vars_check_regex_extract <- gsub("\\.regex[[:space:]]", "", vars_check_regex)
    
    # which cols in x match each regex var (returns 1 list element for each .regex key)
    vars_regex_match_list <- lapply(
      vars_check_regex_extract,
      function(j) names(x)[grepl(j, names(x), perl = TRUE)]
    )
    
    # check for wordlist variables with no match in x
    vars_regex_match_n <- vapply(vars_regex_match_list, length, 0L)
    vars_plain_nomatch <- vars_check_plain[!vars_check_plain %in% names(x)]
    vars_regex_nomatch <- vars_check_regex[vars_regex_match_n == 0]
    vars_nomatch <- unique(c(vars_plain_nomatch, vars_regex_nomatch))
    
    # Some dictionaries aren't in the data ------------------------------
    # (warn if variables in wordlist don't match any columns in x) ------
    if (length(vars_nomatch) > 0) {
      warning("The following variable(s) in 'wordlist' did not match any ",
              "columns in 'x': ", paste(vars_nomatch, collapse = ", "),
              call. = FALSE)
    }
    
    # all columns of x that match variable in wordlists
    cols_match_plain <- vars_check_plain[vars_check_plain %in% names(x)]
    cols_match_regex <- unlist(vars_regex_match_list, use.names = FALSE)
    cols_match <- c(cols_match_plain, cols_match_regex)
    
    # wordlist variable name corresponding to each matching column in x
    matching_var_plain <- cols_match_plain
    matching_var_regex <- rep(vars_check_regex, vars_regex_match_n)
    matching_var <- c(matching_var_plain, matching_var_regex)
    
    to_iterate_x <- cols_match
    to_iterate_wordlist <- matching_var
    
    if (has_global) {
      unprotected_to_add <- setdiff(unprotected, to_iterate_x)
      to_iterate_x <- c(to_iterate_x, unprotected_to_add)
      to_iterate_wordlist <- c(to_iterate_wordlist, unprotected_to_add)
    }
  }

  # check if there is a ".default" value in the global dictionary
  global_with_default <- one_big_dictionary && 
    any(wordlists[[1]] == ".default") || 
    (
     !one_big_dictionary && 
     has_global && 
     any(global_words[[1]] == ".default")
    )

  if (global_with_default) {
  
    stop("the .default keyword cannot be used with .global")
  
  }
  # Prepare warning/error labels ---------------------------------------------
  warns <- errs <- vector(mode = "list", length = length(to_iterate_x))
  iter_print <- gsub(" ", "_", format(to_iterate_x))
  names(iter_print) <- names(warns) <- names(errs) <- to_iterate_x

  # Loop over the variables and clean spelling --------------------------------
  for (i in seq_along(to_iterate_x)) {
    
    i_x <- to_iterate_x[i]
    i_w <- to_iterate_wordlist[i]
    
    d <- if (one_big_dictionary) wordlists else wordlists[[i_w]]

    if (is.null(d)) {
    # d is null because this is a variable without a specific spelling def
      d <- global_words
    } else if (!one_big_dictionary) {
    # d is not null, but the input has specific variables
      # find the words that match the wordlist
      gw <- !global_words[[1]] %in% d[[1]]
      if (sum(gw) > 0) {
      # If there are still global words to clean, pass them through
        g      <- global_words[gw, , drop = FALSE]
        w      <- withWarnings(clean_spelling(x[[i_x]], g, quiet = FALSE))
        x[[i_x]] <- if(is.null(w$val)) x[[i_x]] else w$val
        if (warn) {
          warns[[i_x]] <- collect_ya_errs(w$warnings, iter_print[i_x])
          errs[[i_x]]  <- collect_ya_errs(w$errors, iter_print[i_x])
        }
      }
    } else {
      # There is one big, global dictionary
      d <- d
    }
    # Evaluate and collect any warnings/errors that pop up
    w      <- withWarnings(clean_spelling(x[[i_x]], d, quiet = FALSE))
    x[[i_x]] <- if(is.null(w$val)) x[[i_x]] else w$val
    if (warn) {
      warns[[i_x]] <- c(warns[[i_x]], collect_ya_errs(w$warnings, iter_print[i_x]))
      errs[[i_x]]  <- c(errs[[i_x]], collect_ya_errs(w$errors, iter_print[i_x]))
    }
  }

  # Process warnings and errors and give a warning if there were any
  if (warn) {
    wemsg <- process_werrors(warns, errs)
    if (!is.null(wemsg)) warning(wemsg)
  }

  x
}
