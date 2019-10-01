#' Recode factors, keeping only most frequent levels
#'
#' This function is a generic, with methods for `factor` and `character`
#' objects. It lists all unique values in the input, ranks them from the most to
#' the least frequent, and keeps the top `n` values. Other values are replaced
#' by the chosen replacement. As an option, the user can specify a subset of the
#' input data to define dominant values. Under the hood, this uses
#' [forcats::fct_lump()] and [forcats::fct_recode()].
#'
#' @author Original code by Thibaut Jombart, rewriting using `forecats` by Zhian
#'   N. Kamvar
#'
#' @export
#'
#' @param x a `factor` or a `character` vector
#'
#' @param n the number of levels or values to keep
#'
#' @param replacement a single value to replace the less frequent values with
#'
#' @param subset a `logical`, `integer` or `character` vector used to subset the
#'   input; only the subsetted data will be used to define the dominant values,
#'   which are then used for re-defining values in the entire input
#'
#' @param ties_method how to deal with ties when ranking factor levels, which is
#'  passed on to [rank()]. The default is set at "first" (see Details).
#'
#' @param ... further arguments passed to [forcats::fct_lump()].
#'
#' @details This function is an opinionated wrapper around [forcats::fct_lump()]
#'   with the following changes:
#'
#'   1. characters are not auto-converted to factor
#'   2. the default ties method defaults to "first" instead of "min"
#'   3. if `n = nlevels(x) - 1`, then the nth level is still converted to the
#'      value of `replacement` (forcats will assume you didn't want to convert
#'      the nth level)
#'   4. it is possible to convert the replacement to `NA`
#' 
#' @examples
#' 
#' ## make toy data
#' x <- sample(letters[1:10], 100, replace = TRUE)
#' sort(table(x), decreasing = TRUE)
#' 
#' ## keep top values
#' top_values(x, 2) # top 2
#' top_values(x, 2, NA) # top 3, replace with NA
#' top_values(x, 0) # extreme case, keep nothing
#' 
#' ## dealing with ties
#' x <- c("a", "b", "a", "b", "c")
#' 
#' ## in the case of a tie (a, b), the first value is ranked higher than the
#' ## others
#' top_values(x, n = 1)
#'
#' ## here, the ties are ranked in reverse order, so b comes before a
#' top_values(x, n = 1, ties_method = "last")
#'
#' ## top_values differs from forcats::fct_lump in that if the user selects n - 1
#' ## values, it will force the last value to be "other"
#' forcats::fct_lump(x, n = 2)
#' top_values(x, n = 2)
#'
#' ## If there is a tie for the last level, then it will drop the level
#' ## depending on the ties_method
#' 
#' # replace "d" with other
#' top_values(c(x, "d"), n = 3)
#'
#' # replace "c" with other
#' top_values(c(x, "d"), n = 3, ties_method = "last")
#'
#' ## using subset
#' x <- c("a", "a", "a", "b", "b", "c")
#' x
#' top_values(x, n = 1, subset = 4:6)
#' top_values(x, n = 2, subset = 4:6)
#' top_values(x, n = 1, subset = -1)
#' top_values(x, n = 1, subset = -1, ties_method = "last")

top_values <-  function(x, n, ...) {
  UseMethod("top_values")
}



#' @export
#' @rdname top_values
top_values.default <- function(x, n, ...) {
  class_x <- paste(class(x), collapse = ", ")
  msg <- sprintf("top_values has no method for the class: %s",
                 class_x)
  stop(msg)
}


#' @export
#' @rdname top_values
#' @importFrom forcats fct_lump
top_values.factor <- function(x, n, replacement = "other",
                              subset = NULL, ties_method = "first", ...) {

  ## drop ghost levels
  x <- droplevels(x)
  
  ## check if the replacement is missing... fct_lump doesn't like other_level = NA
  other_is_missing <- is.na(replacement)

  ## use a unique level for the other to avoid overwriting any levels.
  other <- if (other_is_missing) sprintf("other%s", Sys.time()) else replacement
  
  method_not_recommended <- !ties_method %in% c("first", "last", "random")
  if (method_not_recommended) {
    msg <- paste0("using a ties_method other than first, last, or random ",
                  "can give unpredictable results in the event of a tie")
    warning(msg, call. = FALSE)
  }


  ## subsetting 
  if (!is.null(subset)) {
    ## subset and call the function on the subset
    y <- x[subset]
    if (length(y) == 0L) {
      msg <- "`subset` does not retain any input"
      stop(msg)
    }
    y <- top_values(y, n, replacement,
                    ties_method = ties_method,
                    subset = NULL, ...)
    
    ## find the levels that were dropped in the subset and replace them with other
    other_levels <- setdiff(levels(x), levels(y))
    out <- forcats::fct_other(x, drop = other_levels, other_level = other)
    
    return(out)
  }
  
  ## do the work
  out <- forcats::fct_lump(x, n = n,
                           other_level = other,
                           ties.method = ties_method,
                           ...) 

  ## check the work -------------------------------------------------------------
  ##
  ## this is the case where fct_lump decided to be helpful and return the
  ## unblemished vector when one one level would be removed. In this case, we
  ## simply change that level
  
  if (identical(out, x) && n < nlevels(x)) {
    level_counts <- tabulate(x)
    first_min    <- which.min(level_counts)

    if (ties_method == "last") {
      the_level <- first_min
    } else if (ties_method == "random" && stats::runif(1) < 0.5) {
      the_level <- sample(which(level_counts == level_counts[first_min]), 1L)
    } else {
      ## if the ties method is not random, then we should choose the last
      ## minimum value in the levels.
      the_mins  <- level_counts == level_counts[first_min]
      last_min  <- which.max(seq_along(level_counts)[the_mins])
      the_level <- if (last_min == 1) first_min else first_min + last_min - 1L
    }

    the_other_level <- levels(x)[the_level]
    names(the_other_level) <- other

    out <- forcats::fct_relevel(out, the_other_level, after = Inf)
    out <- forcats::fct_recode(out, !!!the_other_level)
    
  }

  ## remove the "other" if other is missing
  if (other_is_missing) {
    out <- forcats::fct_recode(out, NULL = other)
  }
  
  if (!method_not_recommended) {
    ## give warnings if something was removed ----------------------------------
    ##
    ## Note that we are not warning users if we have already warned them about
    ## their poor choice of ties_method.
    ##
    ## We first count up the original levels, find the last level before the
    ## the "other" level, and then find all of the levels that are tied. Once we
    ## have those levels, we can check if they all made it to the final cut. If
    ## they did, we don't need to do anything. If they didn't (some_fell), then
    ## we need to issue a warning. 
    original_levels   <- stats::setNames(tabulate(x), levels(x))
    saved_levels      <- original_levels[levels(out)[-nlevels(out)]]
    min_level         <- saved_levels[which.min(saved_levels)]
    the_fallen        <- original_levels[original_levels == min_level]
    some_fell         <- !all(names(the_fallen) %in% levels(out))

    ## if there are tied levels that didn't make the cut (some_fell), then we
    ## construct a warning message that will list all of the levels that were
    ## candidates and tell the user which one was chosen and why.
    if (some_fell && length(the_fallen) > 1) {
      the_values <- names(the_fallen)
      val <- paste0("(", the_values[1], ", ", the_values[2])
      ues <- paste0(the_values[length(the_values)], ")")

      values <- switch(as.character(length(the_values)),
                       "2" = paste0(val, ")"),
                       "3" = paste0(val, ", ", ues),
                       "4" = paste0(val, ", ", the_values[3], ", ", ues),
                       paste0(val, ", ..., ", ues)
      )
      this_method <- switch(ties_method,
                            last   = "choosing the last value",
                            random = "choosing a value at random",
                            "choosing the first value"
      )
      msg <- paste("a tie among values", values, "was broken by", this_method)
      warning(msg, call. = FALSE)
    }
  }

  out
  
}


#' @export
#' @rdname top_values
top_values.character <- function(x, n, replacement = "other",
                                 subset = NULL, ties_method = "first", ...) {

  ## convert to factor, filter, and return as a character again
  as.character(top_values(factor(x),
                          n = n,
                          replacement = replacement,
                          subset = subset,
                          ties_method = ties_method, ...))

}



