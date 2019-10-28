#' Clean variable names
#'
#' This function standardises the variable names in a
#' `data.frame`. It uses the standardisation implemented by
#' [epitrix::clean_labels()] in the `epitrix` package. See
#' `?epitrix::clean_labels` for more information.
#'
#' @author Thibaut Jombart
#'
#' @param x a `data.frame`
#'
#' @param ... further arguments passed to [epitrix::clean_labels()]; the most
#'   important is `sep`, which refers to the separator used between words,
#'   and defaults to the underscore `_`.
#'
#' @param protect a logical or numeric vector specifying which columns to 
#'   protect from manipulation
#'
#' @export
#'
#' @return A `data.frame` with standardised variable names.
#' 
#' @seealso [clean_data()] for a one-shot wrapper to clean your data
#'
#' @examples
#'
#' ## make toy data
#' onsets <- as.Date("2018-01-01") + sample(1:10, 20, replace = TRUE)
#' gender <- sample(c("male", "female"), 20, replace = TRUE)
#' case_type <- c("confirmed", "probable", "suspected", "not a case")
#' case <- sample(case_type, 20, replace = TRUE)
#' toy_data <- data.frame("Date of Onset." = onsets,
#'                        "_GENDER_ " = gender,
#'                        "Épi.Case_définition" = case)
#' ## show data
#' toy_data
#'
#'
#' ## clean variable names, store in new object, show results
#' clean_data <- clean_variable_names(toy_data)
#' clean_data

clean_variable_names <- function(x, protect = FALSE, ...) {

  variable_names <- new_names <- colnames(x)

  if (is.null(variable_names)) {
    stop('x has no column names')
  }

  # converting only the unprotected varaibles
  protect <- i_logical_from_int(protect, new_names)
  new_names[!protect] <- epitrix::clean_labels(new_names[!protect], ...)

  # checking the variables and ammending them with numbers if they end up being
  # the same
  .dots     <- list(...)
  sep       <- if (is.null(.dots[["sep"]])) "_" else .dots[["sep"]]

  # Step 1: rearrange the vars so that protected are first (but if nothing is
  # protected, then we create dummy subset variables
  # Step 2: make things unique
  # Step 3: rearrange things in the correct order
  # Step 3: check if anything has changed
  # Step 4: apply the suffixes to the new vector 

  if (length(protect) > 1) {
    protect_first <- c(which(protect), which(!protect))
    in_order      <- order(protect_first)
  } else {
    protect_first <- in_order <- TRUE
  }

  var_names <- make.unique(new_names[protect_first], sep = sep)
  var_names <- var_names[in_order]

  var_names[protect] <- new_names[protect]
  
  if (!identical(var_names, new_names)) {
    # If we get here, then that means some of the data had to be corrected
    pat <- paste0("[", sep, "]\\d+$") 
    dupes <- var_names != new_names & grepl(pat, var_names, perl = TRUE)
    suffix <- sub(paste0("^.+(", pat, ")"), "\\1", var_names[dupes], perl = TRUE)
    new_names[dupes] <- paste0(new_names[dupes], suffix)

    varn <- format(variable_names[dupes])
    newn <- format(new_names[dupes])
    renames <- paste(varn, newn, 
                     sep = " -> ",
                     collapse = "\n  ")
    
    wrn <- paste("Some variable names were duplicated after cleaning and had",
                 "suffixes attached:\n\n ",
                 renames)
    warning(wrn, call. = FALSE)
    
  } 

  colnames(x) <- new_names

  # preserving the original variable names in a comment
  names(variable_names) <- colnames(x)
  comment(x) <- c(comment(x), variable_names)
  x
}

