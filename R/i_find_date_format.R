#' Guess date format of a character string
#'
#' Internal function. The motivation behing this is that `as.Date` does not
#' handle correctly its `format` argument,
#' e.g. `as.Date("01-12-2001", format = "%Y-%m-%d")` returns `1-12-20`. Tries to
#' match a single character string against regular expressions representing
#' potential date formats. Returns the format as something that can be processed
#' by `as.Date` if a match is found, and `NULL` otherwise.
#'
#' @author Thibaut Jombart
#'
#' @return If no matching format can be found, the function returns NULL; if a
#'   matching format is found, the function returned the matched regular
#'   expression (clean date) and its format compatible with `as.Date`.

i_find_date_format <- function(x) {
  x <- as.character(x[1])

  ## define the regular expressions used to find dates

  num <- "[[:digit:]]"
  separator <- "[[:punct:][:blank:]]+"
  x <- gsub(separator, "-", x)

  formats <- list(
    "%Y-%m-%d" = paste0(num, "{4}", "-",
                 num, "{2}", "-",
                 num, "{2}", collapse = ""),
    "%d-%m-%Y" = paste0(num, "{2}", "-",
                 num, "{2}",  "-",
                 num, "{4}", collapse = "")
  )


  ## look for these expressions in 'x', return NULL if we don't find anything

  grep_logical <- function(pattern, x, ...) {
    length(grep(pattern, x, ...)) > 0
  }

  matching <- vapply(formats, grep_logical, logical(1), x)
  format <- names(which(matching))[1] # only get the first matching format

  if (length(format) == 0L) {
    return(NULL)
  }


  ## If we do find the format, extract the clean date (it could be flanked by
  ## garbage), and return a named character vector of length 2, containing the
  ## as.Date compatible 'format', and the clean date itself, as a character.

  ## TODO: so far this return the last date of the character string, if there
  ## are several ones amatching the same format

  expression <- formats[[format]]
  cleaning_expr <- paste0("^.*(", expression, ").*$")
  clean_date <- gsub(cleaning_expr, "\\1", x)
  out <- c("format" = format, "date" = clean_date)
  out
}
