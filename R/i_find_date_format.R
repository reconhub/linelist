#' Guess date format of a character string
#'
#' Internal function. The motivation behing this is that `as.Date` does not
#' handle correctly its `format` argument,
#' e.g. `as.Date("01-12-2001", format = "%Y-%m-%d")` returns `1-12-20`. Tries to
#' match a single character string against regular expressions representing
#' potential date formats. Returns the format as something that can be processed
#' by `as.Date` if it can, or NA otherwise.
#'
#' @author Thibaut Jombart
#'

i_find_date_format <- function(x) {
  x <- as.character(x[1])

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

  grep_logical <- function(pattern, x, ...) {
    length(grep(pattern, x, ...)) > 0
  }

  matching <- vapply(formats, grep_logical, logical(1), x)
  out <- names(which(matching))

  ifelse(length(out) == 0L, NA, out)

}
