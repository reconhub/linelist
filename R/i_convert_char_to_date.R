#' Converts a character string to a Date
#'
#' Internal function. This is a wrapper for `as.Date` which uses several
#' possible formats for dates.
#'
#'
#' @author Thibaut Jombart
#'
i_convert_char_to_date <- function(x) {
  new_x <- as.character(x)

  date_formats <- c("%Y-%m-%d", "%Y/%m/%d", "%Y_%m_%d", "%Y %m %d",
                    "%d-%m-%Y", "%d/%m/%Y", "%d_%m_%Y", "%d %m %Y")

  as.Date(x, tryFormats = date_formats)
}
