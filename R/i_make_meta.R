#' Create the meta data frame for the hxl object
#'
#' @param dat a data frame
#' @param hxl an optional list defining the hxl terms for each column
#' @param the_comment and optional list defining the comments for each
#'   column
#' @noRd
#' @importFrom utils stack
i_make_meta <- function(dat, hxl = NULL, the_comment = NULL) {
  meta <- stack(i_find_classes(dat))[2:1]
  names(meta)  <- c("column", "class")
  meta$hxl     <- if (!is.null(hxl)) hxl else ""
  meta$comment <- if (!is.null(comment)) the_comment else ""
  meta
}
