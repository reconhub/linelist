#' Retrieve metadata from a linelist object
#'
#' @param dat a linelist object
#' @param dictionary when \code{TRUE}, a column called "epivar" is appended to
#' the metadata table
#' @return a data frame representing metatdata
#' @export
#' @examples
#' dat <- clean_data(messy_data())
#' ll  <- as_linelist(dat,
#'                   id = "id", 
#'                   date_onset = "date_of_onset", 
#'                   gender = "gender",
#'                   geo = c("lon", "lat")
#'                  )
#' get_meta(ll)
#' get_meta(ll, dict = FALSE)
get_meta <- function(dat, dictionary = TRUE) {
  stopifnot(inherits(dat, "linelist"))
  res <- attr(dat, "epivars")$meta
  if (!dictionary) {
    return(res)
  }
  dict <- stack(attr(dat, "epivars")$vars, stringsAsFactors = FALSE)
  names(dict) <- c("column", "epivar")
  dict$epivar <- as.character(dict$epivar)
  out <- merge(dict, res, all = TRUE, sort = FALSE, stringsAsFactors = FALSE, drop = FALSE)
  out <- out[match(names(dat), out$column), ]
  rownames(out) <- NULL
  out
}
