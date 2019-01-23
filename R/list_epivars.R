#' Retrieve metadata from a linelist object
#'
#' @param x a `linelist` object
#' @param dictionary if `TRUE`, a column called "*epivar*" is appended to the
#'   metadata table, indicating which standard epi variable columns correspond
#'   to; of `FALSE`, the class of the column replaces it
#' @param epivars_only a `logical` indicating if columns which are not registers
#'   `epivars` should be removed from the output
#' @return a `data.frame` representing metatdata
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

list_epivars <- function(x, epivars_only = FALSE, dictionary = TRUE) {
  stopifnot(inherits(x, "linelist"))
  res <- attr(x, "epivars")$meta
  if (!dictionary) {
    return(res)
  }
  dict <- stack(attr(x, "epivars")$vars, stringsAsFactors = FALSE)
  names(dict) <- c("column", "epivar")
  dict$epivar <- as.character(dict$epivar)
  out <- merge(dict, res, all = TRUE, sort = FALSE, stringsAsFactors = FALSE, drop = FALSE)
  out <- out[match(names(x), out$column), ]
  rownames(out) <- NULL
  if (epivars_only) {
    to_keep <- !is.na(out$epivar)
    out <- out[to_keep, ]
  }
  out
}
