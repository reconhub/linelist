#' List registred epivars in a linelist object
#'
#' @param x a `linelist` object
#' 
#' @param simple a logical indicating if a single vector of names of present
#'   `epivars` should be returned (`TRUE`); otherwise the output will contain
#'   more information as a `data.frame` (`FALSE`, default)
#' 
#' @param dictionary if `TRUE`, a column called "*epivar*" is appended to the
#'   metadata table, indicating which standard epi variable columns correspond
#'   to; of `FALSE`, the class of the column replaces it
#' 
#' @param epivars_only a `logical` indicating if columns which are not registers
#'   `epivars` should be removed from the output
#' 
#' @return if `simple` is `TRUE`, a vector of characters giving the names of
#'   available `epivariables`; otherwise, a `data.frame` giving more information
#'   about these variables
#' 
#' @export
#' 
#' @examples
#' dat <- clean_data(messy_data())
#' ll  <- as_linelist(dat,
#'                   id = "id", 
#'                   date_onset = "date_of_onset", 
#'                   gender = "gender",
#'                   geo = c("lon", "lat")
#'                  )
#' list_epivars(ll)
#' list_epivars(ll, simple = TRUE)
#' list_epivars(ll, epivars_only = TRUE)
#' list_epivars(ll, dictionary = FALSE)

list_epivars <- function(x, simple = FALSE, epivars_only = FALSE, dictionary = TRUE) {
  stopifnot(inherits(x, "linelist"))
  if (simple) {
    out <- names(attr(x, "epivars")$vars)
    return(out)
  }
  out <- attr(x, "epivars")$meta
  if (!dictionary) {
    return(out)
  }
  dict <- stack(attr(x, "epivars")$vars, stringsAsFactors = FALSE)
  names(dict) <- c("column", "epivar")
  dict$epivar <- as.character(dict$epivar)
  out <- merge(dict, out, all = TRUE, sort = FALSE, stringsAsFactors = FALSE, drop = FALSE)
  out <- out[match(names(x), out$column), ]
  rownames(out) <- NULL
  if (epivars_only) {
    to_keep <- !is.na(out$epivar)
    out <- out[to_keep, ]
  }
  out
}
