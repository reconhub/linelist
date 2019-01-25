#' List registred epivars in a linelist object
#'
#' @param x a `linelist` object
#' 
#' @param simple a logical indicating if a single vector of names of present
#'   `epivars` should be returned (`TRUE`); otherwise the output will contain
#'   more information as a `data.frame` (`FALSE`, default)
#' 
#' @param full_dict if `FALSE` (default), only the *epivars* present in the
#'   object are documented; otherwise, all the available dictionary is appended
#'   to the output
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
#'                   gender = "gender",
#'                   date_onset = "date_of_onset", 
#'                   geo = c("lon", "lat")
#'                  )
#' list_epivars(ll)
#' list_epivars(ll, simple = TRUE)
#' list_epivars(ll, full_dict = TRUE)

list_epivars <- function(x, simple = FALSE, full_dict = FALSE) {
  stopifnot(inherits(x, "linelist"))
  content <- attr(x, "epivars")
  if (length(content) == 0L ) {
    return(NULL)
  }
  if (simple) {
    out <- names(content)
    return(out)
  }

  epivars        <- stack(order_epivars(x, content))
  names(epivars) <- c("column", "epivar")

  current_dict        <- get_dictionary()
  current_dict$epivar <- as.character(current_dict$epivar)
 
  # merge, retaining the order of the data 
  out <- merge(x = epivars, 
               y = current_dict, 
               by.x = "epivar",
               all.x = TRUE, 
               all.y = full_dict,
               sort = FALSE)
  rownames(out) <- NULL
  out
}
