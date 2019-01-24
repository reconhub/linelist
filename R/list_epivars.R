#' List registred epivars in a linelist object
#'
#' @param x a `linelist` object
#' 
#' @param simple a logical indicating if a single vector of names of present
#'   `epivars` should be returned (`TRUE`); otherwise the output will contain
#'   more information as a `data.frame` (`FALSE`, default)
#' 
#' @param all_dictionary if `FALSE` (default), only the *epivars* present in the
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
#'                   date_onset = "date_of_onset", 
#'                   gender = "gender",
#'                   geo = c("lon", "lat")
#'                  )
#' list_epivars(ll)
#' list_epivars(ll, simple = TRUE)
#' list_epivars(ll, all_dictionary = TRUE)

list_epivars <- function(x, simple = FALSE, all_dictionary = FALSE) {
  stopifnot(inherits(x, "linelist"))
  content <- attr(x, "epivars")
  if (length(content) == 0L ) {
    return(NULL)
  }
  if (simple) {
    out <- names(content)
    return(out)
  }
  current_dict <- get_dictionary()
  
  epivars <- stack(content, stringsAsFactors = FALSE)
  
  names(epivars) <- c("column", "epivar")
  current_dict$epivar <- as.character(current_dict$epivar)
  
  out <- merge(epivars, current_dict, by.x = "epivar",
               all.x = TRUE, all.y = all_dictionary,
               sort = FALSE)
  out[3] <- NULL # remove duplicate column
  rownames(out) <- NULL
  out
}
