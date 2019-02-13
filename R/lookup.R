#' Lookup a column name defined by an epivar
#'
#' For use in dplyr pipelines and other functions that need to know the names
#' of columns, the lookup function looks up the column name for a given epivar
#' in your data set.
#'
#' @param x a [linelist][as_linelist] object. 
#' @param epivar the name of an epivar defined in the [dictionary][get_dictionary()].
#' @param symbol when `TRUE`, the column name will be converted to a symbol
#'   for use in non-standard evaulation
#' @return a symbol or character vector of the column name
#' @export
#' @examples
#' dat <- clean_data(messy_data())
#' ll  <- as_linelist(dat,
#'                    id = "id", 
#'                    gender = "gender",
#'                    case_definition = "epi_case_definition",
#'                    date_onset = "date_of_onset", 
#'                    geo_lon = "lon",
#'                    geo_lat = "lat"
#'                   )
#' # Lookup a column name, and return a character
#' lookup(ll, gender, symbol = FALSE)
#' 
#' # If there are two columns, and `symbol = TRUE`, they are returned as a list
#' # of symbols.
#' lookup(ll, geo)
#' 
#' # Using dplyr ---------------------------------------------------
#'
#' if (require("dplyr")) { withAutoprint({
#'
#'   # Two ways to use lookup with dplyr ---------------------------
#'   #
#'   # -- 1. Lookup in the pipe using the . replacement
#'   ll %>% 
#'     group_by(!!lookup(., gender)) %>%  
#'     count(!!lookup(., case_definition))
#'
#'   # You can also just create an alias:
#'   L <- linelist::lookup
#'   ll %>%
#'     group_by( !!L(., gender) ) %>%
#'     count( !!L(., case_definition) )
#'   # -- 2. define temporary variables to lookup
#'   (CASEDEF <- lookup(ll, case_definition)) 
#'   (GEO <- lookup(ll, geo, symbol = FALSE))
#'   ll %>%
#'     group_by(!!CASEDEF) %>%
#'     summarise_at(GEO, mean) # note, summarise_at uses characters
#'
#'   # Note: the linelist class is removed by dplyr verbs ----------
#'   #
#'   ll %>% group_by(!!CASEDEF) %>% class()
#'
#'   # The epivars are retained, so you can use as_linelist() to refresh
#'   # (this even works for summarized data)
#'   ll %>% 
#'     group_by(!!CASEDEF) %>%
#'     as_linelist() %>%
#'     class()
#' })}
lookup <- function(x, ..., symbol = TRUE) {

  ev <- attr(x, "epivars")
  
  if (is.null(ev)) {
    stop("This object does not contain an epivars attribute.")
  }

  .vars <- rlang::quos(...)
  
  if (length(.vars) == 0) return(ev)

  vars <- tidyselect::vars_select(names(ev), !!!.vars)
  res <- unlist(ev[vars], use.names = FALSE)

  if (symbol) {
    res <- if (length(res) == 1) as.symbol(res) else lapply(res, as.symbol)
  }
  res
}
