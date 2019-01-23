#' @export
#' @rdname dictionary

default_dictionary <- function() {
  c("id",          # Unique identification
    "date_onset",  # Date of symptom onset
    "date_report", # Date of reporting
    "gender",      # Gender of individual
    "age",         # Age of individual
    "age_group",   # Age grouping
    "geo"          # Geographical coordinates (must be two columns)
    )  
}
