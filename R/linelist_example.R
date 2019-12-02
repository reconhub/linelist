#' show the path to a linelist example file
#'
#' @param name the name of a linelist example file
#'
#' @return a path to a linelist example file
#' @export
#' @author Zhian N. Kamvar
#'
#' @examples
#' linelist_example() # list all of the example files
#'
#' # read in example spelling dictionary
#' sd <- linelist_example("spelling-dictionary.csv")
#' read.csv(sd, stringsAsFactors = FALSE)
#'
#' # read in example coded data
#' coded_data <- linelist_example("coded-data.csv")
#' coded_data <- read.csv(coded_data, stringsAsFactors = FALSE)
#' str(coded_data)
#' coded_data$date <- as.Date(coded_data$date)
linelist_example <- function(name = NULL) {
  if (is.null(name)) {
    list.files(system.file("extdata", package = "linelist"))
  } else {
    system.file("extdata", name, package = "linelist", mustWork = TRUE)
  }
}
