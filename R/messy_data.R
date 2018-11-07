#' Function for generating messy data
#' @param n the number of cases to generate
#' @return a data frame with "messy" data
#' @export
#' @examples
#' messy_data(n = 5)
#' messy_data()
messy_data <- function(n = 20) {
  onsets <- as.Date("2018-01-01") + sample(1:10, n, replace = TRUE)
  discharge <- format(as.Date(onsets) + 10, "%d/%m/%Y")
  genders <- c("male", "female", "FEMALE", "Male", "Female", "MALE")
  gender <- sample(genders, n, replace = TRUE)
  case_types <- c("confirmed", "probable", "suspected", "not a case",
                  "Confirmed", "PROBABLE", "suspected  ", "Not.a.Case")
  messy_dates <- sample(
                   c("01-12-2001", "male", "female", "2018-10-18", "2018_10_17",
                     "2018 10 19", "// 24//12//1989", NA, "that's 24/12/1989!"),
                   n, replace = TRUE)
  ID <- replicate(n, paste(sample(letters, 6, replace = TRUE), collapse = ""))
  case <- factor(sample(case_types, n, replace = TRUE))
  toy_data <- data.frame("'ID" = ID,
                         "Date of Onset." = onsets,
                         "DisCharge.." = discharge,
                         "GENDER_ " = gender,
                         "\u00c9pi.Case_d\u00e9finition" = case,
                         "messy/dates" = messy_dates,
                         check.names = FALSE)
  toy_data
}
