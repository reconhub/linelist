context("clean_dates() tests")

## make toy data
onsets <- as.POSIXct("2018-01-01", tz = "UTC")
onsets <- seq(onsets, by = "1 day", length.out = 10)
onsets <- sample(onsets, 20, replace = TRUE)
onsets2 <- format(as.Date(onsets), "%d/%m/%Y")
onsets3 <- format(as.Date(onsets), "%d %m %Y")
outcomes <- onsets + 1e7
admissions <- onsets + 86400 + sample(86400, 20)
admissions[1:5] <- NA
discharges <- admissions + (86400 * sample(5, 20, replace = TRUE)) + sample(86400, 20)
onset_with_errors <- onsets2
onset_with_errors[c(1,20)] <- c("male", "confirmed")
mixed_info <- onsets3
mixed_info[1:10] <- sample(c("bleeding", "fever"), 10, replace = TRUE)
gender <- sample(c("male", "female"), 20, replace = TRUE)
case_type <- c("confirmed", "probable", "suspected", "not a case")
case <- sample(case_type, 20, replace = TRUE)
toy_data <- data.frame("Date of Onset." = onsets,
                       "onset 2" = onsets2,
                       "ONSET 3" = onsets3,
                       "onset_4" = onset_with_errors,
                       "date admission" = admissions,
                       "DATE.of.DISCHARGE" = discharges,
                       "GENDER_ " = gender,
                       "Épi.Case_définition" = case,
                       "date of admission" = admissions,
                       "Date-of_discharge" = discharges,
                       "extra" = mixed_info,
                       stringsAsFactors = FALSE,
                       check.names = FALSE)
cleaned <- clean_variable_names(toy_data)

test_that("clean_dates() will do not a damn thing if both force and guess are off", {
  expect_identical(clean_dates(cleaned, force_Date = FALSE, guess_dates = FALSE), cleaned)
})


test_that("clean_dates() will indiscriminantly parse dates if a tolerance of one is passed", {
  cd1 <- clean_dates(cleaned, error_tolerance = 1)                       
  for (i in names(cd1)) {
    expect_is(cd1[[i]], "Date", info = i)
  }
})


test_that("clean_dates() will avoid parsing possible dates if a tolerance of zero is passed", {
  cd2 <- clean_dates(cleaned, error_tolerance = 0)
  expect_identical(cd2$onset_4, cleaned$onset_4)
  expect_identical(cd2$gender, cleaned$gender)
  expect_identical(cd2$epi_case_definition, cleaned$epi_case_definition)
  expect_identical(cd2$extra, cleaned$extra)
  expect_is(cd2$onset_2, "Date")
  expect_is(cd2$date_of_discharge, "Date")
})


test_that("clean_dates() will parse only the columns passed", {
  onsets <- grep("onset", names(cleaned))
  cd3    <- clean_dates(cleaned, force_Date = onsets, guess_dates = onsets)
  for (i in onsets) {
    expect_is(cd3[[i]], "Date", info = names(cleaned)[i])
  }
  expect_is(cd3$date_admission, "POSIXt")
})


test_that("clean_dates() will be able to parse mixed logical and integer", {
  dates <- grep("date", names(cleaned))
  ldates <- seq_along(cleaned) %in% dates
  not_discharge <- !grepl("discharge", names(cleaned))
  cd4 <- clean_dates(cleaned, force_Date = not_discharge, guess_dates = dates)
  expect_is(dates, "integer")
  expect_is(not_discharge, "logical")
  for (i in names(cd4)[!not_discharge]) {
    expect_is(cd4[[i]], "POSIXt", info = i)
  }
  for (i in names(cd4)[ldates & not_discharge]) {
    expect_is(cd4[[i]], "Date", info = i)
  }
})
