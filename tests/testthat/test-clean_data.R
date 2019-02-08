context("clean_data() tests")

set.seed(2019-02-09)
DAT <- seq(as.Date("2018-01-01"), length.out = 10, by = "1 day")
POS <- as.POSIXlt(DAT, tz = "UTC")
POS2 <- POS + (86400 * sample(10, 10, replace = TRUE)) + sample(86400, 10)
md  <- messy_data(10)
md$`!!Date of Admission` <- DAT
md$`Time of admission`   <- POS
md$`Time of   discharge` <- POS2

expected_colnames <- c("id", "date_of_onset", "discharge", "gender",
                       "epi_case_definition", "messy_dates", "lat", "lon",
                       "date_of_admission", "time_of_admission",
                       "time_of_discharge")
expected_comment <- setNames(names(md), expected_colnames)

test_that("clean_data() needs a data frame with columns and/or rows", {
  expect_error(clean_data(DAT), "DAT is not a data frame")
  expect_error(clean_data(data.frame()), "data.frame\\(\\) has no columns")
}) 

test_that("messy data will be clean", {
  cd <- clean_data(md, error_tolerance = 0.8)
  # The names are cleaned
  expect_named(cd, expected_colnames)
  # The original names are preserved as a comment
  expect_identical(comment(cd), expected_comment)
  expect_is(cd$id, "character")
  expect_is(cd$date_of_onset, "Date")
  expect_is(cd$discharge, "Date")
  expect_is(cd$gender, "character")
  expect_is(cd$epi_case_definition, "character")
  expect_is(cd$messy_dates, "Date")
  expect_is(cd$lat, "numeric")
  expect_is(cd$lon, "numeric")
  expect_is(cd$date_of_admission, "Date")
  expect_is(cd$time_of_admission, "Date")
  expect_is(cd$time_of_discharge, "Date")
})


test_that("columns can be protected", {
  cd  <- clean_data(md, error_tolerance = 0.8)
  cdp <- clean_data(md, protect = 6, error_tolerance = 0.8)
  expect_identical(cdp[-6], cd[-6])
  expect_identical(cdp[6], md[6])
  expect_identical(comment(cdp)[-6], expected_comment[-6])
})

test_that("numbers or logicals are required for protection", {
  expect_error(clean_data(md, protect = "messy/dates"), "protect must be a logical or integer vector")
})

test_that("Dates won't be forced", {
  cdd <- clean_data(md, force_Date = FALSE, error_tolerance = 0.8)
  expect_named(cdd, expected_colnames)
  # The original names are preserved as a comment
  expect_identical(comment(cdd), expected_comment)
  expect_is(cdd$id, "character")
  expect_is(cdd$date_of_onset, "Date")
  expect_is(cdd$discharge, "Date")
  expect_is(cdd$gender, "character")
  expect_is(cdd$epi_case_definition, "character")
  expect_is(cdd$messy_dates, "Date")
  expect_is(cdd$lat, "numeric")
  expect_is(cdd$lon, "numeric")
  expect_is(cdd$date_of_admission, "Date")
  expect_is(cdd$time_of_admission, "POSIXt")
  expect_is(cdd$time_of_discharge, "POSIXt")

})

test_that("protect overrides columns specified in force_Date and guess_dates", {
  cdf <- clean_data(md, force_Date = 11, guess_dates = 6, protect = c(6, 11))
  expect_identical(md[6], cdf[6])
  expect_identical(md[11], cdf[11])
})
