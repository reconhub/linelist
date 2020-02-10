context("test guess_dates")

x <- c("04 Feb 1982", "19 Sep 2018", "2001-01-01", "2011.12.13",
       "ba;abb;a: 03:11:2012!", "haha... 2013-12-13..",
       "that's a NA", "gender", "not a date", "01__Feb__1999___")

expected_result <- structure(c(4417, 17793, 11323, 15321, 15647, 16052,
                               NA, NA, NA, 10623), class = "Date")


test_that("only characters and factors are expected", {
       
  expect_error(guess_dates(NULL), "guess dates will only work for characters and factors")
  expect_error(guess_dates(pi), "guess dates will only work for characters and factors")

})

test_that("vectors of all missing values are returned unharmed", {
  na <- factor(c(NA, NA))
  expect_warning({  
    expect_identical(guess_dates(na), na) 
  }, "all dates were missing, returning data unchanged")
})

test_that("mixed formats work", {
  expect_equal(guess_dates(x, error_tolerance = 0.8, first_date = as.Date("1980-01-01")),
               expected_result)
})

test_that("excel dates work", {

  xl <- x
  xl[1:2] <- expected_result[1:2] - as.Date("1899-12-30")
  expect_equal(guess_dates(xl, error_tolerance = 0.8, first_date = as.Date("1980-01-01")),
               expected_result)
  xl[1:2] <- expected_result[1:2] - as.Date("1904-01-01")
  expect_equal(guess_dates(xl, error_tolerance = 0.8, first_date = as.Date("1980-01-01"), modern_excel = FALSE),
               expected_result)

})

test_that("date input requires no guesswork", {

  expect_identical(guess_dates(expected_result), expected_result)
  expect_identical(guess_dates(as.POSIXlt(expected_result)), expected_result)
  expect_identical(guess_dates(as.POSIXct(expected_result)), expected_result)

  er <- c(expected_result, as.Date(NA_character_))
  expect_identical(guess_dates(c(expected_result, Sys.Date() + 10)), er)

})

test_that("American dates also work", { 
  y <- c(x, "February the 29th, 2016", "02/29/16")
  er <- c(expected_result, as.Date(c("2016-02-29", "2016-02-29")))
  expect_equal(guess_dates(y, error_tolerance = 0.8, first_date = as.Date("1980-01-01")),
               er)
})

test_that("hms dates also work", {

  y   <- c(x, "February the 29th, 2016 16:37 24", "02/29/16 16:37")
  er  <- c(expected_result, as.Date(c("2016-02-29", "2016-02-29")))
  o   <- getOption("linelist_guess_orders")
  o$US_formats <- c(o$US_formats, "Omdyhm", "Omdyhms")
  res <- guess_dates(y, 
                     orders = o,
                     error_tolerance = 0.8, 
                     first_date = as.Date("1980-01-01"))
  expect_equal(res, er)

})

test_that("The first date defaults to fifty years prior", {

  last_date <-as.Date("2012-11-05")
  first_date <- as.Date("1962-11-05")
  er <- c(expected_result, NA, NA)
  er[er < first_date | er > last_date] <- NA
  z <- c(x, "19/09/18", "09/08/18") # slightly ambiguous dates that are missing
  warn <- "The following 4 dates were not in the correct timeframe \\(1962-11-05 -- 2012-11-05\\):

  original              |  parsed    
  --------              |  ------    
  09/08/18              |  2018-08-09
  09/08/18              |  2018-09-08
  19 Sep 2018           |  2018-09-19
  19/09/18              |  2018-09-19
  haha... 2013-12-13..  |  2013-12-13
"
  
  expect_warning(res <- guess_dates(z, error_tolerance = 1, last_date = last_date),
                 warn)
  expect_equal(res, er)

})


test_that("passing a non-data frame to clean_dates throws an error", {

  x <- structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 4L, 1L, 2L, 3L), .Label = c("", 
    "04-Jul-1985", "12-Sep-1987", "27-Jun-1975"), class = "factor")
  expect_error(clean_dates(x), "x must be a data frame")
  res <- clean_dates(data.frame(date = x), error_tol = 1, first_date = as.Date("1969-4-20"))
  expect_is(res, "data.frame")
  expect_length(res, 1)
  expect_is(res$date, "Date")

})

test_that("passing a non-date as first_date throws an error", {

  expect_error(guess_dates(x, first_date = "18-01-01"), "first_date and last_date must be Date objects.")
  expect_failure(expect_error(guess_dates(x, first_date = "1918-01-01"), "first_date and last_date must be Date objects."))
  expect_error(guess_dates(x, last_date = "18-01-01"), "first_date and last_date must be Date objects.")
  expect_failure(expect_error(guess_dates(x, last_date = "2019-01-01"), "first_date and last_date must be Date objects."))

})
