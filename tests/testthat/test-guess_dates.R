context("test guess_dates")

x <- c("04 Feb 1982", "19 Sep 2018", "2001-01-01", "2011.12.13",
       "ba;abb;a: 03:11:2012!", "haha... 2013-12-13..",
       "that's a NA", "gender", "not a date", "01__Feb__1999___")

expected_result <- structure(c(4417, 17793, 11323, 15321, 15647, 16052,
                               NA, NA, NA, 10623), class = "Date")


test_that("mixed formats work", {
  expect_equal(guess_dates(x, error_tolerance = 0.8, first_date = as.Date("1980-01-01")),
               expected_result)
})


test_that("American dates also work", { 
  y <- c(x, "February the 29th, 2016", "02/29/16")
  er <- c(expected_result, as.Date(c("2016-02-29", "2016-02-29")))
  expect_equal(guess_dates(y, error_tolerance = 0.8, first_date = as.Date("1980-01-01")),
               er)
})


test_that("The first date defaults to on year prior", {
  last_date <- as.Date("2012-11-05")
  first_date <- as.Date("2011-11-05")
  er <- expected_result
  er[er < first_date | er > last_date] <- NA
  expect_warning(res <- guess_dates(x, error_tolerance = 1, last_date = last_date),
  "original")
  expect_equal(res, er)
})


test_that("passing a non-data frame throws an error", {

  x <- structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 4L, 1L, 2L, 3L), .Label = c("", 
    "04-Jul-1985", "12-Sep-1987", "27-Jun-1975"), class = "factor")
  expect_error(clean_dates(x), "x must be a data frame")
  res <- clean_dates(data.frame(date = x), error_tol = 1, first_date = as.Date("1969-4-20"))
  expect_is(res, "data.frame")
  expect_length(res, 1)
  expect_is(res$date, "Date")

})

test_that("passing a non-date as first_date throws an error", {
  expect_error(guess_dates(x, first_date = "2018-01-01"), "first_date and last_date must be Date objects.")
})
