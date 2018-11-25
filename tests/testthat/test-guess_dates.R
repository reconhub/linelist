context("test guess_dates")

test_that("test mixed formats", {

  x <- c("04 Feb 1982", "19 Sep 2018", "2001-01-01", "2011.12.13",
         "ba;abb;a: 03:11:2012!", "haha... 2013-12-13..",
         "that's a NA", "gender", "not a date", "01__Feb__1999___")
  
  expected_result <- structure(c(4417, 17793, 11323, 15321, 15647, 16052,
                                 NA, NA, NA, 10623), class = "Date")

  expect_equal(guess_dates(x, error_tolerance = 0.8),
               expected_result)

})

test_that("passing a non-data frame throws an error", {

  x <- structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 4L, 1L, 2L, 3L), .Label = c("", 
    "04-Jul-1985", "12-Sep-1987", "27-Jun-1975"), class = "factor")
  expect_error(clean_dates(x), "x must be a data frame")
  res <- clean_dates(data.frame(date = x), error_tol = 1)
  expect_is(res, "data.frame")
  expect_length(res, 1)
  expect_is(res$date, "Date")

})
