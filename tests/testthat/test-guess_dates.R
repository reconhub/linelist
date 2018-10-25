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
