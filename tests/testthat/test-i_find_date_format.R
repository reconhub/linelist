
context("test i_find_date_format")

test_that("function handles NULL", {
  expect_true(is.na(i_find_date_format(NULL)))
})




test_that("test Ymd formats", {

  x <- as.Date("2001-01-01")
  
  expected_format <- "%Y-%m-%d"
  names(expected_format) <- "format"
  expected_string <- as.character(x)
  names(expected_string) <- "date"
  
  expect_equal(i_find_date_format(x)["format"], expected_format)
  expect_equal(i_find_date_format(x)["date"], expected_string)

  expect_equal(i_find_date_format(x), i_find_date_format("2001/01/01"))
  expect_equal(i_find_date_format(x), i_find_date_format("2001 01 01"))
  expect_equal(i_find_date_format(x), i_find_date_format("2001_01_01"))
  expect_equal(i_find_date_format(x), i_find_date_format("2001.01.01"))

  expect_equal(i_find_date_format(x), i_find_date_format("this is 2001/01/01"))
  expect_equal(i_find_date_format(x), i_find_date_format("2001 01 01 a date 02-02-02"))
  expect_equal(i_find_date_format(x), i_find_date_format("mixing-2001-01_01 separators-_"))
  expect_equal(i_find_date_format(x), i_find_date_format("c'est ca: 2001.01.01 //../."))

})




test_that("test dmY formats", {

  x <- "21-11-2021"
  
  expected_format <- "%d-%m-%Y"
  names(expected_format) <- "format"
  expected_string <- as.character(x)
  names(expected_string) <- "date"
  
  expect_equal(i_find_date_format(x)["format"], expected_format)
  expect_equal(i_find_date_format(x)["date"], expected_string)

  expect_equal(i_find_date_format(x), i_find_date_format("21/11/2021"))
  expect_equal(i_find_date_format(x), i_find_date_format("21 11 2021"))
  expect_equal(i_find_date_format(x), i_find_date_format("21_11_2021"))
  expect_equal(i_find_date_format(x), i_find_date_format("21.11.2021"))

  expect_equal(i_find_date_format(x), i_find_date_format("21/11/2021 /bla/bla"))
  expect_equal(i_find_date_format(x), i_find_date_format("it's 21 11 2021 a"))
  expect_equal(i_find_date_format(x), i_find_date_format("date 21_11_2021:) "))
  expect_equal(i_find_date_format(x), i_find_date_format("... here 21.11.2021"))


})
