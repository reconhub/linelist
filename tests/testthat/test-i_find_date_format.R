
context("test i_find_date_format")

## test_that("function handles NULL", {
## })


test_that("test Ymd formats", {

  x <- as.Date("2001-01-01")
  
  expected_result <- "%Y-%m-%d"
  
  expect_equal(i_find_date_format(x), expected_result)
  
})
