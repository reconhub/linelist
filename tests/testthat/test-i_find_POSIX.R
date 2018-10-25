
context("test i_find_POSIX")

test_that("function handles NULL", {
  expect_equal(i_find_POSIX(NULL), integer(0))
})


test_that("function finds POSIX in a a data.frame", {

  x <- data.frame(
      a = character(10),
      b = as.POSIXlt(as.Date("2001-01-01") + 1:10),
      c = numeric(10),
      d = factor(1:10),
      e = as.Date("2001-01-01") + 1:10,
      f = as.POSIXct(as.Date("2001-01-01") + 1:10),
      stringsAsFactors = FALSE
  )

  expected_result <- c(2L, 6L)

  expect_equal(i_find_POSIX(x), expected_result)
  
})



test_that("function works with no POSIX", {

  x <- data.frame(
      a = character(10),
      c = numeric(10),
      d = factor(1:10),
      e = as.Date("2001-01-01") + 1:10
  )


  expected_result <- integer(0)

  expect_equal(i_find_POSIX(x), expected_result)

})



