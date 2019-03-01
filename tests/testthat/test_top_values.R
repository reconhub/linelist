context("top_values() tests")

x <- rep(letters, 1:26)
y <- factor(x, rev(letters))

test_that("top values will reject numbers", {

  expect_error(top_values(1:10), "top_values has no method for the class: integer")

})


test_that("top_values returns the same type", {

  xx <- top_values(x, n = 26)
  expect_is(x, "character")
  expect_identical(x, xx)
  
  yy <- top_values(y, n = 26)
  expect_is(y, "factor")
  expect_identical(y, yy)

}) 


test_that("top_values can replace with NA", {

  xx <- top_values(x, n = 24, replacement = NA)
  expect_is(x, "character")
  expect_identical(xx[1:3], rep(NA_character_, 3))
  
  
  yy <- top_values(y, n = 24, replacement = NA)
  expect_is(y, "factor")
  expect_identical(as.character(yy[1:3]), rep(NA_character_, 3))

})
