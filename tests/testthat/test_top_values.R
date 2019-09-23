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


test_that("top_values() will respect ties", {

  x <- c("a", "b", "a", "b", "c")
  expect_equal(top_values(x, n = 1), c("a", "other", "a", "other", "other"))
  expect_equal(top_values(x, n = 1, ties.method = "last"), c("other", "b", "other", "b", "other"))


})

test_that("top_values() will respect ties in order of factor", {

  # if the factor is reversed, then the ties method should follow the factor levels
  x <- factor(c("a", "b", "a", "b", "c"), levels = c("c", "b", "a"))
  expect_equal(top_values(x, n = 1), 
               factor(c("other", "b", "other", "b", "other"), levels = c("b", "other")))
  expect_equal(top_values(x, n = 1, ties.method = "last"), 
               factor(c("a", "other", "a", "other", "other"), levels = c("a", "other")))

})


test_that("top_values() will change n-1 levels", {

  x <- c("b", "a", "c", "a", "a", "b", "d", "d", "c", "b")
  xf <- factor(x, levels = c("d", "c", "b", "a"))
  expect_equal(top_values(x, n = 3), gsub("d", "other", x))
  expect_equal(levels(top_values(xf, n = 3)), c("d", "b", "a", "other"))

})


test_that("top_values() will choose dropped ties based on user input", {

  x <- c("b", "a", "c", "a", "a", "b", "d", "d", "c", "b")
  xf <- factor(x, levels = c("d", "c", "b", "a"))
  expect_equal(top_values(x, n = 3, ties.method = "last"), gsub("c", "other", x))
  expect_equal(levels(top_values(xf, n = 3, ties.method = "last")), c("c", "b", "a", "other"))

})


test_that("top_values() will drop a value randomly", {

  set.seed(2019-09-23)
  lttrs1 <- top_values(letters, n = 25, ties.method = "random")
  lttrs2 <- top_values(letters, n = 25, ties.method = "random")
  expect_equal(sum(lttrs1 %in% letters), 25L)
  expect_equal(sum(lttrs2 %in% letters), 25L)
  expect_failure(expect_identical(lttrs1, lttrs2))

})
