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
  expect_warning(tv_first <- top_values(x, n = 1),
                 "a tie among values (a, b) was broken by choosing the first value",
                 fixed = TRUE)
  expect_warning(tv_last <- top_values(x, n = 1, ties_method = "last"), 
                 "a tie among values (a, b) was broken by choosing the last value",
                 fixed = TRUE)
  expect_equal(tv_first, c("a", "other", "a", "other", "other"))
  expect_equal(tv_last, c("other", "b", "other", "b", "other"))

})

test_that("warnings will not be given in the event of a non-tie", {

  x <- c("a", "b", "a", "b", "c")
  expect_failure(expect_warning(res <- top_values(x, n = 2)))
  expect_equal(res, gsub("c", "other", x))

})


test_that("top_values() will respect ties in order of factor", {

  ## if the factor is reversed, then the ties method should follow the factor levels
  x <- factor(c("a", "b", "a", "b", "c"), levels = c("c", "b", "a"))
  expect_warning(tv_first <- top_values(x, n = 1),
                 "a tie among values (b, a) was broken by choosing the first value",
                 fixed = TRUE)
  expect_warning(tv_last <- top_values(x, n = 1, ties_method = "last"), 
                 "a tie among values (b, a) was broken by choosing the last value",
                 fixed = TRUE)

  expect_equal(tv_first,
               factor(c("other", "b", "other", "b", "other"),
                      levels = c("b", "other")))
  expect_equal(tv_last,
               factor(c("a", "other", "a", "other", "other"),
                      levels = c("a", "other")))

})


test_that("top_values() will throw a warning if the user uses a non-recommended ties.value", {

  x <- c("b", "a", "c", "a", "a", "b", "d", "d", "c", "b")
  msg <- paste0("using a ties_method other than first, last, or random ",
                "can give unpredictable results in the event of a tie")
  expect_warning(
      top_values(x, n = 1, ties_method = "min"),
      msg,
      fixed = TRUE)

})

test_that("top_values() will change n-1 levels", {

  x <- c("b", "a", "c", "a", "a", "b", "d", "d", "c", "b")
  xf <- factor(x, levels = c("d", "c", "b", "a"))

  msg <- "a tie among values (c, d) was broken by choosing the first value"
  expect_warning(tv_char <- top_values(x, n = 3),
                 msg,
                 fixed = TRUE)
  msg <- "a tie among values (d, c) was broken by choosing the first value"
  expect_warning(tv_fact <- top_values(xf, n = 3),
                 msg,
                 fixed = TRUE)

  expect_equal(tv_char, gsub("d", "other", x))
  expect_equal(levels(tv_fact), c("d", "b", "a", "other"))

})


test_that("top_values() will choose dropped ties based on user input", {

  x <- c("b", "a", "c", "a", "a", "b", "d", "d", "c", "b")
  xf <- factor(x, levels = c("d", "c", "b", "a"))

  msg <- "a tie among values (c, d) was broken by choosing the last value" 
  expect_warning(tv_char <- top_values(x, n = 3, ties_method = "last"),
                 msg,
                 fixed = TRUE)

  msg <- "a tie among values (d, c) was broken by choosing the last value"
  expect_warning(tv_fact <- top_values(xf, n = 3, ties_method = "last"),
                 msg,
                 fixed = TRUE)

  expect_equal(tv_char, gsub("c", "other", x))
  expect_equal(levels(tv_fact), c("c", "b", "a", "other"))

  msg <- "a tie among values (c, d, e) was broken by choosing the first value"
  expect_warning(top_values(c(x, "e", "e"), n = 3, ties_method = "first"),
                 msg,
                 fixed = TRUE)

  msg <- "a tie among values (c, d, e, f) was broken by choosing the first value"
  expect_warning(top_values(c(x, "e", "e", "f", "f"), n = 3, ties_method = "first"),
                 msg,
                 fixed = TRUE)

})


test_that("top_values() will drop a value randomly", {

  set.seed(2019-09-23)
  wrn <- paste0("a tie among values (a, b, ..., z) ",
                "was broken by choosing a value at random")
  expect_warning(lttrs1 <- top_values(letters, n = 25, ties_method = "random"),
                 wrn,
                 fixed = TRUE)
  expect_warning(lttrs2 <- top_values(letters, n = 25, ties_method = "random"),
                 wrn,
                 fixed = TRUE)

  expect_equal(sum(lttrs1 %in% letters), 25L)
  expect_equal(sum(lttrs2 %in% letters), 25L)
  expect_failure(expect_identical(lttrs1, lttrs2))

})


test_that("top_values() with subsetting", {

  x <- c("a", "a", "a", "b", "c", "b")

  ## basic use of subset
  expect_identical(top_values(x, n = 1, subset = 4:6),
                   c("other", "other", "other", "b", "other", "b"))
  expect_identical(top_values(x, n = 2, subset = 4:6),
                   c("other", "other", "other", "b", "c", "b"))

  ## creating a tie
  msg <- "a tie among values (a, b) was broken by choosing the first value"
  expect_warning(res <- top_values(x, n = 1, subset = -1),
                 msg,
                 fixed = TRUE)
  expect_identical(res,
                   c("a", "a", "a", "other", "other", "other"))

  msg <- "a tie among values (a, b) was broken by choosing the last value"
  expect_warning(res <- top_values(x, n = 1, subset = -1, ties_method = "last"),
                 msg,
                 fixed = TRUE)
  expect_identical(res,
                   c("other", "other", "other", "b", "other", "b"))


  ## handle subsetting retaining nothing
  msg <- "`subset` does not retain any input"
  expect_error(top_values(x, n = 3, subset = FALSE),
               msg,
               fixed = TRUE)
  expect_error(top_values(x, n = 3, subset = integer(0)),
               msg,
               fixed = TRUE)
  
})

test_that("top_values() will not return a warning if there is nothing to label as other", {

 ## see https://github.com/reconhub/linelist/issues/96
 expect_failure(expect_warning(top_values(c("b", "b", "b"), n = 1, subset = c(FALSE, TRUE, TRUE))))

})


test_that("top_values() works with ghost levels", {

  ## this is from issue 92
  res <- top_values(factor(c('b', 'a'))[-1], n = 1)
  expect_identical(res, factor("a"))

  ## variant from the original post, using subset
  x_test_bad <- c("vuhovi", "beni", "beni")
  x_subset <- c(FALSE, TRUE, TRUE)
  res <- top_values(x = x_test_bad, n = 1, subset = x_subset)
  expect_identical(res, c("other", "beni", "beni"))
  
})
