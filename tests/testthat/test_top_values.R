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
                 "a tie among values (a, b) was broken by choosing the first value", fixed = TRUE)
  expect_warning(tv_last <- top_values(x, n = 1, ties.method = "last"), 
                 "a tie among values (a, b) was broken by choosing the last value", fixed = TRUE)
  expect_equal(tv_first, c("a", "other", "a", "other", "other"))
  expect_equal(tv_last, c("other", "b", "other", "b", "other"))


})

test_that("top_values() will respect ties in order of factor", {

  # if the factor is reversed, then the ties method should follow the factor levels
  x <- factor(c("a", "b", "a", "b", "c"), levels = c("c", "b", "a"))
  expect_warning(tv_first <- top_values(x, n = 1),
                 "a tie among values (b, a) was broken by choosing the first value", fixed = TRUE)
  expect_warning(tv_last <- top_values(x, n = 1, ties.method = "last"), 
                 "a tie among values (b, a) was broken by choosing the last value", fixed = TRUE)

  expect_equal(tv_first,
               factor(c("other", "b", "other", "b", "other"), levels = c("b", "other")))
  expect_equal(tv_last,
               factor(c("a", "other", "a", "other", "other"), levels = c("a", "other")))

})


test_that("top_values() will throw a warning if the user uses a non-recommended ties.value", {

  x <- c("b", "a", "c", "a", "a", "b", "d", "d", "c", "b")
  expect_warning(top_values(x, n = 1, ties.method = "min"), 
"using a ties.method other than first, last, or random can give unpredictable results in the event of a tie", fixed = TRUE)

})

test_that("top_values() will change n-1 levels", {

  x <- c("b", "a", "c", "a", "a", "b", "d", "d", "c", "b")
  xf <- factor(x, levels = c("d", "c", "b", "a"))
  expect_warning(tv_char <- top_values(x, n = 3),
                 "a tie among values (c, d) was broken by choosing the first value", fixed = TRUE)
  expect_warning(tv_fact <- top_values(xf, n = 3),
                 "a tie among values (b, a) was broken by choosing the first value", fixed = TRUE)

  expect_equal(tv_char, gsub("d", "other", x))
  expect_equal(levels(tv_fact), c("d", "b", "a", "other"))

})


test_that("top_values() will choose dropped ties based on user input", {

  x <- c("b", "a", "c", "a", "a", "b", "d", "d", "c", "b")
  xf <- factor(x, levels = c("d", "c", "b", "a"))

  expect_warning(tv_char <- top_values(x, n = 3, ties.method = "last"),
                 "a tie among values (c, d) was broken by choosing the last value", fixed = TRUE)

  expect_warning(tv_fact <- top_values(xf, n = 3, ties.method = "last"),
                 "a tie among values (b, a) was broken by choosing the last value", fixed = TRUE)

  expect_equal(tv_char, gsub("c", "other", x))
  expect_equal(levels(tv_fact), c("c", "b", "a", "other"))

  expect_warning(top_values(c(x, "e", "e"), n = 3, ties.method = "first"),
                 "a tie among values (c, d, e) was broken by choosing the first value", fixed = TRUE)

  expect_warning(top_values(c(x, "e", "e", "f", "f"), n = 3, ties.method = "first"),
                 "a tie among values (c, d, e, f) was broken by choosing the first value", fixed = TRUE)

})


test_that("top_values() will drop a value randomly", {

  set.seed(2019-09-23)
  wrn <- "a tie among values (a, b, ..., z) was broken by choosing a value at random"
  expect_warning(lttrs1 <- top_values(letters, n = 25, ties.method = "random"), wrn, fixed = TRUE)
  expect_warning(lttrs2 <- top_values(letters, n = 25, ties.method = "random"), wrn, fixed = TRUE)

  expect_equal(sum(lttrs1 %in% letters), 25L)
  expect_equal(sum(lttrs2 %in% letters), 25L)
  expect_failure(expect_identical(lttrs1, lttrs2))

})
