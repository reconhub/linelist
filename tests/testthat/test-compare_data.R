context("data comparison tests") 


test_that("identical data will return a list of TRUEth", {

  res   <- compare_data(iris, iris)
 
  expected <- list(dim = TRUE, names = TRUE, classes = TRUE, values = TRUE)
  class(expected) <- "data_comparison"

  expect_identical(res, expected)

  expect_output(print(res), "Comparisons of data content")

})

test_that("Same content in different order will say so", {

  res <- compare_data(iris, rev(iris))
  expect_is(res, "data_comparison")
  expect_identical(res$names, list(different_order = TRUE))
  expect_identical(names(res$classes), names(iris))
  expect_identical(vapply(res$classes, "[[", character(1), 1), i_find_classes(iris))
  expect_true(res$values)
  expect_true(res$dim)
  

})


test_that("different classes will trigger", {

  ichar <- iris
  ichar$Species <- as.character(iris$Species)
  res <- compare_data(iris, ichar)
  expect_true(res$values)
  expect_true(res$dim)
  expect_true(res$names)
  expect_is(res$classes, "list")
  expect_equal(unname(res$classes$Species), c("Species", "factor", "character"))
  expect_output(print(res), "`Species` has changed from `factor` to `character`")
  expect_output(print(res), "`Sepal.Length`: same class \\(numeric\\)")
  expect_failure(expect_output(print(res, diff_only = TRUE), "`Sepal.Length`: same class \\(numeric\\)"))

})


test_that("different variables will trigger", {

  ichar <- iris
  levels(ichar$Species) <- c("hickory", "dickory", "setosa")
  res <- compare_data(iris, ichar)
  expect_true(res$dim)
  expect_true(res$names)
  expect_true(res$classes)
  expect_is(res$values, "list")
  expect_named(res$values, "Species")
  expect_setequal(res$values$Species$new, c("hickory", "dickory"))
  expect_setequal(res$values$Species$missing, c("versicolor", "virginica"))
  expect_setequal(res$values$Species$common, "setosa")
  expect_output(print(res), "Missing values in `Species`")
  expect_output(print(res, common_values = FALSE), "1 value\\(s\\) \\[hidden\\]")

})

test_that("users can choose columns to keep/ignore", {

  ichar <- iris
  levels(ichar$Species) <- c("hickory", "dickory", "setosa")
  res <- compare_data(iris, ichar, columns = -5)
  expect_true(all(unlist(res)))

})

test_that("completely different data will render incomparable", {

  res <- compare_data(iris, mtcars)
  expect_output(print(res), "new variables")
  expect_length(res$dim, 2)
  expect_named(res$names, c("missing", "new", "common"))
  expect_true(res$classes$cannot_compare)
  expect_true(res$values$cannot_compare)

})
