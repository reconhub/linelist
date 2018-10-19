
context("test i_find_classes")

test_that("function handles NULL", {
  expect_equal(i_find_classes(NULL), character(0))
})


test_that("function finds classes of a data.frame", {

  x <- data.frame(
      a = character(10),
      b = integer(10),
      c = numeric(10),
      d = factor(1:10),
      e = as.Date("2001-01-01") + 1:10,
      stringsAsFactors = FALSE
  )

  expected_result <- c(a = "character", b = "integer", c = "numeric", d = "factor", 
                       e = "Date")

  expect_equal(i_find_classes(x), expected_result)
  
})



test_that("function works with several class attributes - returns first one", {

  x <- data.frame(
      a = structure(1:10, class = c("toto", "numeric"))
  )

  expected_result <- "toto"

  expect_true(all(i_find_classes(x) == expected_result))

})



