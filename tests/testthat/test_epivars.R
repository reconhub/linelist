context("epivars tests")

oev <- get_dictionary(reset = TRUE)

test_that("epivars will return the default epivars", {
  expect_identical(get_dictionary(), getOption("linelist.epivars"))
})

test_that("users can add new valid epivars", {
  get_dictionary("diddle", "fastidious", "ridiculous", set = TRUE)
  expect_true(all(oev %in% get_dictionary()))
  expect_equal(length(get_dictionary()) - length(oev), 3)
  expect_identical(get_dictionary(), getOption("linelist.epivars"))
})

test_that("epivars can be reset", {
  expect_identical(oev, get_dictionary(reset = TRUE))
  expect_identical(oev, get_dictionary())
})
