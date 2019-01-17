context("epivars tests")

oev <- epivars(reset = TRUE)

test_that("epivars will return the default epivars", {
  expect_identical(epivars(), getOption("linelist.epivars"))
})

test_that("users can add new valid epivars", {
  epivars("diddle", "fastidious", "ridiculous", set = TRUE)
  expect_true(all(oev %in% epivars()))
  expect_equal(length(epivars()) - length(oev), 3)
  expect_identical(epivars(), getOption("linelist.epivars"))
})

test_that("epivars can be reset", {
  expect_identical(oev, epivars(reset = TRUE))
  expect_identical(oev, epivars())
})
