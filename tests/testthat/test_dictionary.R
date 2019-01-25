context("dictionary tests")

oev <- get_dictionary()
hosp <- data.frame(
  epivar = "date_hospital",
  hxl    = "#date +start",
  description = "date at which patient was hospitalized",
  stringsAsFactors = FALSE
)

test_that("epivars will return the default epivars", {
  expect_identical(get_dictionary(), getOption("linelist_dictionary"))
  expect_identical(get_dictionary(), default_dictionary())
})

## TODO add test for changing dictionary
## test_that("users can add new valid epivars", {
##   set_dictionary("diddle", "fastidious", "ridiculous")
##   expect_true(all(oev %in% get_dictionary()))
##   expect_equal(length(get_dictionary()) - length(oev), 3)
## })

test_that("a new dictionary can be added", {
  allhosp <- rbind(default_dictionary(), hosp)
  set_dictionary(hosp)
  expect_identical(get_dictionary(), hosp)
  expect_identical(get_dictionary(), getOption("linelist_dictionary"))
  set_dictionary(allhosp)
  expect_identical(get_dictionary(), allhosp)
  expect_identical(get_dictionary(), getOption("linelist_dictionary"))
})


test_that("epivars can be reset", {
  reset_dictionary()
  expect_identical(default_dictionary(), get_dictionary())
})
