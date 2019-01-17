context("linelist class tests")

oev <- epivars()
invisible(epivars("date_discharge", "case", set = TRUE))
ll <- as_linelist(clean_data(messy_data()),
                  id = "id", 
                  date_onset = "date_of_onset",
                  date_discharge = "discharge",
                  case = "epi_case_definition",
                  gender = "gender",
                  geo = c("lon", "lat"))
                  

test_that("a linelist is a data frame", {
  expect_is(ll, "data.frame")
  expect_is(ll, "linelist")
})

test_that("a linelist contains the epivars attribute", {
  expect_true("epivars" %in% names(attributes(ll)))
  expect_named(attr(ll, "epivars"), c("vars", "meta"))
  expect_is(attr(ll, "epivars")$vars, "list")
  expect_is(attr(ll, "epivars")$meta, "data.frame")
  expect_named(attr(ll, "epivars")$meta, c("column", "class", "hxl"))
})

test_that("a linelist class will be the same subsetting by nothing", {
  expect_identical(ll, ll[])
})

epivars(reset = TRUE)
