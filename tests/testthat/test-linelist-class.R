context("linelist class tests")

oev <- get_dictionary()
ll <- as_linelist(clean_data(messy_data()),
                  id = "id", 
                  date_onset = "date_of_onset",
                  case_definition = "epi_case_definition",
                  gender = "gender",
                  geo = c("lon", "lat"))
                  

test_that("a linelist is a data frame", {
  expect_is(ll, "data.frame")
  expect_is(ll, "linelist")
})

test_that("a linelist contains the epivars attribute", {
  expect_true("epivars" %in% names(attributes(ll)))
  expect_is(attr(ll, "epivars"), "list")
})

test_that("a linelist class will be the same subsetting by nothing", {
  expect_identical(ll, ll[])
})

reset_dictionary()
