context("linelist class tests")

md <- messy_data(10)
cd <- clean_data(md)
ll <- as_linelist(cd, 
                  id = "id", 
                  date_onset = "date_of_onset", 
                  gender = "gender",
                  geo = c("lon", "lat")
                 )
                  

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

