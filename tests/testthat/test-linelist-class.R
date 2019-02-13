context("linelist class tests")

oev <- get_dictionary()
ll <- as_linelist(clean_data(messy_data(), first_date = as.Date("1969-4-20")),
                  id = "id", 
                  date_onset = "date_of_onset",
                  case_definition = "epi_case_definition",
                  gender = "gender",
                  geo_lon = "lon",
                  geo_lat = "lat",
                  NULL
)
                  

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

test_that("the epivars attribute will reflect the order of the linelist class", {
  rll <- rev(ll)
  evll <- unlist(attr(rll, "epivars"), use.names = FALSE)
  expect_identical(evll, names(rll)[names(rll) %in% evll])
})
reset_dictionary()
