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
                  
test_that("linelist announces itself", {

  expect_output(print(ll), "linelist")
  expect_output(print(ll, show_epivars = TRUE), "epivars")

})

test_that("a linelist is a data frame", {

  expect_is(ll, "data.frame")
  expect_is(ll, "linelist")

})

test_that("a linelist contains the epivars attribute", {

  expect_true("epivars" %in% names(attributes(ll)))
  expect_is(attr(ll, "epivars"), "list")
  
})

test_that("set_epivars() will return the original linelist unharmed", {

  expect_identical(ll, set_epivars(ll))

})

test_that("[re]set_epivars() will create a linelist", {

  ll2 <- set_epivars(as.data.frame(ll), 
                     id = "id", 
                     date_onset = "date_of_onset",
                     case_definition = "epi_case_definition",
                     gender = "gender",
                     geo_lon = "lon",
                     geo_lat = "lat",
                     NULL
  )

  ll3 <- reset_epivars(as.data.frame(ll), 
                       id = "id", 
                       date_onset = "date_of_onset",
                       case_definition = "epi_case_definition",
                       gender = "gender",
                       geo_lon = "lon",
                       geo_lat = "lat",
                       NULL
  )
  expect_identical(ll, ll2)
  expect_identical(ll, ll3)

})

test_that("reset_epivars() will return nothing if given nothing", {

  expect_length(attr(reset_epivars(ll), "epivars"), 0)

})

test_that("a linelist class will be the same subsetting by nothing", {

  expect_identical(ll, ll[])

})


test_that("the epivars attribute will reflect the order of the linelist class", {

  rll <- rev(ll)
  evll <- unlist(attr(rll, "epivars"), use.names = FALSE)
  expect_identical(evll, names(rll)[names(rll) %in% evll])

})


test_that("epivars will be subset along with the linelist", {
  
  llsub <- ll[, c("epi_case_definition", "lon", "lat")]
  expect_named(attr(llsub, "epivars"), c("case_definition", "geo_lon", "geo_lat"))

})


test_that("epivars can be removed", {
  
   llnocase <- set_epivars(ll, case_definition = NULL, id = NULL)
   expect_false("case_definition" %in% attr(llnocase, "epivars"))
   expect_false("id" %in% attr(llnocase, "epivars"))
})

reset_dictionary()
