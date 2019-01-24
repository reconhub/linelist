context("get_epivars() tests")


oev <- get_dictionary()
invisible(set_dictionary("date_discharge", "case"))
ll <- as_linelist(tibble::as_tibble(clean_data(messy_data())),
                  id = "id", 
                  date_onset = "date_of_onset",
                  date_discharge = "discharge",
                  case = "epi_case_definition",
                  gender = "gender",
                  geo = c("lon", "lat"))
dfll <- as.data.frame(ll)

test_that("id() works as expected", {
  expect_identical(id(ll), dfll$id)                  
})
test_that("date_discharge() works as expected", {
  expect_identical(get_epivars(ll, "date_discharge"), dfll$discharge)                  
})
test_that("case works as expected", {
  expect_identical(get_epivars(ll, "case"), dfll$epi_case_definition)                  
})
test_that("date_onset() works as expected", {
  expect_identical(date_onset(ll), dfll$date_of_onset)                  
})
test_that("gender() works as expected", {
  expect_identical(gender(ll), dfll$gender)                  
})
test_that("geo() works as expected", {
  expect_is(geo(ll), "matrix")
  expect_equal(ncol(geo(ll)), 2)
})

test_that("geo() returns the columns in the specified order", {
  expect_identical(colnames(geo(ll)), c("lon", "lat"))
  expect_false(identical(get_epivars(ll, "geo", simplify = FALSE), ll[names(ll) %in% c("lon", "lat")]))
})

test_that("get_epivars() will return only the defined epivars if provided nothing", {
  noll <- !names(dfll) %in% c("lon", "lat")
  ev <- list_epivars(ll[noll], epivars_only = TRUE)$column
  expect_identical(get_epivars(ll[noll]), dfll[ev])
})

test_that("get_epivars() allows character vectors", {
  the_sub <- c("id", "geo")
  idgeo   <- get_epivars(ll, the_sub)
  expect_named(idgeo, c("id", "lon", "lat"))
  expect_is(idgeo, "data.frame") 
})

test_that("get_epivars() throws an error if an invalid epivar is used", {
  expect_error(get_epivars(ll, "id", "date_onset", "one", "two"),
    "set_dictionary\\('one'\\, 'two'\\)"
  )
})

test_that("get_epivars() throws an error if unused (but valid) epivar is used", {
  expect_error(get_epivars(ll, "age", "date_report"),
    "The following epivars were not found in the data..age, date_report"
  )
})

test_that("get_epivars() throws an error if passed to a non-linelist object", {
  expect_error(get_epivars(dfll), "This object has no 'epivars' attribute")
})

test_that("get_epivars() throws an error if passed non-characters", {
  expect_error(get_epivars(ll, "id", vector = TRUE, 2), "all variables must be characters")
})

invisible(reset_dictionary())
