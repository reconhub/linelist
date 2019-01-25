context("get_epivars() tests")


oev <- get_dictionary()
## invisible(set_dictionary("date_discharge", "case"))
dfll <- clean_data(messy_data())
if (requireNamespace("tibble")) {
  dfll <- tibble::as_tibble(dfll)
}
ll <- as_linelist(dfll,
                  case_definition = "epi_case_definition",
                  id = "id", 
                  date_onset = "date_of_onset",
                  gender = "gender",
                  geo = c("lon", "lat"))

test_that("id() works as expected", {
  expect_identical(id(ll), dfll$id)                  
})
test_that("case works as expected", {
  expect_identical(get_epivars(ll, "case_definition"), dfll$epi_case_definition)                  
})
test_that("date_onset() works as expected", {
  expect_identical(date_onset(ll), dfll$date_of_onset)                  
})
test_that("gender() works as expected", {
  expect_identical(gender(ll), dfll$gender)                  
})
test_that("geo() works as expected", {
  expect_is(geo(ll), "data.frame")
  expect_length(geo(ll), 2)
})

test_that("geo() returns the columns in the specified order", {
  expect_named(geo(ll), c("lon", "lat"))
  expect_false(identical(get_epivars(ll, "geo", simplify = FALSE), ll[names(ll) %in% c("lon", "lat")]))
})

test_that("get_epivars() will return only the defined epivars if provided nothing", {
  noll <- !names(dfll) %in% c("lon", "lat")
  ev <- list_epivars(ll[noll])$column
  llev <- get_epivars(ll[noll])
  class(llev) <- class(dfll)
  expect_identical(llev, dfll[names(dfll) %in% ev])
  expect_identical(dfll[ev], dfll[names(dfll) %in% ev])
})

test_that("get_epivars() allows character vectors", {
  the_sub <- c("id", "geo")
  idgeo   <- get_epivars(ll, the_sub)
  expect_named(idgeo, c("id", "lon", "lat"))
  expect_is(idgeo, "data.frame") 
})

test_that("get_epivars() will return data in the order specified", {
  vars <- c("gender", "id", "gender")
  expect_identical(as.data.frame(ll[vars]), get_epivars(ll, vars))
  expect_length(get_epivars(ll, vars), 3)
})

## test_that("get_epivars() throws an error if an invalid epivar is used", {
##   expect_error(get_epivars(ll, "id", "date_onset", "one", "two"),
##     "set_dictionary\\('one'\\, 'two'\\)"
##   )
## })

## test_that("get_epivars() throws an error if unused (but valid) epivar is used", {
##   expect_error(get_epivars(ll, "age", "date_report"),
##     "The following epivars were not found in the data..age, date_report"
##   )
## })

## test_that("get_epivars() throws an error if passed to a non-linelist object", {
##   expect_error(get_epivars(dfll), "This object has no 'epivars' attribute")
## })

## test_that("get_epivars() throws an error if passed non-characters", {
##   expect_error(get_epivars(ll, "id", vector = TRUE, 2), "all variables must be characters")
## })

invisible(reset_dictionary())
