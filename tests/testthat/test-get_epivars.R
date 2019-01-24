context("get_epivars() tests")


oev <- get_dictionary()
invisible(set_dictionary("date_discharge", "case"))
ll <- as_linelist(clean_data(messy_data()),
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

test_that("get_epivars() throws an error if an invalid epivar is used", {
  expect_error(get_epivars(ll, "id", "date_onset", "one", "two"),
    "set_dictionary\\('one'\\, 'two'\\)"
  )
})
