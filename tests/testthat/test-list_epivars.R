context("list_epivars tests")

ll <- as_linelist(clean_data(messy_data()),
                  id = "id", 
                  date_onset = "date_of_onset",
                  case_definition = "epi_case_definition",
                  gender = "gender",
                  geo = c("lon", "lat"))

test_that("list_epivars() returns a data frame", {
  expect_is(list_epivars(ll), "data.frame")
  expect_is(list_epivars(ll, full_dict = TRUE), "data.frame")  
  expect_named(list_epivars(ll), c("epivar", "column", "hxl", "description"))
})

test_that("list_epivars() returns a data frame with columns in the right order", {
  evcol <- list_epivars(ll)$column
  evcol <- evcol[!evcol %in% c("lon", "lat")]
  expect_identical(as.character(evcol), names(ll[names(ll) %in% evcol]))
})
