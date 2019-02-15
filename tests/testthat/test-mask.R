context("masking tests")


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
llm <- mask(ll)
                  
test_that("mask and unmask return the same data", {

  expect_identical(unmask(mask(ll)), ll)

})

test_that("unmask can work on a data frame that has been stripped of its class", {

  llmg <- dplyr::group_by(llm, case_definition)
  # dplyr drops the linelist class, but keeps the attributes
  expect_failure(expect_is(llmg, "linelist"))
  expect_is(unmask(llmg), "linelist")
  
})

test_that("mask will not re-mask a masked linelist", {
  
  expect_identical(llm, mask(llm))

})

test_that("unmask will trim epivars", {
  
  case_area      <- dplyr::select(ll, epi_case_definition, lat, lon)
  mask_case_area <- dplyr::select(llm, case_definition, geo_lat, geo_lon)
  expect_failure(expect_identical(case_area, unmask(mask_case_area)))

})

test_that("adding new epivars to a masked linelist will add to the mask", {

  llmd <- set_epivars(llm, date_report = "messy_dates")
  expect_failure(expect_true("messy_dates" %in% names(llmd)))
  expect_true("date_report" %in% names(llmd))
  expect_true("messy_dates" %in% names(ll))
  expect_true("messy_dates" %in% names(unmask(llmd)))

})


test_that("updating epivars to a masked linelist will update the columns and mask", {

  
  expect_false("date_of_onset" %in% names(llm))
  llmd <- set_epivars(llm, date_onset = "messy_dates")
  expect_false("messy_dates" %in% names(llmd))
  expect_true("date_of_onset" %in% names(llmd))
  expect_true("date_onset" %in% names(llmd))
  expect_true("messy_dates" %in% names(ll))
  expect_true("messy_dates" %in% names(unmask(llmd)))

})

test_that("removing epivars to a masked linelist will remove that mask", {

  llmd <- set_epivars(llm, case_definition = NULL)
  expect_failure(expect_true("case_definition" %in% names(llmd)))
  expect_true("epi_case_definition" %in% names(llmd))
  expect_true("epi_case_definition" %in% names(unmask(llmd)))
  
})

test_that("mask will not mask a data set that has no epivars attribute", {

  expect_error(mask(iris), "There is no mask method defined for an object of class data.frame")
  expect_warning(miris <- unmask(iris), "Only masked linelist objects can be unmasked")
  expect_identical(miris, iris) 
  
})
