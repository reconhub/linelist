context("dictionary tests")

reset_dictionary()
hosp <- data.frame(
  epivar = "date_hospital",
  hxl    = "#date +start",
  description = "date at which patient was hospitalized",
  stringsAsFactors = FALSE
)

gdf <- data.frame(
  epivar = c("geo_lon", "geo_lat"),
  hxl    = c("#geo +lon", "#geo +lat"),
  description = c("longitude in degrees", "latitude in degrees"),
  stringsAsFactors = FALSE
)

# The tests ---------------------------------------------------------

test_that("epivars will return the default epivars", {
  expect_identical(get_dictionary(), getOption("linelist_dictionary"))
  expect_identical(get_dictionary(), default_dictionary())
})


test_that("a new epivar can be added", {
  allhosp <- rbind(default_dictionary(), hosp)
  add_epivar(hosp)
  expect_identical(get_dictionary(), allhosp)
  expect_identical(get_dictionary(), getOption("linelist_dictionary"))
  expect_error(add_epivar(hosp), 
               "The following epivars already exist in the dictionary:  date_hospital") 
})

# Errors and such ---------------------------------------------------

test_that("check_dictionary() will thwart attempts to import incorrect epivars", {
  expect_error(set_dictionary(pi), "is not a data frame but a numeric")
  expect_error(set_dictionary("non/existant/file.txt"), "The file 'non/existant/file.txt' does not appear to exist")
  expect_error(set_dictionary(iris), "x does not have 3 columns but 5")
  expect_error(set_dictionary(iris[1:3]), "dictionary does not have the expected column names")
})

test_that("attempts to add existing epivars will be thwarted", {
  # works with character input
  expect_error(add_epivar("geo", "#geo +lon +lat", "whoops"),
               "geo already exists in the dictionary")
  # and data frame input
  gdf <- data.frame(epivar = "geo",
                    hxl = "#geo +lon +lat", 
                    description = "whoops", 
                    stringsAsFactors = FALSE)
  expect_error(add_epivar(rbind(gdf, hosp)),
               "The following epivars already exist in the dictionary:  geo, date_hospital")
})

test_that("a description can be updated", {
  add_description("date_hospital", "what")
  dict <- get_dictionary()
  expect_identical(dict$description[dict$epivar == "date_hospital"], "what")
})

test_that("there can be no description for a non-existant epivar", {
  expect_error(add_description("what", "the hell"),
               "add_definition\\(epivar = \\\"what\\\", description = \\\"the hell\\\", hxl = \\\"\\\"\\)")
})

test_that("set_dictionary() can take file input", {
  set_dictionary(system.file("example_dict.xlsx", package = "linelist"))
  expect_identical(get_dictionary(), rbind(default_dictionary(), gdf))
})

test_that("set_epivars() will allow different column names", {
  names(gdf) <- c("this", "is", "it")
  set_dictionary(gdf, epivar = "this", hxl = "is", description = "it")
  names(gdf) <- names(default_dictionary())
  expect_identical(get_dictionary(), gdf)
})

test_that("epivars can be reset", {
  reset_dictionary()
  expect_identical(default_dictionary(), get_dictionary())
})
