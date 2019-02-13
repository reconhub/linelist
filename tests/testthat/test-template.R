context("template_linelist() tests")

# Setting up expectations ------------------------------------------
ll_output <- "ll <- as_linelist\\(x = 
  id              =    ,
  date_onset      =    ,
  date_report     =    ,
  date_outcome    =    ,
  case_definition =    ,
  outcome         =    ,
  gender          =    ,
  age             =    ,
  age_group       =    ,
  geo_lon         =    ,
  geo_lat         =    ,
  NULL  # don't delete me
\\)"
  
flu_output <- gsub("x = ", "x = outbreaks::fluH7N9_china_2013,", ll_output)

hosp_output <- "ll <- as_linelist\\(x = 
  date_hospital =    ,
  NULL  # don't delete me
\\)"


# Running the tests ------------------------------------------------

test_that("template linelist will print something by default", {
  expect_output(template_linelist(), ll_output)
})

test_that("template linelist will use a data set", {
  expect_output(template_linelist(outbreaks::fluH7N9_china_2013), flu_output)
})

test_that("template linelist will use the current dictionary", {
  hosp <- data.frame(
    epivar = "date_hospital",
    hxl    = "#date +start",
    description = "date at which patient was hospitalized",
    stringsAsFactors = FALSE
  )
  set_dictionary(hosp)
  expect_output(template_linelist(), hosp_output)
  reset_dictionary()
}) 

