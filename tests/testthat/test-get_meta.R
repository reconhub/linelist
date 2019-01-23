## TODO: this should be replaced by tests on list_epivars()

## context("metadata tests")

## oev <- epivars()
## invisible(epivars("date_discharge", "case", set = TRUE))
## ll <- as_linelist(clean_data(messy_data()),
##                   id = "id", 
##                   date_onset = "date_of_onset",
##                   date_discharge = "discharge",
##                   case = "epi_case_definition",
##                   gender = "gender",
##                   geo = c("lon", "lat"))

## test_that("get_meta() returns a data frame", {
##   expect_is(get_meta(ll), "data.frame")
##   expect_is(get_meta(ll, dictionary = FALSE), "data.frame")  
## })

## test_that("get_meta() returns vars in the same order", {
##   expect_identical(get_meta(ll)$columns, get_meta(ll, dictionary = FALSE)$columns)
## })


## epivars(reset = TRUE)
