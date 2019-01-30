context("lookup tests")

# Setup data ------------------------------------------------------------------
#
dat <- clean_data(messy_data(5))
ll  <- as_linelist(dat,
                   id = "id", 
                   gender = "gender",
                   case_definition = "epi_case_definition",
                   date_onset = "date_of_onset", 
                   geo = c("lon", "lat")
                  )
tmp <- ll
class(tmp) <- class(tmp)[-1]

# Start tests -----------------------------------------------------------------

test_that("Lookup a column name and return a character works", {
  expect_identical(lookup(ll, "gender", symbol = FALSE), "gender")
  expect_identical(lookup(ll, "case_definition", symbol = FALSE), "epi_case_definition")
  expect_identical(lookup(ll, "date_onset", symbol = FALSE), "date_of_onset")
  expect_identical(lookup(ll, "geo", symbol = FALSE), c("lon", "lat"))
})

test_that("a non-epivars-containing object will return an informative error", {
  expect_error(lookup(dat), "This object does not contain an epivars attribute.")
})

test_that("Lookup will return a symbol or list of symbols by default", {
  expect_identical(lookup(ll, "date_onset"), as.symbol("date_of_onset"))
  expect_identical(lookup(ll, "geo"), list(as.symbol("lon"), as.symbol("lat")))
})

test_that("A null lookup will return the entire list", {
  expect_identical(lookup(ll), attr(ll, "epivars"))
})

test_that("lookup still works with plain data frames", {
  expect_is(tmp, "data.frame")
  expect_false(inherits(tmp, "linelist"))
  expect_identical(lookup(tmp, "case_definition"), as.symbol("epi_case_definition"))
})

test_that("dplyr works with lookup inline", {
  skip_if_not_installed("dplyr")
  require("dplyr", quietly = TRUE, warn.conflicts = FALSE)

  res <- ll %>% 
    group_by(!!lookup(., "gender")) %>%  
    count(!!lookup(., "case_definition"))

  expect_is(res, "tbl_df")
  expect_identical(lookup(res), lookup(ll))
  expect_named(res, c("gender", "epi_case_definition", "n"))
})

test_that("dplyr works with lookup references", {
  skip_if_not_installed("dplyr")
  require("dplyr", quietly = TRUE, warn.conflicts = FALSE)
  CASEDEF <- lookup(ll, "case_definition") 
  GEO <- lookup(ll, "geo", symbol = FALSE)
  res <- ll %>%
    group_by(!!CASEDEF) %>%
    summarise_at(GEO, mean) # note, summarise_at uses characters
  expect_is(res, "tbl_df")
  expect_named(res, c("epi_case_definition", "lon", "lat"))
  expect_identical(lookup(res), lookup(ll))
  expect_is(as_linelist(res), "linelist")
})
