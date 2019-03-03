context("clean_data() tests")

set.seed(2019-02-09)
DAT <- seq(as.Date("2018-01-01"), length.out = 10, by = "1 day")
POS <- as.POSIXlt(DAT, tz = "UTC")
POS2 <- POS + (86400 * sample(10, 10, replace = TRUE)) + sample(86400, 10)
md  <- messy_data(10)
md$`'ID` <- as.character(md$`'ID`)
md$`!!Date of Admission` <- DAT
md$`Time of admission`   <- POS
md$`Time of   discharge` <- POS2


# location data with mis-spellings, French, and English.
messy_locations <- c("hopsital", "h\u00f4pital", "hospital", 
                     "m\u00e9dical", "clinic", 
                     "feild", "field", "hopsital", 
                     "home", "m\u00e9dical")
md$location <- factor(messy_locations)

# add a wordlist
wordlist <- data.frame(
  from  = c("hopsital", "hopital",  "medical", "feild", "not_a_case"),
  to    = c("hospital", "hospital", "clinic",  "field", "not a case"),
  var_shortname = c(rep("location", 4), "epi_case_definition"),
  orders = c(1:4, 1),
  stringsAsFactors = FALSE
)

# define a global wordlist to check for things that change and things that don't
global_words <- data.frame(
  from = c("female", "male", "hopital"),
  to  = c("feminine", "masculine", "HOSPITAL"),
  var_shortname     = ".global",
  orders = Inf,
  stringsAsFactors = FALSE
)

expected_colnames <- c("id", "date_of_onset", "discharge", "gender",
                       "epi_case_definition", "messy_dates", "lat", "lon",
                       "date_of_admission", "time_of_admission",
                       "time_of_discharge", "location")
expected_comment <- setNames(names(md), expected_colnames)

test_that("clean_data() needs a data frame with columns and/or rows", {
  expect_error(clean_data(DAT), "DAT is not a data frame")
  expect_error(clean_data(data.frame()), "data.frame\\(\\) has no columns")
}) 

test_that("messy data will be clean", {
  cd <- clean_data(md, error_tolerance = 0.8)
  # The names are cleaned
  expect_named(cd, expected_colnames)
  # The original names are preserved as a comment
  expect_identical(comment(cd), expected_comment)
  expect_is(cd$id, "character")
  expect_is(cd$date_of_onset, "Date")
  expect_is(cd$discharge, "Date")
  expect_is(cd$gender, "factor")
  expect_is(cd$epi_case_definition, "factor")
  expect_is(cd$messy_dates, "Date")
  expect_is(cd$lat, "numeric")
  expect_is(cd$lon, "numeric")
  expect_is(cd$date_of_admission, "Date")
  expect_is(cd$time_of_admission, "Date")
  expect_is(cd$time_of_discharge, "Date")
  expect_is(cd$location, "factor")
})


test_that("columns can be protected", {
  cd  <- clean_data(md, error_tolerance = 0.8)
  cdp <- clean_data(md, protect = 6, error_tolerance = 0.8)
  expect_identical(cdp[-6], cd[-6])
  expect_identical(cdp[6], md[6])
  expect_identical(comment(cdp)[-6], expected_comment[-6])
})

test_that("numbers or logicals are required for protection", {
  expect_error(clean_data(md, protect = "messy/dates"), "protect must be a logical or integer vector")
})

test_that("Dates won't be forced", {
  cdd <- clean_data(md, force_Date = FALSE, error_tolerance = 0.8)
  expect_named(cdd, expected_colnames)
  # The original names are preserved as a comment
  expect_identical(comment(cdd), expected_comment)
  expect_is(cdd$id, "character")
  expect_is(cdd$date_of_onset, "Date")
  expect_is(cdd$discharge, "Date")
  expect_is(cdd$gender, "factor")
  expect_is(cdd$epi_case_definition, "factor")
  expect_is(cdd$messy_dates, "Date")
  expect_is(cdd$lat, "numeric")
  expect_is(cdd$lon, "numeric")
  expect_is(cdd$date_of_admission, "Date")
  expect_is(cdd$time_of_admission, "POSIXt")
  expect_is(cdd$time_of_discharge, "POSIXt")
  expect_is(cdd$location, "factor")

})

test_that("protect overrides columns specified in force_Date and guess_dates", {
  cdf <- clean_data(md, force_Date = 11, guess_dates = 6, protect = c(6, 11))
  expect_identical(md[6], cdf[6])
  expect_identical(md[11], cdf[11])
})


test_that("A wordlist can be implemented", {
  
  cdwl <- clean_data(md, wordlists = wordlist, spelling_vars = "var_shortname")
  expect_is(cdwl$location, "factor")
  expect_identical(levels(cdwl$location), c("hospital", "clinic", "field", "home"))

})

test_that("A global wordlist can be implemented alongside the wordlist", {

  wl <- rbind(wordlist, global_words, stringsAsFactors = FALSE)

  clean_global <- clean_data(md, wordlists = wl)

  expect_is(clean_global$location, "factor")

  # column-specific definitions aren't overwritten
  expect_identical(levels(clean_global$location), c("hospital", "clinic", "field", "home"))
  expect_false("HOSPITAL" %in% clean_global$location)

  # global definitions are changed
  expect_true("not a case" %in% clean_global$epi_case_definition)
  expect_false("not_a_case" %in% clean_global$epi_case_definition)
  expect_true("masculine" %in% clean_global$gender)
  expect_true("feminine" %in% clean_global$gender)

})


test_that("A global wordlist can be implemented as-is", {

  
  clean_global <- clean_data(md, wordlists = global_words)

  expect_true("HOSPITAL" %in% clean_global$location)
  expect_true("medical" %in% clean_global$location)
  expect_true("hopsital" %in% clean_global$location)

})

test_that("A global wordlist with a '.default' value would throw an error", {

  gw <- rbind(global_words, c(".default", "NOOOOO", ".global", "Inf"))
  expect_error(clean_data(md, wordlists = gw), "the .default keyword cannot be used with .global")

  wl <- rbind(wordlist, gw, stringsAsFactors = FALSE)
  expect_error(clean_data(md, wordlists = gw), "the .default keyword cannot be used with .global")
  
})


test_that("clean_variables and clean_data will return the same thing if no dates", {

  cdcd <- clean_data(md, wordlists = wordlist, spelling_vars = "var_shortname", guess_dates = FALSE, force_Date = FALSE)
  cdcv <- clean_variables(clean_variable_names(md), wordlists = wordlist, spelling_vars = "var_shortname")
  expect_identical(cdcd, cdcv)
})

