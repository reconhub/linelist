context("clean spelling tests") 


corrections <- data.frame(
  bad = c("foubar", "foobr", "fubar", NA, "unknown"),
  good = c("foobar", "foobar", "foobar", "missing", "missing"),
  stringsAsFactors = FALSE
)

my_data <- c(letters[1:5], "foubar", "foobr", "fubar", NA, "unknown", "fumar")
cleaned_data <- c(letters[1:5], "foobar", "foobar", "foobar", "missing", "missing", "fumar")

test_that("clean_spelling throws an error with no data", {

  expect_error(clean_spelling(), "x must be a character or factor")
  expect_error(clean_spelling(1:10), "x must be a character or factor")
  expect_error(clean_spelling(TRUE), "x must be a character or factor")
  expect_error(clean_spelling(corrections), "x must be a character or factor")

})


test_that("clean_spelling throws an error with no dictionary", {

  expect_error(clean_spelling(my_data), "dictionary must be a data frame")
  expect_error(clean_spelling(my_data, my_data), "dictionary must be a data frame")

})


test_that("clean_spelling will clean everything defined in the dictionary", {

  expect_identical(clean_spelling(my_data, corrections), cleaned_data) 

})


test_that("clean_spelling will be silent if the data are already cleaned", {

  expect_failure(expect_warning(clean_spelling(cleaned_data, corrections)))

})


test_that("clean_spelling will throw a warning for completely unknown data", {

  expect_warning(clean_spelling(letters[1:4], corrections), 
                 "None of the variables in letters\\[1:4\\] were found in corrections. Did you use the correct dictionary?")

})

test_that("clean_spelling maintains order of original data", {

  abc_dict <- data.frame(c(letters, "zz"), c(LETTERS, "Z"), stringsAsFactors = FALSE)
  lfac <- sample(c("zz", sample(letters, 19, replace = TRUE)))
  upps <- clean_spelling(factor(lfac, c("zz", rev(letters))), abc_dict)
  expect_true(all(upps %in% LETTERS))
  expect_identical(levels(upps), rev(LETTERS))
  expect_false("zz" %in% upps)
  
})
