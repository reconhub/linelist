context("clean spelling tests") 


corrections <- data.frame(
  bad = c("foubar", "foobr", "fubar", NA, "unknown"),
  good = c("foobar", "foobar", "foobar", "missing", "missing"),
  stringsAsFactors = FALSE
)

my_data <- c(letters[1:5], "foubar", "foobr", "fubar", NA, "unknown", "fumar")
cleaned_data <- c(letters[1:5], "foobar", "foobar", "foobar", "missing", "missing", "fumar")

test_that("clean_spelling throws an error with no data or a data frame", {

  expect_error(clean_spelling(), "x must be coerceable to a character")
  expect_error(clean_spelling(corrections), "x must be coerceable to a character")

})


test_that("clean_spelling throws an error with no wordlist", {

  bad_df   <- data.frame(a = 1:10)
  bad_df$b <- as.list(1:10) # list column
  expect_error(clean_spelling(my_data), "wordlist must be a data frame")
  expect_error(clean_spelling(my_data, my_data), "wordlist must be a data frame")
  expect_error(clean_spelling(my_data, bad_df[1]), "wordlist must be a data frame with at least two columns")
  expect_error(clean_spelling(my_data, bad_df), "wordlist must have two columns coerceable to a character")

})

test_that("logical values can be coerced", {

  res <- clean_spelling(1:5 > 2, data.frame(a = c(FALSE, TRUE), b = c("hell", "yeah")))
  expected <- rep(c("hell", "yeah"), c(2, 3))
  expect_identical(res, expected)

})

test_that("clean_spelling will clean everything defined in the wordlist", {

  expect_identical(clean_spelling(my_data, corrections), cleaned_data) 

})


test_that("clean_spelling will work with a matrix", {

  expect_identical(clean_spelling(my_data, as.matrix(corrections)), cleaned_data) 

})

test_that("clean_spelling will be silent if the data are already cleaned", {

  expect_failure(expect_warning(clean_spelling(cleaned_data, corrections)))

})


test_that("clean_spelling will throw a warning for completely unknown data", {

  expect_warning(clean_spelling(letters[1:4], corrections), 
                 "None of the variables in letters\\[1:4\\] were found in corrections. Did you use the correct wordlist?")

})

test_that("clean_spelling maintains order of original data", {

  abc_dict <- data.frame(c(letters, "zz"), c(LETTERS, "Z"), stringsAsFactors = FALSE)
  lfac <- sample(c("zz", sample(letters, 19, replace = TRUE)))
  # Letters are reversed
  upps <- clean_spelling(factor(lfac, c("zz", rev(letters))), abc_dict)
  expect_true(all(upps %in% LETTERS))
  # The results respects the wordlist
  expect_identical(levels(upps), LETTERS)
  expect_false("zz" %in% upps)
  
  # untranslated factors are placed at the end
  upps2 <- clean_spelling(factor(lfac, c("hey", "there", "zz", rev(letters))), abc_dict)
  expect_true(all(upps2 %in% LETTERS))
  expect_identical(levels(upps2), c(LETTERS, "hey", "there"))
  expect_false("zz" %in% upps2)

})
