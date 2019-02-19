context("clean_variable_spelling() tests")


corrections <- data.frame(
  bad = c("foubar", "foobr", "fubar", NA, "unknown", "Yes", "Y", "No", "N", NA),
  good = c("foobar", "foobar", "foobar", "missing", "missing", "yes", "yes", "no", "no", "missing"),
  column = c(rep("raboof", 5), rep("treatment", 5)),
  orders = c(1:5, 5:1),
  stringsAsFactors = FALSE
)

clist <- split(corrections, corrections$column)

my_data_frame <- data.frame(
   raboof    = c(letters[1:5], "foubar", "foobr", "fubar", NA, "unknown", "fumar"),
   treatment = c(letters[5:1], "Y", "Yes", "N", NA, "No", "yes"),
   region    = state.name[1:11]
)

cleaned_data <- data.frame(
   raboof    = factor(c(letters[1:5], "foobar", "foobar", "foobar", "missing", "missing", "fumar"),
                      levels = c("foobar", "missing", letters[1:5], "fumar")),
   treatment = factor(c(letters[5:1], "yes", "yes", "no", "missing", "no", "yes"),
                      levels = c("yes", "no", "missing", letters[1:5])),
   region    = state.name[1:11]
)


test_that("a data frame is needed for the first part", {

  expect_error(clean_variable_spelling(), "x must be a data frame")
  expect_error(clean_variable_spelling(clist), "x must be a data frame")
  expect_error(clean_variable_spelling(my_data_frame$raboof), "x must be a data frame")

})


test_that("a list of data frames is needed for the second part", {

  expect_error(clean_variable_spelling(my_data_frame),
               "wordlists must be a list of data frames")
  expect_error(clean_variable_spelling(my_data_frame, list(1:10)),
               "everything in wordlists must be a data frame")
  expect_error(clean_variable_spelling(my_data_frame, c(clist, list(corrections))),
               "all dictionaries must be named")
  expect_error(clean_variable_spelling(my_data_frame, c(clist, funkytime = list(corrections))),
               "all dictionaries must match a column in the data")
  
})


test_that("spelling cleaning works as expected", {

  test_cleaned <- clean_variable_spelling(my_data_frame, clist)
  expect_identical(test_cleaned$raboof, cleaned_data$raboof)
  # Uncomment if this is mis-behaving. <3, Zhian
  # print(fct_recode(my_data_frame$treatment, yes = "Yes", yes = "Y", no = "No", no = "N"))
  # print(test_cleaned$treatment)
  # print(cleaned_data$treatment)
  expect_identical(test_cleaned$treatment, cleaned_data$treatment)
  expect_identical(test_cleaned$region, cleaned_data$region)

})


test_that("sorting works as expected", {

  # sorting by data.frame 
  test_sorted_df <- clean_variable_spelling(my_data_frame, 
                                             corrections,
                                             group = "column",
                                             sort_by = "orders"
  )

  # sorting by list
  test_sorted_ls <- clean_variable_spelling(my_data_frame, 
                                             clist,
                                             sort_by = "orders"
  )
  resorted_trt <- forcats::fct_relevel(cleaned_data$treatment, c("missing", "no", "yes"))
  expect_identical(test_sorted_df, test_sorted_ls)
  expect_identical(test_sorted_df$raboof, cleaned_data$raboof)
  expect_identical(test_sorted_df$treatment, resorted_trt)

})

test_that("global data frame works if group = NULL", { 

  expect_error({
    global_test <- clean_variable_spelling(my_data_frame, corrections, group = 69)
  }, "group must be the name or position of a column in the wordlist")

  expect_warning({
    global_test <- clean_variable_spelling(my_data_frame, corrections, group = NULL)
  }, "Using wordlist globally across all character/factor columns.")

  resorted_trt <- forcats::fct_relevel(cleaned_data$treatment, "missing") 
  expect_identical(global_test$raboof, cleaned_data$raboof) 
  expect_identical(global_test$treatment, resorted_trt)

})
