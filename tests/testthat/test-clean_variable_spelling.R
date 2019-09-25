context("clean_variable_spelling() tests")


corrections <- data.frame(
  bad = c("foubar", "foobr", "fubar", ".missing", "unknown", "Yes", "Y", "No", "N", ".missing"),
  good = c("foobar", "foobar", "foobar", "missing", "missing", "yes", "yes", "no", "no", "missing"),
  column = c(rep("raboof", 5), rep("treatment", 5)),
  orders = c(1:5, 5:1),
  stringsAsFactors = FALSE
)

clist <- split(corrections, corrections$column)

my_data_frame <- data.frame(
   raboof    = c(letters[1:5], "foubar", "foobr", "fubar", "", "unknown", "fumar"),
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

test_that("default errors will be thrown", {

  corr <- data.frame(bad = c(".default", ".default"),
                     good = c("check data", "check data"),
                     column = c("raboof", "treatment"),
                     orders = Inf,
                     stringsAsFactors = FALSE
  )
  corr <- rbind(corrections, corr)
  wrn <- "raboof_____:.+?treatment__:.+?'check data'"
  expect_warning(clean_variable_spelling(my_data_frame, corr, warn = TRUE), wrn)

})


test_that("errors will be captured and passed through; error'd cols are preserved", {

  with_list <- my_data_frame
  with_list$listcol <- as.list(with_list$region)
  corr <- corrections
  corr[12, ] <- c("Florida", "Flo Rida", "listcol", 1)
  err <- "listcol____:.+?x must be coerceable to a character"
  expect_warning(lc <- clean_variable_spelling(with_list, corr, warn = TRUE), err)
  expect_length(lc, 4)
  expect_is(lc[[4]], "list")
  expect_named(lc, names(with_list))
  expect_identical(with_list[[4]], lc[[4]])
  expect_warning(lc <- clean_data(with_list, wordlists = corr, warn_spelling = TRUE), err)


})


test_that("sorting works as expected", {

  # sorting by data.frame 
  test_sorted_df <- clean_variable_spelling(my_data_frame, 
                                             corrections,
                                             spelling_vars = "column",
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

test_that("global data frame works if spelling_vars = NULL", { 

  expect_error({
    global_test <- clean_variable_spelling(my_data_frame, corrections, spelling_vars = 69)
  }, "spelling_vars must be the name or position of a column in the wordlist")

  expect_warning({
    global_test <- clean_variable_spelling(my_data_frame, corrections, spelling_vars = NULL)
  }, "Using wordlist globally across all character/factor columns.")

  resorted_trt <- forcats::fct_relevel(cleaned_data$treatment, "missing") 
  expect_identical(global_test$raboof, cleaned_data$raboof) 
  expect_identical(global_test$treatment, resorted_trt)

})


test_that("regex matching works as expected", { 
  
  dict <- data.frame(val = c("a", "b", "c"),
                     replace = c("alpha", "bravo", "charlie"),
                     var = c("column", "column", "column"),
                     stringsAsFactors = FALSE)
  
  df <- data.frame(column = sample(c("a", "b", "c"), 10, replace = TRUE),
                   column_2 = sample(c("a", "b", "c"), 10, replace = TRUE),
                   stringsAsFactors = FALSE)
  
  # clean without regex (only var 'column' matched and cleaned)
  x1 <- clean_variable_spelling(df, dict)
  expect_true(all(x1$column %in% dict$replace))
  expect_identical(x1$column_2, df$column_2)
  
  # clean with regex (vars 'column' and 'column_2' matched and cleaned)
  x2 <- clean_variable_spelling(df, dict, regex_vars = TRUE)
  expect_true(all(x2$column %in% dict$replace))
  expect_true(all(x2$column_2 %in% dict$replace))
})
