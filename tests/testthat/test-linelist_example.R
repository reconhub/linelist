test_that("linelist_example will show the example files in inst/extdata", {
  expect_identical(linelist_example(), 
                   c(
                     "coded-data.csv", 
                     "spelling-dictionary.csv"
                    )
                   )
})

test_that("linelsit_example will fetch example files", {
  expect_true(file.exists(linelist_example("coded-data.csv")))
})
