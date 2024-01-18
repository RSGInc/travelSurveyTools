
context("Test suite for hts_prep_byvar function")

library(testthat)
library(data.table)


test_that("hts_prep_byvar should prepare data by variable correctly", {
  
  results = hts_prep_byvar(summarize_by = "age", variables_dt = variable_list, hts_data = test_data)
  
  expect_is(results, "data.table", 
            info = "hts_prep_byvar should return a data.table")
  
   # expect_equal()
})
