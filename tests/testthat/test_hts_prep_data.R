
context("Test suite for hts_prep_data function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)



test_that("hts_prep_data should return counts and units", {
  
  results = hts_prep_data(summarize_var = 'age', variables_dt = variable_list, data = test_data)
  
  expect_is(results, "list", info = "hts_prep_data should return a list")

})

