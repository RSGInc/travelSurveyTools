
context("Test suite for hts_prep_triprate function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_prep_triprate should return counts and units", {
  
  results = hts_prep_triprate(summarize_by = 'age', variables_dt = variable_list, hts_data = test_data)
  
  expect_is(results, "list", info = "hts_prep_triprate should return a list")
  
})
