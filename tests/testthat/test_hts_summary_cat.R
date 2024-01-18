
context("Test suite for hts_summary_cat function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_summary_cat should return counts and units", {
  
  results = hts_summary_cat(
    prepped_dt = test_data$person, 
    summarize_var = 'age', 
    variable_list = variable_list)
  
  expect_is(results, "list", info = "hts_summary_cat should return a list")
  
})
