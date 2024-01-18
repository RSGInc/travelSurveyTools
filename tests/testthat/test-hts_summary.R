
context("Test suite for hts_summary function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_summary should return counts and units", {
  
  results = hts_summary(
    test_data,
    summarize_var = 'race',
    variables_dt = variable_list)
  
  expect_is(results, "list", info = "hts_summary should return a list")
  
})
