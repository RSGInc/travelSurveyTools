
context("Test suite for hts_summary_num function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_summary_num should return counts and units", {
  
  results = hts_summary_num(
    prepped_dt = test_data$trip, 
    summarize_var = 'speed_mph')
  
  expect_is(results, "list", info = "hts_summary_num should return a list")
  
})
