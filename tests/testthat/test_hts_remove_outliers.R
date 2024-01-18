
context("Test suite for hts_remove_outliers function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_remove_outliers should return counts and units", {
  
  results = hts_remove_outliers(var_dt = test_data$trip, numvar = 'speed_mph')
  
  expect_is(results, "list", info = "hts_remove_outliers should return a list")
  
})
