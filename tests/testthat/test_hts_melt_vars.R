
context("Test suite for hts_melt_vars function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)


variable_list = variable_list

sample_dt = test_data$person

test_that("hts_melt_vars should melt variables correctly", {
  
  results = hts_melt_vars(shared_name = 'race', wide_dt = sample_dt)
  
  # Check if the result is a data.table
  expect_is(results, "data.table", 
            info = "hts_melt_vars should return a data.table")
  
  
  # expect_equal(2 * 2, 4)
})
