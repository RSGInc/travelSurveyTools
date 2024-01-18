
context("Test suite for hts_get_keycols function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)

sample_dt = test_data$person


test_that("hts_get_keycols should return key columns based on options", {
  
  results_all = hts_get_keycols(sample_dt, ids=TRUE, weights = TRUE)
  
  expect_is(results_all, "character", info = "hts_get_keycols should return a character vector")
  
  results_ids = hts_get_keycols(sample_dt, ids=TRUE, weights = FALSE)
  
  expect_is(results_ids, "character",
            info = "Only id columns should be included in the result when weights = FALSE")
  
  # need to add two more test to check weight only cols and priority option

  # expect_equal()
})
