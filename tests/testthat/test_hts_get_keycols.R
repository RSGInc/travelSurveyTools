
# Load necessary libraries and setup environment
library(testthat)
library(data.table)

sample_dt = test_data$person

test_that("hts_get_keycols should return key columns based on options", {
  
  results_all = hts_get_keycols(sample_dt, ids=TRUE, weights = TRUE)
  
  expect_type(results_all, "character")
  
  results_ids = hts_get_keycols(sample_dt, ids=TRUE, weights = FALSE)
  
  expect_type(results_ids, "character")
  
})
