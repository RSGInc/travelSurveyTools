
context("Test suite for hts_get_ns function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)

sample_dt = test_data$person

test_that("hts_get_ns should return counts", {
  
  results = hts_get_ns(sample_dt, weighted = FALSE)
  
  expect_is(results, "list", info = "hts_get_ns should return a list")
  
  expect_is(results$unwtd, "list", info = "unwtd should return a list")
  
  expect_null(results$wtd, info = "'wtd' component should be NULL for unweighted counts")
  
  # expect_equal()
})
