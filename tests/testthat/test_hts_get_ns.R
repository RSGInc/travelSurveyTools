
# Load necessary libraries and setup environment
library(testthat)
library(data.table)

sample_dt = test_data$person

test_that("hts_get_ns should return counts", {
  
  results = hts_get_ns(sample_dt, weighted = FALSE)
  
  expect_type(results, "list")
  
  expect_type(results$unwtd, "list")
  
  expect_null(results$wtd, info = "'wtd' component should be NULL for unweighted counts")
  
  expect_type(results$units, "character")
  
  expect_type(results$unwtd$Households, "integer")
  
})
