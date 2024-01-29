
# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_prep_triprate should return counts and units", {
  
  results = hts_prep_triprate(
    summarize_by = 'age',
    variables_dt = variable_list,
    hts_data = test_data)
  
  expect_type(results, "list")
  
  expect_true(results$outliers$threshold == 0.975)
  
  expect_true('num_trips_wtd' %in% names(results$num))
  
})
