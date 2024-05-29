# Load necessary libraries and setup environment
library(testthat)
library(data.table)




test_that("hts_summary_wrapper should return counts and units", {
  results = hts_summary_wrapper(
    summarize_var = 'employment',
    summarize_by = 'age')
  
  expect_type(results, "list")
  
  expect_true(results$cat$summary$weight_name == "person_weight")
  
  expect_true(sum(results$cat$summary$wtd$est) == sum(results$cat$n_ls$wtd))
})
