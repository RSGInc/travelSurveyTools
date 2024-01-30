
# Load necessary libraries and setup environment
library(testthat)
library(data.table)

DT = hts_prep_data(summarize_var = 'age',
                   variables_dt = variable_list,
                   data = test_data)$cat


test_that("hts_summary should return counts and units", {
  
  results = hts_summary(
    prepped_dt = DT,
    summarize_var = 'age',
    summarize_vartype = 'categorical',
    wtname = 'person_weight')
  
  expect_type(results, "list")
  
  expect_true(results$summary$weight_name == 'person_weight')
  
  expect_true(sum(results$summary$wtd$est) == sum(results$n_ls$wtd))
})
