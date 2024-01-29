
# Load necessary libraries and setup environment
library(testthat)
library(data.table)

DT = hts_prep_data(summarize_var = 'age',
                   summarize_by = 'employment',
                   variables_dt = variable_list,
                   missing_values = 995,
                   data = test_data)$cat



test_that("hts_summary_cat should return counts and units", {
  
  results = hts_summary_cat(prepped_dt = DT,
                            summarize_var = 'age',
                            summarize_by = 'employment',
                            wtname = 'person_weight')
  
  expect_type(results, "list")
  
  expect_true('age' %in% names(results$wtd))
  
  # FIXME: expect_true(!('995' %in% results$wtd$employment))
  
})
