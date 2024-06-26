# Load necessary libraries and setup environment
library(testthat)
library(data.table)



test_that("hts_prep_variable should return counts and units", {
  results = hts_prep_variable(
    summarize_var = "age",
    summarize_by = "mode_type",
    variables_dt = variable_list,
    data = test_data
  )

  expect_type(results, "list")

  expect_true("mode_type" %in% names(results$cat))

  expect_true("trip_weight" %in% names(results$cat))

  # FIXME: expect_false('995' %in% results$cat$mode_type)
})
