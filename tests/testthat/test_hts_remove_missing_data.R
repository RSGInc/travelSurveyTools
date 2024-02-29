# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_remove_missing_data should return counts and units", {
  results = hts_remove_missing_data(
    hts_data = test_data,
    summarize_var = "speed_mph",
    variables_dt = variable_list
  )

  expect_type(results, "list")

  expect_true(!("995" %in% results$trip$speed_mph))
})
