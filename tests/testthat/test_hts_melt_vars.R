# Load necessary libraries and setup environment
library(testthat)
library(data.table)

sample_dt <- test_data$person

test_that("hts_melt_vars should melt variables correctly", {
  results <- hts_melt_vars(shared_name = "race", wide_dt = sample_dt)

  # Check if the result is a list
  expect_type(results, "list")

  expect_true("race" %in% names(results))
})
