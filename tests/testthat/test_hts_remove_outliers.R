# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_remove_outliers should return counts and units", {
  results <- hts_remove_outliers(var_dt = test_data$trip, numvar = "speed_mph")

  expect_type(results, "list")

  expect_true("speed_mph" %in% names(results$dt))

  expect_true(results$outlier_description$threshold == 0.975)
})
