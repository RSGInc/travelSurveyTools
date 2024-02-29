# Load necessary libraries and setup environment
library(testthat)
library(data.table)
library(srvyr)


test_that("hts_to_so should return counts and units", {
  results = hts_to_so(prepped_dt = test_data$day, wtname = "day_weight")

  expect_type(results, "list")
})
