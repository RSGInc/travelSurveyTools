# Load necessary libraries and setup environment
library(testthat)
library(data.table)
library(srvyr)


DT = hts_prep_data(
  summarize_var = "speed_mph",
  variables_dt = variable_list,
  data = test_data
)$num


test_that("hts_summary_num should return counts and units", {
  results = hts_summary_num(
    prepped_dt = DT,
    summarize_var = "speed_mph",
    wtname = "trip_weight"
  )

  expect_type(results, "list")

  expect_true("max" %in% names(results$wtd))

  expect_true(results$unwtd$max == results$wtd$max)
})
