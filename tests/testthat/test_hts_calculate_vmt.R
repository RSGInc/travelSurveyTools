
context("Test suite for hts_calculate_vmt function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)



test_that("hts_calculate_vmt should a data table", {
  
  results = hts_calculate_vmt(
      trip_name = 'trip',
      data = test_data,
      agg_tbl = 'day',
      mode_cols = c('mode_1', 'mode_2'),
      miles_col = 'distance_miles',
      vehicle_modes = 6:10
     )
  
  expect_is(results, "data.table", info = "hts_prep_data should return a data table")
  
})
