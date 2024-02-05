
# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_calculate_vmt should return a data table", {
  
  results = hts_calculate_vmt(
      trip_name = 'trip',
      data = test_data,
      agg_tbl = 'day',
      mode_cols = c('mode_1', 'mode_2'),
      miles_col = 'distance_miles',
      vehicle_modes = 6:10
     )
  
  expect_type(results, "list")
  
  expect_true('vmt' %in% names(results), info = "vmt is in returned table")
  
})
