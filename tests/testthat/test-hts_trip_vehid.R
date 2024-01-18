
context("Test suite for hts_trip_vehid function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)

# Sample trip_table and vehicle_table for testing
trip_ex = data.table(
  hh_id = sample(1:10, size = 30, replace = TRUE),
  trip_id = 1:30,
  mode_type = sample(1:2, size = 30, replace = TRUE),
  mode_1 = sample(1, size = 30, replace = TRUE))

vehicle_ex = data.table(
  hh_id = sample(1:10, size = 30, replace = TRUE),
  vehicle_id = 1:30)

values_ex = data.table(
  variable = c(rep('mode_type', 2), ('mode_1')),
  value = c(1,2,1),
  value_label = c('Vehicle', 'Walk', 'Car')
)


test_that("hts_trip_vehid should create vehicle_id correctly", {
  
  results = hts_trip_vehid(trip_table = trip_ex,
                           vehicle_table = vehicle_ex,
                           vehicle_mode_type = 'Vehicle',
                           values_dt = values_ex)
  
  expect_is(results, "data.table", info = "hts_trip_vehid should return a data.table object")
  
  expect_true("vehicle_id" %in% names(results), info = "Results should include 'vehicle_id' column")
  
})
