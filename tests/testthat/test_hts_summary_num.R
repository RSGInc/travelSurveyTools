
context("Test suite for hts_summary_num function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)


DT = hts_prep_data(summarize_var = 'speed_mph',
                   variables_dt = variable_list,
                   data = list('hh' = hh,
                               'person' = person,
                               'day' = day,
                               'trip' = trip,
                               'vehicle' = vehicle))$num


test_that("hts_summary_num should return counts and units", {
  
  results =  hts_summary_num(prepped_dt = DT,
                             summarize_var = 'speed_mph',
                             wtname = 'trip_weight')
  
  expect_is(results, "list", info = "hts_summary_num should return a list")
  
})
