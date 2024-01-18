
context("Test suite for hts_summary function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)

DT = hts_prep_data(summarize_var = 'age',
                   variables_dt = variable_list,
                   data = list('hh' = hh,
                               'person' = person,
                               'day' = day,
                               'trip' = trip,
                               'vehicle' = vehicle))$cat


test_that("hts_summary should return counts and units", {
  
  results = hts_summary(
    prepped_dt = DT,
    summarize_var = 'age',
    summarize_vartype = 'categorical',
    wtname = 'person_weight')
  
  expect_is(results, "list", info = "hts_summary should return a list")
  
})
