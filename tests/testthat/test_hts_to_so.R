
context("Test suite for hts_to_so function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)
library(srvyr)


test_that("hts_to_so should return counts and units", {
  
  results = hts_to_so(test_data$day)
  
  expect_is(results, "survey.design", info = "hts_to_so should return a list")
  
})
