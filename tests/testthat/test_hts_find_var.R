
# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_find_var should find the location of a variable", {
  
  result = hts_find_var('gender', variable_list)
  
  expect_type(result, 'character')
  
})
