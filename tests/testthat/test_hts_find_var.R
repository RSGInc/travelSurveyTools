
context("Test suite for hts_find_var function")


library(testthat)
library(data.table)



test_that("hts_find_var should find the location of a variable", {
  
  variable_list = variable_list
  
  result = hts_find_var('gender', variable_list)
  
  expect_is(result, 'character', info = "hts_find_var should return a character vector")
  
  # expect_equal()
})
