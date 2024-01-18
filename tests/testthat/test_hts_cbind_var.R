context("Test suite for hts_cbind_var function")

# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_cbind_var should bind a column to another table", {
  # Create a sample lhs_table
  lhs_table = test_data$trip
  
  # Create a sample rhs_table
  rhs_table = test_data$person
  
  # Create a sample variable_list
  variable_list = variable_list
  
  # Call the function
  result = hts_cbind_var(lhs_table, rhs_var = 'speed_mph', variable_list = variable_list)
  
  # Check if the result is a data.table
  expect_is(result, "data.table", 
            info = "hts_cbind_var should return a data.table")
  
  # Check if the column 'speed_mph' is present in the result
  expect_true("speed_mph" %in% names(result), 
              info = "Result should have a column named 'speed_mph'")
  
  # Check if the column 'speed_mph' is bound correctly
  # expect_equal(result$speed_mph, c(NA, 30, NA), 
  #              info = "Bounded column 'speed_mph' should have correct values")
  
  # Add more test cases as needed
})

# Clean up environment
rm(list = c("create_variable_list"))
