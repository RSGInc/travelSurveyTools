context("Test suite for hts_bin_var function")

library(travelSurveyTools)
library(data.table)
library(stringr)



test_that("hts_bin_var should bin numeric variables", {
  # Create a sample data.table for testing
  prepped_dt = test_data$trip
  
  # Call the function
  result = hts_bin_var(prepped_dt, numvar = "speed_mph", nbins = 5)
  
  # Check if the result is a data.table
  expect_is(result, "data.table", 
            info = "hts_bin_var should return a data.table")
  
  # Check if the binned column is present in the result
  expect_true("speed_mph" %in% names(result), 
              info = "Result should have a column named 'speed_mph'")
  
  # Check if the binned column is a factor
  expect_is(result$speed_mph, "factor", 
            info = "Binned column should be of type 'factor'")
  
  # Add more test cases as needed
})
