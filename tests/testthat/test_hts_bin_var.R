# Load necessary libraries and setup environment
library(travelSurveyTools)
library(data.table)
library(stringr)

# Create a sample data.table for testing
prepped_dt = test_data$trip


test_that("hts_bin_var should bin numeric variables", {
  # Call the function
  result = hts_bin_var(prepped_dt, numvar = "speed_mph", nbins = 5)

  # Check if the result is a data.table
  expect_type(result, "list")

  # Check if the binned column is present in the result
  expect_true("speed_mph" %in% names(result),
    info = "Result should have a column named 'speed_mph'"
  )

  # Check if the binned column is a factor
  expect_true(is.factor(result$speed_mph))
})
