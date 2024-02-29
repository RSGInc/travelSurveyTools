# Load necessary libraries and setup environment
library(testthat)
library(data.table)


# Create a sample lhs_table
lhs_table = test_data$trip

test_that("hts_cbind_var should bind a column to another table", {
  # Create a sample hts_data
  data("test_data")

  # Call the function
  result = hts_cbind_var(lhs_table, rhs_var = "speed_mph", hts_data = test_data, variable_list = variable_list)

  # Check if the result is a list
  expect_type(result, "list")

  # Check if the column 'speed_mph' is present in the result
  expect_true("speed_mph" %in% names(result),
    info = "Result should have a column named 'speed_mph'"
  )
})
