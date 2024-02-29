# Load necessary libraries and setup environment
library(testthat)
library(data.table)

data("test_data")

test_that("hts_validate_variable_list should return an error", {
  data("variable_list")

  variable_list[, shared_name := NA]

  expect_error(hts_validate_variable_list(variable_list, test_data))
})

test_that("hts_validate_variable_list should return a warning", {
  data("variable_list")

  variable_list[variable == "age", person := 0]

  expect_warning(hts_validate_variable_list(variable_list, test_data))
})
