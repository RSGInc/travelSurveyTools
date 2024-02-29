# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_prep_byvar should prepare data by variable correctly", {
  results = hts_prep_byvar(
    summarize_by = "age",
    variables_dt = variable_list,
    hts_data = test_data
  )

  expect_type(results, "list")

  expect_true("age" %in% names(results))
})
