
# Load necessary libraries and setup environment
library(testthat)
library(data.table)

# Create a sample hts_data
hts_data = test_data

test_that("hts_filter_data should filter data based on id_type", {

  # Call the function to filter based on hh_id
  result_hh = hts_filter_data(hts_data, hh[num_people > 5, hh_id], id_name = 'hh_id')
  
  # Check if the result is a list
  expect_type(result_hh, "list")
  
  # Check if hh data is filtered correctly
  expect_true('hh_id' %in% names(result_hh$hh),
               info = "hh data should be filtered correctly")
  
  # Check that no hhs with 5 or less people remain
  expect_true(min(result_hh$hh$num_people) > 5)
  
})

