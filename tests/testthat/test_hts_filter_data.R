
context("Test suite for hts_filter_data function")


library(testthat)
library(data.table)


  
  test_that("hts_filter_data should filter data based on id_type", {
    # Create a sample hts_data
    hts_data = test_data
    
    # Call the function to filter based on hh_id
    result_hh = hts_filter_data(hts_data, hh[num_people > 5, hh_id], id_type = 'hh')
    
    # Check if the result is a list
    expect_is(result_hh, "list", 
              info = "hts_filter_data should return a list")
    
    # Check if hh data is filtered correctly
    # expect_equal(result_hh$hh$hh_id, c(1, 2), 
    #              info = "hh data should be filtered correctly")
    
    # Add more test cases for other id_type values
  })

