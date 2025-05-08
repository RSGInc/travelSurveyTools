# Load necessary libraries and setup environment
library(testthat)
library(data.table)


test_that("hts_prep_vmtrate should return counts and units", {
  results = hts_prep_vmtrate(
    summarize_by = "age",
    traveler_count_var="num_travelers",
    dist_var="distance_miles",
    mode_var="mode_type",
    veh_regex="^(5|6|8)$",
    variables_dt = travelSurveyTools::variable_list,
    remove_outliers = TRUE,
    hts_data = travelSurveyTools::test_data
  )
  
  expect_type(results, "list")
  
  expect_true(results$outliers$threshold == 0.975)
  
  expect_true("vmt_wtd" %in% names(results$num))
})
