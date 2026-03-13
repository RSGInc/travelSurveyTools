# Load necessary libraries and setup environment
library(testthat)
library(data.table)

test_that("hts_summary_wrapper returns the expected wrapper structure for categorical summaries", {
  results = hts_summary_wrapper(
    summarize_var = 'employment',
    summarize_by = 'age')
  
  expect_type(results, "list")
  expect_equal(results$variable, "employment")
  expect_equal(results$summarize_by, "age")
  expect_equal(results$table, "person")
  expect_true(all(c("meta", "diagnostics", "summaries") %in% names(results)))
  expect_equal(results$meta$weight_var, "person_weight")
  expect_equal(results$meta$shared_name, "employment")
  expect_true("categorical" %in% names(results$summaries))
  
  expect_true(results$summaries$categorical$summary$weight_name == "person_weight")
  expect_true(
    sum(results$summaries$categorical$summary$wtd$est) ==
      sum(results$diagnostics$n_ls$wtd)
  )
})

test_that("hts_summary_wrapper includes numeric summaries when available", {
  expect_warning(
    results <- hts_summary_wrapper(
      summarize_var = "speed_mph"
    ),
    "outliers were removed"
  )

  expect_type(results, "list")
  expect_equal(results$variable, "speed_mph")
  expect_equal(results$table, "trip")
  expect_true("categorical" %in% names(results$summaries))
  expect_true("numeric" %in% names(results$summaries))
  expect_false(is.null(results$summaries$numeric))
  expect_equal(results$meta$weight_var, "trip_weight")
})
