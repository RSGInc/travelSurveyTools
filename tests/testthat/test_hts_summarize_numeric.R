# Load necessary libraries and setup environment
library(testthat)
library(data.table)

test_that("hts_summarize_numeric returns unweighted and weighted summaries", {
  variables_dt = data.table(
    variable = c("speed_mph", "employment"),
    entity = c("trip", "person"),
    data_type = c("numeric", "categorical")
  )

  normalized = hts_normalize_inputs(
    data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
    variables_dt = variables_dt,
    value_labels = value_labels,
    settings = hts_summary_settings()
  )

  analysis_bundle = hts_build_analysis_table(
    summarize_var = "speed_mph",
    summarize_by = "employment",
    normalized_inputs = normalized
  )

  results = hts_summarize_numeric(analysis_bundle)

  expect_type(results, "list")
  expect_true(data.table::is.data.table(results$summary_data$unwtd))
  expect_true(data.table::is.data.table(results$summary_data$wtd))
  expect_true(all(c("count", "min", "max", "mean", "median") %in% names(results$summary_data$unwtd)))
  expect_true(all(c("count", "min", "max", "mean", "median") %in% names(results$summary_data$wtd)))
  expect_equal(results$weight_var, "trip_weight")
})

test_that("hts_summarize_numeric returns weighted mean SE and CI when requested", {
  variables_dt = data.table(
    variable = c("speed_mph", "employment"),
    entity = c("trip", "person"),
    data_type = c("numeric", "categorical")
  )

  normalized = hts_normalize_inputs(
    data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
    variables_dt = variables_dt,
    value_labels = value_labels,
    settings = hts_summary_settings()
  )

  analysis_bundle = hts_build_analysis_table(
    summarize_var = "speed_mph",
    summarize_by = "employment",
    normalized_inputs = normalized
  )

  results = hts_summarize_numeric(
    analysis_bundle = analysis_bundle,
    se = TRUE
  )

  expect_true(all(c("mean_se", "mean_low", "mean_upp") %in% names(results$summary_data$wtd)))
})
