# Load necessary libraries and setup environment
library(testthat)
library(data.table)

test_that("hts_summarize_categorical returns unweighted and weighted summaries", {
  variables_dt = data.table(
    variable = c("employment", "age"),
    entity = c("person", "person"),
    data_type = c("categorical", "categorical")
  )

  normalized = hts_normalize_inputs(
    data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
    variables_dt = variables_dt,
    value_labels = value_labels,
    settings = hts_summary_settings()
  )

  analysis_bundle = hts_build_analysis_table(
    summarize_var = "employment",
    summarize_by = "age",
    normalized_inputs = normalized
  )

  results = hts_summarize_categorical(analysis_bundle)

  expect_type(results, "list")
  expect_true(all(c("summary_data", "weight_var", "psu_var", "strata_var") %in% names(results)))
  expect_true(data.table::is.data.table(results$summary_data$unwtd))
  expect_true(data.table::is.data.table(results$summary_data$wtd))
  expect_equal(results$weight_var, "person_weight")
  expect_true(all(c("employment", "age", "count", "prop") %in% names(results$summary_data$unwtd)))
  expect_true(all(c("employment", "age", "count", "prop", "est") %in% names(results$summary_data$wtd)))
})

test_that("hts_summarize_categorical returns reliability stats when se is requested", {
  variables_dt = data.table(
    variable = c("employment", "age"),
    entity = c("person", "person"),
    data_type = c("categorical", "categorical")
  )

  normalized = hts_normalize_inputs(
    data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
    variables_dt = variables_dt,
    value_labels = value_labels,
    settings = hts_summary_settings()
  )

  analysis_bundle = hts_build_analysis_table(
    summarize_var = "employment",
    summarize_by = "age",
    normalized_inputs = normalized
  )

  results = hts_summarize_categorical(
    analysis_bundle = analysis_bundle,
    se = TRUE
  )

  expect_true(all(
    c("prop_se", "prop_low", "prop_upp", "est_low", "est_upp", "rse", "deff", "ess") %in%
      names(results$summary_data$wtd)
  ))
})

test_that("hts_summarize_categorical works on ancestor-entity analysis tables", {
  variables_dt = data.table(
    variable = c("mode_type", "employment"),
    entity = c("trip", "person"),
    data_type = c("categorical", "categorical")
  )

  normalized = hts_normalize_inputs(
    data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
    variables_dt = variables_dt,
    value_labels = value_labels,
    settings = hts_summary_settings()
  )

  analysis_bundle = hts_build_analysis_table(
    summarize_var = "mode_type",
    summarize_by = "employment",
    normalized_inputs = normalized
  )

  results = hts_summarize_categorical(analysis_bundle)

  expect_true(all(c("mode_type", "employment") %in% names(results$summary_data$unwtd)))
  expect_equal(results$weight_var, "trip_weight")
})
