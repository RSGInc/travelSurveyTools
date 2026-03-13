# Load necessary libraries and setup environment
library(testthat)
library(data.table)

test_that("hts_build_analysis_table expands checkbox targets", {
  variables_dt = data.table(
    variable = c("race_1", "race_2", "age"),
    entity = c("person", "person", "person"),
    data_type = c("integer/categorical", "integer/categorical", "categorical"),
    variable_description = c("Race: Race 1", "Race: Race 2", "Age"),
    is_checkbox = c(TRUE, TRUE, FALSE),
    shared_name = c("race", "race", "age")
  )

  normalized = hts_normalize_inputs(
    data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
    variables_dt = variables_dt,
    value_labels = value_labels,
    settings = hts_summary_settings()
  )

  results = hts_build_analysis_table(
    summarize_var = "race",
    summarize_by = "age",
    normalized_inputs = normalized
  )

  expect_true(results$meta$target$is_checkbox)
  expect_equal(results$meta$target$shared_name, "race")
  expect_true(all(c("race", "checkbox_value", "age", "person_id") %in% names(results$analysis_table)))
  expect_equal(results$meta$checkbox$checkbox_vars, c("race_1", "race_2"))
})

test_that("hts_summarize_checkbox returns unweighted and weighted summaries", {
  variables_dt = data.table(
    variable = c("race_1", "race_2", "age"),
    entity = c("person", "person", "person"),
    data_type = c("integer/categorical", "integer/categorical", "categorical"),
    variable_description = c("Race: Race 1", "Race: Race 2", "Age"),
    is_checkbox = c(TRUE, TRUE, FALSE),
    shared_name = c("race", "race", "age")
  )

  normalized = hts_normalize_inputs(
    data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
    variables_dt = variables_dt,
    value_labels = value_labels,
    settings = hts_summary_settings()
  )

  analysis_bundle = hts_build_analysis_table(
    summarize_var = "race",
    summarize_by = "age",
    normalized_inputs = normalized
  )

  results = hts_summarize_checkbox(analysis_bundle)

  expect_type(results, "list")
  expect_true(data.table::is.data.table(results$summary_data$unwtd))
  expect_true(data.table::is.data.table(results$summary_data$wtd))
  expect_true(all(c("race", "age", "count", "prop") %in% names(results$summary_data$unwtd)))
  expect_true(all(c("race", "age", "count", "prop", "est") %in% names(results$summary_data$wtd)))
})

test_that("hts_summarize_checkbox returns reliability stats when se is requested", {
  variables_dt = data.table(
    variable = c("race_1", "race_2", "age"),
    entity = c("person", "person", "person"),
    data_type = c("integer/categorical", "integer/categorical", "categorical"),
    variable_description = c("Race: Race 1", "Race: Race 2", "Age"),
    is_checkbox = c(TRUE, TRUE, FALSE),
    shared_name = c("race", "race", "age")
  )

  normalized = hts_normalize_inputs(
    data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
    variables_dt = variables_dt,
    value_labels = value_labels,
    settings = hts_summary_settings()
  )

  analysis_bundle = hts_build_analysis_table(
    summarize_var = "race",
    summarize_by = "age",
    normalized_inputs = normalized
  )

  results = hts_summarize_checkbox(
    analysis_bundle = analysis_bundle,
    se = TRUE
  )

  expect_true(all(
    c("prop_se", "prop_low", "prop_upp", "est_low", "est_upp", "rse", "deff", "ess") %in%
      names(results$summary_data$wtd)
  ))
})
