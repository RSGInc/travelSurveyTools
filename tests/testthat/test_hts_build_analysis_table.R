# Load necessary libraries and setup environment
library(testthat)
library(data.table)

test_that("hts_build_analysis_table handles same-entity summaries", {
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

  results = hts_build_analysis_table(
    summarize_var = "employment",
    summarize_by = "age",
    normalized_inputs = normalized
  )

  expect_true(data.table::is.data.table(results$analysis_table))
  expect_true(all(c("employment", "age", "person_id", "person_weight") %in% names(results$analysis_table)))
  expect_equal(results$meta$target$entity, "person")
  expect_equal(results$meta$design$weight_var, "person_weight")
})

test_that("hts_build_analysis_table joins ancestor entities for grouping variables", {
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

  results = hts_build_analysis_table(
    summarize_var = "mode_type",
    summarize_by = "employment",
    normalized_inputs = normalized
  )

  expect_true(all(c("mode_type", "employment", "trip_id", "trip_weight") %in% names(results$analysis_table)))
  expect_equal(results$meta$target$entity, "trip")
  expect_equal(results$meta$group_by$entities$entity, "person")
  expect_true("person" %in% names(results$meta$joins))
})

test_that("hts_build_analysis_table errors on unsupported grouping directions", {
  variables_dt = data.table(
    variable = c("employment", "mode_type"),
    entity = c("person", "trip"),
    data_type = c("categorical", "categorical")
  )

  normalized = hts_normalize_inputs(
    data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
    variables_dt = variables_dt,
    value_labels = value_labels,
    settings = hts_summary_settings()
  )

  expect_error(
    hts_build_analysis_table(
      summarize_var = "employment",
      summarize_by = "mode_type",
      normalized_inputs = normalized
    ),
    "not the same as or an ancestor"
  )
})
