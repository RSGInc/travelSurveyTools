# Load necessary libraries and setup environment
library(testthat)
library(data.table)

test_that("hts_validate_variables normalizes the new entity-based contract", {
  variables_dt = data.table(
    variable = c("employment", "travel_mode"),
    entity = c("person", "trip"),
    data_type = c("categorical", "categorical")
  )

  results = hts_validate_variables(variables_dt, hts_summary_settings())

  expect_s3_class(results, "data.table")
  expect_true(all(c(
    "variable_label", "question_text", "variable_description", "variable_logic",
    "is_checkbox", "shared_name", "universe", "topic", "notes"
  ) %in% names(results)))
  expect_false(any(results$is_checkbox))
  expect_equal(results$shared_name, results$variable)
})

test_that("hts_validate_variables rejects unknown entities", {
  variables_dt = data.table(
    variable = "bar_mode",
    entity = "bar",
    data_type = "categorical"
  )

  expect_error(
    hts_validate_variables(variables_dt, hts_summary_settings()),
    "Unknown entity name"
  )
})

test_that("hts_validate_variables supports custom entity maps", {
  settings = hts_summary_settings(
    entity_map = list(
      foo = list(table = "foos", id = "foo_id"),
      bar = list(table = "bars", id = "bar_id", parent = "foo", join_keys = c("foo_id"))
    )
  )

  variables_dt = data.table(
    variable = c("foo_type", "bar_mode"),
    entity = c("foo", "bar"),
    data_type = c("categorical", "categorical"),
    shared_name = c("foo_type", "bar_mode")
  )

  results = hts_validate_variables(variables_dt, settings)

  expect_equal(results$entity, c("foo", "bar"))
})

test_that("hts_validate_variables warns on singleton checkbox groups", {
  variables_dt = data.table(
    variable = c("race_1", "employment"),
    entity = c("person", "person"),
    data_type = c("integer/categorical", "categorical"),
    is_checkbox = c(TRUE, FALSE),
    shared_name = c("race", "employment")
  )

  expect_warning(
    hts_validate_variables(variables_dt, hts_summary_settings()),
    "contain only one variable"
  )
})

test_that("hts_validate_variables errors on duplicate variables", {
  variables_dt = data.table(
    variable = c("employment", "employment"),
    entity = c("person", "person"),
    data_type = c("categorical", "categorical")
  )

  expect_error(
    hts_validate_variables(variables_dt, hts_summary_settings()),
    "Duplicate variables"
  )
})
