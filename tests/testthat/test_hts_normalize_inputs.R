# Load necessary libraries and setup environment
library(testthat)
library(data.table)

test_that("hts_normalize_inputs returns a normalized bundle", {
  variables_dt = data.table(
    variable = c("employment", "travel_date"),
    entity = c("person", "trip"),
    data_type = c("categorical", "date")
  )

  results = hts_normalize_inputs(
    data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
    variables_dt = variables_dt,
    value_labels = value_labels,
    settings = hts_summary_settings()
  )

  expect_type(results, "list")
  expect_equal(names(results), c("data", "variables", "value_labels", "settings"))
  expect_true(data.table::is.data.table(results$data$person))
  expect_true(data.table::is.data.table(results$variables))
  expect_true(data.table::is.data.table(results$value_labels))
  expect_true("entity_map" %in% names(results$settings))
})

test_that("hts_normalize_inputs checks variable existence against mapped entities", {
  settings = hts_summary_settings(
    entity_map = list(
      foo = list(table = "foos", id = "foo_id")
    )
  )

  data = list(
    foos = data.table(foo_id = 1:3, foo_value = c("a", "b", "c"))
  )

  variables_dt = data.table(
    variable = c("foo_value", "missing_col"),
    entity = c("foo", "foo"),
    data_type = c("categorical", "categorical")
  )

  expect_error(
    hts_normalize_inputs(
      data = data,
      variables_dt = variables_dt,
      settings = settings
    ),
    "Variable\\(s\\) not found"
  )
})

test_that("hts_normalize_inputs adds val_order to value labels when missing", {
  variables_dt = data.table(
    variable = "employment",
    entity = "person",
    data_type = "categorical"
  )

  labels = data.table(
    variable = c("employment", "employment"),
    value = c(1, 2),
    label = c("Employed", "Unemployed")
  )

  results = hts_normalize_inputs(
    data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
    variables_dt = variables_dt,
    value_labels = labels,
    settings = hts_summary_settings()
  )

  expect_true("val_order" %in% names(results$value_labels))
  expect_equal(results$value_labels$val_order, c(1L, 2L))
})
