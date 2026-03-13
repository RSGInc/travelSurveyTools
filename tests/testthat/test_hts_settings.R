# Load necessary libraries and setup environment
library(testthat)
library(data.table)

test_that("hts_summary_settings returns the default entity map", {
  settings = hts_summary_settings()

  expect_type(settings, "list")
  expect_true("entity_map" %in% names(settings))
  expect_true(all(c("household", "person", "day", "trip", "vehicle") %in% names(settings$entity_map)))
  expect_equal(settings$entity_map$household$table, "hh")
  expect_equal(settings$entity_map$household$id, "hh_id")
  expect_equal(settings$entity_map$household$strata, "sample_segment")
})

test_that("hts_validate_settings accepts a custom entity map against data", {
  custom_data = list(
    households = data.table(
      hhid = 1:3,
      hh_weight = c(1, 2, 3),
      sample_segment = c("Zone 1", "Zone 2", "Zone 3")
    ),
    persons = data.table(
      pid = 1:4,
      hhid = c(1, 1, 2, 3),
      person_weight = c(1, 1, 2, 3)
    ),
    trip_unlinked = data.table(
      tripid = 1:5,
      hhid = c(1, 1, 1, 2, 3),
      pid = c(1, 1, 2, 3, 4),
      trip_weight = c(1, 1, 1, 1, 1)
    )
  )

  settings = hts_summary_settings(
    entity_map = list(
      household = list(
        table = "households",
        id = "hhid",
        weight = "hh_weight",
        psu = "hhid",
        strata = "sample_segment"
      ),
      person = list(
        table = "persons",
        id = "pid",
        parent = "household",
        join_keys = c("hhid"),
        weight = "person_weight"
      ),
      trip_unlinked = list(
        table = "trip_unlinked",
        id = "tripid",
        parent = "person",
        join_keys = c("hhid", "pid"),
        weight = "trip_weight"
      )
    )
  )

  validated = hts_validate_settings(settings, data = custom_data)

  expect_equal(validated$entity_map$household$table, "households")
  expect_equal(validated$entity_map$person$parent, "household")
  expect_equal(validated$entity_map$trip_unlinked$join_keys, c("hhid", "pid"))
})

test_that("hts_validate_settings errors on invalid entity map definitions", {
  settings_missing_id = list(
    entity_map = list(
      household = list(table = "hh")
    )
  )

  settings_unknown_parent = list(
    entity_map = list(
      household = list(table = "hh", id = "hh_id"),
      person = list(table = "person", id = "person_id", parent = "missing_entity", join_keys = c("hh_id"))
    )
  )

  settings_missing_join_keys = list(
    entity_map = list(
      household = list(table = "hh", id = "hh_id"),
      person = list(table = "person", id = "person_id", parent = "household")
    )
  )

  expect_error(hts_validate_settings(settings_missing_id), "missing required field")
  expect_error(hts_validate_settings(settings_unknown_parent), "unknown parent entity")
  expect_error(hts_validate_settings(settings_missing_join_keys), "must declare `join_keys`")
})
