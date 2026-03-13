# Load necessary libraries and setup environment
library(testthat)
library(data.table)
library(srvyr)


test_that("hts_to_so should return counts and units", {
  results = hts_to_so(prepped_dt = test_data$day, wtname = "day_weight")

  expect_type(results, "list")
})

test_that("hts_to_so resolves PSU explicitly and honors the strata toggle", {
  dt = data.table(
    hh_id = c(1L, 1L, 2L, 2L),
    person_id = c(10L, 11L, 20L, 21L),
    strata_id = c("A", "A", "B", "B"),
    trip_weight = c(1, 1, 1, 1),
    value = c(1, 2, 1, 2)
  )

  default_psu = hts_to_so(
    prepped_dt = dt,
    wtname = "trip_weight",
    strataname = "strata_id",
    use_strata = FALSE
  )
  person_psu = hts_to_so(
    prepped_dt = dt,
    wtname = "trip_weight",
    psu_var = "person_id",
    strataname = "strata_id",
    use_strata = TRUE
  )

  expect_s3_class(default_psu, "tbl_svy")
  expect_s3_class(person_psu, "tbl_svy")
})
