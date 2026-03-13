# Load necessary libraries and setup environment
library(testthat)
library(data.table)

test_that("hts_summarize_datetime returns date-class summary columns", {
  variables_dt = data.table(
    variable = "travel_date",
    entity = "trip",
    data_type = "date"
  )

  normalized = hts_normalize_inputs(
    data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
    variables_dt = variables_dt,
    value_labels = value_labels,
    settings = hts_summary_settings()
  )

  analysis_bundle = hts_build_analysis_table(
    summarize_var = "travel_date",
    normalized_inputs = normalized
  )

  results = hts_summarize_datetime(analysis_bundle)

  expect_true(inherits(results$summary_data$unwtd$min, "Date"))
  expect_true(inherits(results$summary_data$unwtd$max, "Date"))
  expect_true(inherits(results$summary_data$unwtd$mean, "Date"))
  expect_true(inherits(results$summary_data$unwtd$median, "Date"))
  expect_true(inherits(results$summary_data$wtd$mean, "Date"))
})
