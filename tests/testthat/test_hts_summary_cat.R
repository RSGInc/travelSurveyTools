# Load necessary libraries and setup environment
library(testthat)
library(data.table)

DT = hts_prep_variable(
  summarize_var = "age",
  summarize_by = "employment",
  variables_dt = variable_list,
  missing_values = 995,
  data = test_data
)$cat



test_that("hts_summary_cat should return counts and units", {
  results = hts_summary_cat(
    prepped_dt = DT,
    summarize_var = "age",
    summarize_by = "employment",
    wtname = "person_weight"
  )

  expect_type(results, "list")

  expect_true("age" %in% names(results$wtd))

  expect_true(sum(results$unwtd$count) == sum(results$wtd$count))
  # FIXME: expect_true(!('995' %in% results$wtd$employment))
})

test_that("hts_summary_cat returns reliability statistics when se is requested", {
  results = hts_summary_cat(
    prepped_dt = DT,
    summarize_var = "age",
    summarize_by = "employment",
    wtname = "person_weight",
    se = TRUE
  )

  expect_true(all(
    c("prop_se", "prop_low", "prop_upp", "est_low", "est_upp", "rse", "deff", "ess") %in%
      names(results$wtd)
  ))
  expect_true(all(results$wtd$rse >= 0, na.rm = TRUE))
  expect_true(all(results$wtd$deff >= 0, na.rm = TRUE))
  expect_true(any(!is.na(results$wtd$ess)))
})

test_that("hts_summary_cat respects conf_level for confidence intervals", {
  results_95 = hts_summary_cat(
    prepped_dt = DT,
    summarize_var = "age",
    summarize_by = "employment",
    wtname = "person_weight",
    se = TRUE,
    conf_level = 0.95
  )

  results_90 = hts_summary_cat(
    prepped_dt = DT,
    summarize_var = "age",
    summarize_by = "employment",
    wtname = "person_weight",
    se = TRUE,
    conf_level = 0.90
  )

  width_95 = results_95$wtd$prop_upp - results_95$wtd$prop_low
  width_90 = results_90$wtd$prop_upp - results_90$wtd$prop_low

  expect_true(all(width_90 <= width_95 | is.na(width_90) | is.na(width_95)))
  expect_true(any(width_90 < width_95, na.rm = TRUE))
})
