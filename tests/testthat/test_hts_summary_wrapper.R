# Load necessary libraries and setup environment
library(testthat)
library(data.table)

test_that("hts_summary_wrapper returns the expected wrapper structure for categorical summaries", {
  expected_n_valid = nrow(hts_prep_variable(
    summarize_var = "employment",
    summarize_by = "age"
  )$cat)

  results = hts_summary_wrapper(
    summarize_var = 'employment',
    summarize_by = 'age')
  
  expect_type(results, "list")
  expect_equal(names(results), c("meta", "diagnostics", "summaries"))
  expect_true(all(c("target", "group_by", "source_tables", "design") %in% names(results$meta)))
  expect_equal(results$meta$target$variable, "employment")
  expect_equal(results$meta$target$shared_name, "employment")
  expect_equal(results$meta$group_by$variables, "age")
  expect_equal(results$meta$source_tables, "person")
  expect_equal(results$meta$design$weight_var, "person_weight")
  expect_true(all(
    c(
      "unit_counts", "n_total", "n_valid", "n_missing", "pct_missing",
      "n_distinct", "all_missing"
    ) %in% names(results$diagnostics)
  ))
  expect_true(all(
    c("summary_data", "weight_var", "unit_counts") %in%
      names(results$summaries$categorical)
  ))
  expect_equal(results$diagnostics$unit_counts, results$summaries$categorical$unit_counts)
  expect_equal(results$diagnostics$n_total, nrow(person))
  expect_equal(results$diagnostics$n_valid, expected_n_valid)
  expect_equal(
    results$diagnostics$n_missing,
    results$diagnostics$n_total - results$diagnostics$n_valid
  )
  expect_equal(
    results$diagnostics$pct_missing,
    results$diagnostics$n_missing / results$diagnostics$n_total
  )
  expect_true(results$diagnostics$n_distinct > 0)
  expect_false(results$diagnostics$all_missing)
  expect_true("categorical" %in% names(results$summaries))
  
  expect_true(results$summaries$categorical$weight_var == "person_weight")
  expect_true(
    sum(results$summaries$categorical$summary_data$wtd$est) ==
      sum(results$diagnostics$unit_counts$wtd)
  )
})

test_that("hts_summary_wrapper includes numeric summaries when available", {
  expected_n_valid = nrow(suppressWarnings(hts_prep_variable(
    summarize_var = "speed_mph"
  )$cat))

  expect_warning(
    results <- hts_summary_wrapper(
      summarize_var = "speed_mph"
    ),
    "outliers were removed"
  )

  expect_type(results, "list")
  expect_equal(results$meta$target$variable, "speed_mph")
  expect_equal(results$meta$source_tables, "trip")
  expect_true("categorical" %in% names(results$summaries))
  expect_true("numeric" %in% names(results$summaries))
  expect_false(is.null(results$summaries$numeric))
  expect_equal(results$meta$design$weight_var, "trip_weight")
  expect_true(all(
    c("summary_data", "weight_var", "unit_counts") %in%
      names(results$summaries$numeric)
  ))
  expect_equal(results$diagnostics$unit_counts, results$summaries$categorical$unit_counts)
  expect_equal(results$diagnostics$n_total, nrow(trip))
  expect_equal(results$diagnostics$n_valid, expected_n_valid)
  expect_equal(
    results$diagnostics$n_missing,
    results$diagnostics$n_total - results$diagnostics$n_valid
  )
  expect_equal(
    results$diagnostics$pct_missing,
    results$diagnostics$n_missing / results$diagnostics$n_total
  )
  expect_true(results$diagnostics$n_distinct > 0)
  expect_false(results$diagnostics$all_missing)
  expect_equal(results$summaries$numeric$weight_var, "trip_weight")
})

test_that("hts_summary_wrapper carries optional variable metadata into meta target", {
  data("variable_list")

  variable_list[, question_text := NA_character_]
  variable_list[, logic := NA_character_]
  variable_list[, universe := NA_character_]
  variable_list[, topic := NA_character_]
  variable_list[, notes := NA_character_]
  variable_list[variable == "employment", label := "Employment status"]
  variable_list[variable == "employment", question_text := "What is your employment status?"]
  variable_list[variable == "employment", logic := "Asked of all persons age 16+"]
  variable_list[variable == "employment", universe := "Persons age 16 and older"]
  variable_list[variable == "employment", topic := "Demographics"]
  variable_list[variable == "employment", notes := "Collapsed to major employment categories"]

  results = hts_summary_wrapper(
    summarize_var = "employment",
    variables_dt = variable_list
  )

  expect_equal(results$meta$target$variable_label, "Employment status")
  expect_equal(results$meta$target$question_text, "What is your employment status?")
  expect_equal(results$meta$target$variable_logic, "Asked of all persons age 16+")
  expect_equal(results$meta$target$variable_universe, "Persons age 16 and older")
  expect_equal(results$meta$target$variable_topic, "Demographics")
  expect_equal(results$meta$target$variable_notes, "Collapsed to major employment categories")
})

test_that("hts_summary_wrapper includes estimate-level reliability stats for weighted categorical summaries", {
  results = hts_summary_wrapper(
    summarize_var = "employment",
    summarize_by = "age",
    se = TRUE
  )

  expect_true(all(
    c("prop_se", "prop_low", "prop_upp", "est_low", "est_upp", "rse", "deff", "ess") %in%
      names(results$summaries$categorical$summary_data$wtd)
  ))
  expect_true(all(results$summaries$categorical$summary_data$wtd$rse >= 0, na.rm = TRUE))
  expect_true(all(results$summaries$categorical$summary_data$wtd$deff >= 0, na.rm = TRUE))
  expect_true(any(!is.na(results$summaries$categorical$summary_data$wtd$ess)))
})

test_that("hts_summary_wrapper passes conf_level through to weighted categorical summaries", {
  results_95 = hts_summary_wrapper(
    summarize_var = "employment",
    summarize_by = "age",
    se = TRUE,
    conf_level = 0.95
  )

  results_90 = hts_summary_wrapper(
    summarize_var = "employment",
    summarize_by = "age",
    se = TRUE,
    conf_level = 0.90
  )

  width_95 =
    results_95$summaries$categorical$summary_data$wtd$prop_upp -
    results_95$summaries$categorical$summary_data$wtd$prop_low
  width_90 =
    results_90$summaries$categorical$summary_data$wtd$prop_upp -
    results_90$summaries$categorical$summary_data$wtd$prop_low

  expect_true(all(width_90 <= width_95 | is.na(width_90) | is.na(width_95)))
  expect_true(any(width_90 < width_95, na.rm = TRUE))
})

test_that("hts_summary_wrapper handles checkbox summaries with unit-level diagnostics", {
  checkbox_vars = variable_list[shared_name == "race", variable]
  expected_n_valid = person[
    ,
    sum(Reduce(`&`, lapply(.SD, function(x) !is.na(x)))),
    .SDcols = checkbox_vars
  ]
  expected_n_selected = person[
    ,
    sum(rowSums(.SD == 1, na.rm = TRUE) > 0),
    .SDcols = checkbox_vars
  ]
  expected_n_responses = person[
    ,
    sum(as.matrix(.SD == 1), na.rm = TRUE),
    .SDcols = checkbox_vars
  ]

  results = hts_summary_wrapper(
    summarize_var = "race",
    summarize_by = "age"
  )

  expect_true(results$meta$target$is_checkbox)
  expect_equal(results$meta$target$variable, "race")
  expect_equal(results$meta$target$shared_name, "race")
  expect_equal(results$meta$target$variable_description, "Race")
  expect_equal(results$meta$source_tables, "person")
  expect_equal(results$diagnostics$n_total, nrow(person))
  expect_equal(results$diagnostics$n_valid, expected_n_valid)
  expect_equal(results$diagnostics$n_selected, expected_n_selected)
  expect_equal(results$diagnostics$n_responses, expected_n_responses)
  expect_equal(
    results$diagnostics$n_missing,
    results$diagnostics$n_total - results$diagnostics$n_valid
  )
  expect_true("categorical" %in% names(results$summaries))
  expect_null(results$summaries$numeric)
  expect_true(all(
    c(
      "African American or Black",
      "American Indian or Alaska Native",
      "Asian",
      "Native Hawaiian or other Pacific Islander",
      "White",
      "Other race",
      "Prefer not to answer"
    ) %in% unique(as.character(results$summaries$categorical$summary_data$unwtd$race))
  ))
})
