# Load necessary libraries and setup environment
library(testthat)
library(data.table)

test_that("hts_summary_wrapper returns the expected wrapper structure for categorical summaries", {
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
  expect_true("categorical" %in% names(results$summaries))
  
  expect_true(results$summaries$categorical$summary$weight_name == "person_weight")
  expect_true(
    sum(results$summaries$categorical$summary$wtd$est) ==
      sum(results$diagnostics$n_ls$wtd)
  )
})

test_that("hts_summary_wrapper includes numeric summaries when available", {
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
})

test_that("hts_summary_wrapper carries optional variable metadata into meta target", {
  data("variable_list")

  variable_list[, question_text := NA_character_]
  variable_list[, logic := NA_character_]
  variable_list[variable == "employment", label := "Employment status"]
  variable_list[variable == "employment", question_text := "What is your employment status?"]
  variable_list[variable == "employment", logic := "Asked of all persons age 16+"]

  results = hts_summary_wrapper(
    summarize_var = "employment",
    variables_dt = variable_list
  )

  expect_equal(results$meta$target$variable_label, "Employment status")
  expect_equal(results$meta$target$question_text, "What is your employment status?")
  expect_equal(results$meta$target$variable_logic, "Asked of all persons age 16+")
})
