# Load necessary libraries and setup environment
library(testthat)
library(data.table)

data("test_data")

test_that("hts_validate_variable_list should return an error", {
  data("variable_list")

  variable_list[, shared_name := NA]

  expect_error(hts_validate_variable_list(variable_list, test_data))
})

test_that("hts_validate_variable_list should return a warning", {
  data("variable_list")

  variable_list[variable == "age", person := 0]

  expect_warning(hts_validate_variable_list(variable_list, test_data))
})

test_that("hts_validate_variable_list preserves extra metadata columns", {
  data("variable_list")

  variable_list[, logic := NA_character_]
  variable_list[variable == "age", logic := "Asked of all persons"]

  results = hts_validate_variable_list(variable_list, test_data)

  expect_true("logic" %in% names(results))
  expect_equal(results[variable == "age", logic], "Asked of all persons")
})

test_that("hts_validate_variable_list adds optional wrapper metadata columns when absent", {
  data("variable_list")

  drop_cols = intersect(c("label", "question_text", "logic"), names(variable_list))
  if (length(drop_cols) > 0) {
    variable_list[, (drop_cols) := NULL]
  }

  results = hts_validate_variable_list(variable_list, test_data)

  expect_true(all(c("label", "question_text", "logic") %in% names(results)))
  expect_true(all(is.na(results$label)))
  expect_true(all(is.na(results$question_text)))
  expect_true(all(is.na(results$logic)))
})
