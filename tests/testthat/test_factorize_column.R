library(travelSurveyTools)
library(data.table)

x <- sample(5, replace = TRUE, size = 10)
x <- c(x, NA)
var_str <- "test_column"

vals_df <- data.table(
  variable = "test_column",
  value = 1:7,
  value_label = letters[1:7],
  stringsAsFactors = FALSE
)


test_that("Returns a factor", {
  expect_equal(
    class(factorize_column(x, var_str, vals_df)),
    c("ordered", "factor")
  )
  expect_equal(
    class(factorize_column(x, var_str, vals_df, add_na = FALSE)),
    c("ordered", "factor")
  )
})

test_that("Includes expected value levels", {
  expect_equal(
    levels(factorize_column(x, var_str, vals_df)),
    c(vals_df$value_label, NA)
  )
})
