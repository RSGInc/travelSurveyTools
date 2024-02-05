
library(travelSurveyTools)
library(data.table)


dt = data.table(x = c(sample(5, replace=TRUE, size=10),NA),
                y = sample(3, replace = TRUE, size = 11))

vals_df =   data.table(
  variable = "x",
  value = 1:7,
  value_label = letters[1:7],
  stringsAsFactors = FALSE)


test_that("Returns the same structure as the input",{

    suppressMessages(expect_equal(class(factorize_df(dt, vals_df, add_na = TRUE)), class(dt)))
    suppressMessages(expect_equal(ncol(factorize_df(dt, vals_df, add_na = TRUE)), ncol(dt)))
    suppressMessages(expect_equal(nrow(factorize_df(dt, vals_df, add_na = TRUE)), nrow(dt)))

  })

test_that("Works when add_na is false",{
  suppressMessages(expect_equal(class(factorize_df(dt, vals_df, add_na = F)),class(dt)))
})

