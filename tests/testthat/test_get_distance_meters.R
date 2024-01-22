

context("get_distance_meters")
library(travelSurveyTools)

test_that("Returns appropriate distance",{
  expect_true(
    get_distance_meters(
      location_1 = c(-72.3188808, 43.648975),
      location_2 = c(-72.3188808, 43.648975)) < 0.00001)
})

test_that("Returns appropriate distance",{
  expect_true(
    get_distance_meters(
      location_1 = c(-72.3277826, 43.6527773),
      location_2 = c(-72.3188808, 43.648975)) -
    832.6199 < 0.001)
})

test_that("Returns appropriate distance for multiple locations",{
  expect_true(
    sum(get_distance_meters(
      location_1 = matrix(c(-72.3277826, 43.6527773), 2, 2, byrow = TRUE),
      location_2 = matrix(c(-72.3188808, 43.648975), 2, 2, byrow = TRUE)) -
    832.6199 < 0.001) == 2)
})

test_that("Errors if bad input",{
  expect_error(
    get_distance_meters(
      location_1 = c(-72.3277826, a),
      location_2 = c(-72.3188808, 43.648975)))
})

test_that("Errors if bad input",{
  expect_error(
    get_distance_meters(
      location_1 = c(-400, 43.6527773),
      location_2 = c(-72.3188808, 43.648975)))
})

test_that("Errors if bad input",{
  expect_error(
    get_distance_meters(
      location_1 = c(-72.3277826, 43.6527773),
      location_2 = c(-72.3188808, 100)))
})
