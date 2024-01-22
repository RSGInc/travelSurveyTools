
context("join_spatial")
library(travelSurveyTools)
library(sf)

test_dt =
  data.table(
    id = 1:3,
    lon = c(-82.33, -79.17, -76.17),
    lat = c(35.50, 36.27, 36.49))

test_geog =
  st_read(
    system.file('shape/nc.shp', package = 'sf'), quiet = TRUE)[, c('FIPS', 'NAME', 'geometry')]

test_geog$FIPS = as.character(test_geog$FIPS)
test_geog$NAME = as.character(test_geog$NAME)

test_result =
  cbind(
    test_dt,
    data.table(
      FIPS = c('37021', '37033', '37053'),
      NAME = c('Buncombe', 'Caswell', 'Currituck')))

setkey(test_result, 'id')

testthat::test_that("Returns spatial id columns properly",{
  testthat::expect_equal(
    join_spatial(
      test_dt,
      test_geog,
      id_col = 'id',
      lon_col = 'lon',
      lat_col = 'lat'),
    test_result
  )
})

# FIXME: Make a test for largest = TRUE
# Requires a test dataset that has a row that joins to multiple rows in
# test_geog
