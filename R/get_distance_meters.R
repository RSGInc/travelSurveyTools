#' Get distance between points
#'
#' Function to get haversine distance in meters between two points
#' Based on calculation from the geosphere package
#'
#' @param location_1 A matrix or vector of longitudes and latitudes
#' @param location_2 A matrix or vector of longitudes and latitudes
#' @param radius Radius of the sphere to use for haversine calculation (Defaults to meters)
#' @return A vector of distances in meters
#' @export get_distance_meters
get_distance_meters =
  function(
    location_1,
    location_2,
    radius = 6378137) {

  # convert to matrix if not already a matrix
  location_1 = matrix(location_1, ncol = 2)
  location_2 = matrix(location_2, ncol = 2)

  # do some checks on inputs
  # longitudes should be numeric and between -180 & 180
  stopifnot(is.numeric(location_1[, 1]))
  stopifnot(is.numeric(location_2[, 1]))
  stopifnot(location_1[!is.na(location_1[, 1]), 1] %between% c(-180, 180))
  stopifnot(location_2[!is.na(location_2[, 1]), 1] %between% c(-180, 180))

  # latitudes should be numeric and between -90 & 90
  stopifnot(is.numeric(location_1[, 2]))
  stopifnot(is.numeric(location_2[, 2]))
  stopifnot(location_1[!is.na(location_1[, 2]), 2] %between% c(-90, 90))
  stopifnot(location_2[!is.na(location_2[, 2]), 2] %between% c(-90, 90))

  lon_1 = location_1[, 1] * pi / 180 # converts to radian
  lon_2 = location_2[, 1] * pi / 180

  lat_1 = location_1[, 2] * pi / 180
  lat_2 = location_2[, 2] * pi / 180

  dLat = lat_2 - lat_1
  dLon = lon_2 - lon_1

  a = sin(dLat / 2) ^ 2 + cos(lat_1) * cos(lat_2) * sin(dLon / 2) ^ 2
  a = pmin(a, 1)
  dist = 2 * atan2(sqrt(a), sqrt(1 - a)) * radius

  return(dist)
}
