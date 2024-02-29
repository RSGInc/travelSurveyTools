#' Join spatial data to points
#'
#' Performs a spatial join to obtain geographic fields for specified lon/lat
#' columns.
#'
#' @param x A data.table of survey data
#' @param y an \code{sf} object with the geometry of interest
#' @param id_col character. The column in \code{x} that is a unique identifier
#' @param lon_col character. The column in \code{x} that has longitude
#' @param lat_col character.  The column in \code{x} that has latitude
#' @param crs_lonlat numeric.  The EPSG code for lonlat data (defaults to WGS84)
#' @param crs_equal_area numeric.  The EPSG code for an equal area projection.
#'   Many spatial operations assume projected data (not lon/lat).  Defaults to US
#'   Albers Equal Area
#' @param largest logical.  If \code{TRUE}, return \code{x}
#'   features with fields of \code{y} that have the largest overlap with each of
#'   the features of \code{x}; see \code{\link[sf]{st_join}}. This is useful if
#'   join is returning more than one value of y for each x.
#' @return A data.table is returned with all columns from x plus all columns
#'   from y.
#' @export
#' @author Matt Landis
#'
#' @import sf
#'
#' @examples
#' \dontrun{
#' x = data.table(id = 1:3, lon = c(-82.33, -79.17, -76.17), lat = c(35.50, 36.27, 36.49))
#' nc = st_read(system.file("shape/nc.shp", package = "sf"))[, c("FIPS", "NAME", "geometry")]
#' z = join_spatial(x, nc, id_col = "id", lon_col = "lon", lat_col = "lat")
#' z
#' }
#'
join_spatial =
  function(
      x,
      y,
      id_col,
      lon_col,
      lat_col,
      crs_lonlat = 4326,
      crs_equal_area = 5070,
      largest = FALSE) {
    n_x = x[, .N]

    x_spatial =
      x[!is.na(get(lat_col)), c(id_col, lon_col, lat_col), with = FALSE] %>%
      sf::st_as_sf(coords = c(lon_col, lat_col), crs = crs_lonlat) %>%
      sf::st_transform(crs = crs_equal_area)

    y_with_id =
      y %>%
      sf::st_transform(crs = crs_equal_area) %>%
      sf::st_join(x_spatial, ., largest = largest) %>%
      sf::st_drop_geometry() %>%
      data.table::data.table()

    x_with_y = merge(x, y_with_id, by = id_col, all.x = TRUE)

    if (largest) {
      stopifnot(n_x == x_with_y[, .N])
    }

    return(x_with_y)
  }
