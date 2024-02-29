#' Calculate vmt for each trip
#'
#' @param data List of data tables
#' @param trip_name Name of trip table in data
#' @param ids Unique id in order for each table in data
#' @param agg_tbl Table to append vmt to
#' @param mode_cols Column(s) in trip_dt containing trip mode
#' @param miles_col Column in trip_dt containing miles per trip
#' @param vehicle_modes List of modes that are considered vehicle
#' @param occupancy_var Ocuupancy column to divide distance by if specified. Default is NULL
#'
#' @return List of vmt for each trip in trip_dt
#' @export
#'
#' @examples
#'
#' hts_calculate_vmt(
#'   trip_name = "trip",
#'   data = test_data,
#'   agg_tbl = "day",
#'   mode_cols = c("mode_1", "mode_2"),
#'   miles_col = "distance_miles",
#'   vehicle_modes = c(6, 7, 10)
#' )
#'
#'
#' hts_calculate_vmt(
#'   data = test_data,
#'   trip_name = "trip",
#'   agg_tbl = "trip",
#'   mode_cols = "mode_type",
#'   miles_col = "distance_miles",
#'   vehicle_modes = 8,
#'   occupancy_var = "num_travelers"
#' )
hts_calculate_vmt = function(data,
                              trip_name = "trip",
                              ids = c("hh_id", "person_id", "day_id", "trip_id", "vehicle_id"),
                              agg_tbl = "trip",
                              mode_cols,
                              miles_col,
                              vehicle_modes,
                              occupancy_var = NULL) {
  dt = data[[trip_name]]

  if (length(mode_cols) > 1) {
    in_modes = 0

    for (i in 1:length(mode_cols)) {
      var_name = mode_cols[i]

      col = 1 * (dt[, get(var_name)] %in% vehicle_modes)

      in_modes = col + in_modes
    }

    dt[, in_vehicle_modes := 1 * (in_modes > 0)]
  } else {
    dt[, in_vehicle_modes := 1 * (get(mode_cols) %in% vehicle_modes)]
  }

  if (!is.null(occupancy_var)) {
    dt[, vmt := in_vehicle_modes * (get(miles_col) / get(occupancy_var))]
  } else {
    dt[, vmt := in_vehicle_modes * get(miles_col)]
  }

  # Fill any na values with 0
  dt[is.na(vmt), vmt := 0]

  # Get id to aggregate on
  id_index = which(names(data) == agg_tbl)

  id = ids[id_index]

  vmt_crosswalk = dt[, .(vmt = sum(vmt)), .(id = get(id))]

  setnames(vmt_crosswalk, "id", id)

  vmt_tbl = copy(get(agg_tbl))

  vmt_tbl[
    vmt_crosswalk,
    vmt := i.vmt,
    on = id
  ]

  # Fill in NA values (no trips on agg id) with 0
  vmt_tbl[is.na(vmt), vmt := 0]

  return(vmt_tbl[])
}

## quiets concerns of R CMD check
utils::globalVariables(c("vmt", "i.vmt", "in_vehicle_modes"))
