hts_trip_vehid = function(trip_table,
                          vehicle_table,
                          vehicle_mode_type = "Vehicle",
                          values_dt = value_labels,
                          ...) {
  trip_dt =data.table::copy(trip_table)
  if (!("mode_1" %in% names(trip_dt)
        & "mode_type" %in% names(trip_dt))) {
    stop("Please append mode_type and mode_1 to trip table
before proceding.")
  }
  # check - is this factorized data?
  is_factorized = any(class(trip_dt$mode_type) == "factor")
  if (is_factorized == FALSE) {
    trip_dt[, vehicle_id := factorize_column(x = mode_1,
                                             var_str = "mode_1",
                                             vals_df = values_dt)]
  }
  trip_dt[,
          vehicle_id :=
            fcase(mode_type == vehicle_mode_type,
                  as.numeric(paste0(
                    hh_id,
                    stringr::str_pad(
                      paste0(str_extract_all(mode_1, pattern = "[:digit:]")[[1]],
                             collapse = ""),
                      width = 2,
                      side = "left",
                      pad = "0"
                    )
                  ),
                  default = NA)),
          by = row.names(trip_dt)]
  if (class(vehicle_table$vehicle_id) == "integer64") {
    trip_dt[, vehicle_id := as.integer64(vehicle_id)]
  }
  return(trip_dt)
}
