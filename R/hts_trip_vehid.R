#' Add vehicle_id to trip table
#'
#' @param trip_table Dataset of trips in data.table format.
#' @param vehicle_table Dataset of vehicles in data.table format.
#' @param vehicle_mode_type Mode type label for vehicle. Default is 'Vehicle'.
#' @param values_dt Dataset of value labels in data.table format.
#' @param ... Additional arguments passed to \code{link{factorize_column}}
#'
#' @return Trip table with vehicle_id attached.
#' @export
#'
#' @examples
#'
#' require(data.table)
#' trip_ex = data.table(
#'       hh_id = sample(1:10, size = 30, replace = TRUE),
#'       trip_id = 1:30,
#'       mode_type = sample(1:2, size = 30, replace = TRUE),
#'       mode_1 = sample(1, size = 30, replace = TRUE))
#' vehicle_ex = data.table(
#'       hh_id = sample(1:10, size = 30, replace = TRUE),
#'       vehicle_id = 1:30)
#' values_ex = data.table(
#'       variable = c(rep('mode_type', 2), ('mode_1')),
#'       value = c(1,2,1),
#'       value_label = c('Vehicle', 'Walk', 'Car')
#'       )
#' hts_trip_vehid(trip_table = trip_ex,
#'                vehicle_table = vehicle_ex,
#'                vehicle_mode_type = 'Vehicle',
#'                values_dt = values_ex)
#'
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
                      paste0(stringr::str_extract_all(mode_1, pattern = "[:digit:]")[[1]],
                             collapse = ""),
                      width = 2,
                      side = "left",
                      pad = "0"
                    )
                  ),
                  default = NA)),
          by = row.names(trip_dt)]
  if (inherits(vehicle_table$vehicle_id, "integer64")) {
    trip_dt[, vehicle_id := bit64::as.integer64(vehicle_id)]
  }
  return(trip_dt[])
}

## quiets concerns of R CMD check
utils::globalVariables(c("value_labels", "vehicle_id", "mode_1", "mode_type", "hh_id"))

