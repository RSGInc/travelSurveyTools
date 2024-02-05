#' Bind a column from one table to another
#'
#' @param lhs_table Table to bind a column to in data.table format
#' @param rhs_var Variable to bind to the lhs_table.
#' @param hts_data List of household, person, vehicle, day, and trip tables in
#'  data.table format.
#' @param variable_list A variable list with descriptions and table locations
#'  of variables.
#' @param cbind_ids list of unique identifiers for each table in hts_data
#' @param cbind_wts list of weight for each table in hts_data 
#' @param return_weight_cols If true binds weight variable along with rhs_var
#'  to lhs_table. Default is FALSE.
#' 
#' @return Inputted table with inputted variable binded.
#' @export
#'
#' @examples
#' 
#' require(data.table)
#' hts_cbind_var(lhs_table = trip,
#'  rhs_var = 'speed_mph',
#'  hts_data = test_data,
#'   variable_list = variable_list)
#' hts_cbind_var(lhs_table = trip,
#'  rhs_var = 'speed_mph',
#'   hts_data = test_data,
#' variable_list = variable_list,
#'  return_weight_cols = TRUE)
#'
hts_cbind_var = function(lhs_table,
                         rhs_var,
                         hts_data,
                         variable_list = variable_list,
                         return_weight_cols = FALSE,
                         cbind_ids = c('hh_id', 'person_id', 'day_id', 'trip_id', 'vehicle_id'),
                         cbind_wts = c('hh_weight', 'person_weight', 'day_weight', 'trip_weight', 'hh_weight')) {
  
  var_location =
    hts_find_var(rhs_var, data = hts_data, variables_dt = variable_list)
  
  rhs_table = hts_data[[var_location]]
  
  # If joining trip to vehicle or vice versa, need vehicle ID:
  if ("trip_id" %in% names(lhs_table) &
      var_location == "vehicle" &
      !"vehicle_id" %in% names(lhs_table)) {
    lhs_table = hts_trip_vehid(trip_table = lhs_table,
                               vehicle_table = rhs_table)
  }
  if ("vehicle_id" %in% names(lhs_table) &
      (!"trip_id" %in% names(lhs_table)) &
      var_location == "trip" &
      !"vehicle_id" %in% names(rhs_table)) {
    rhs_table = hts_trip_vehid(trip_table = rhs_table,
                               vehicle_table = lhs_table)
  }
  
  # Subset table to ID columns, weight columns (if desired), rhs_var:
  selected_cols = c(intersect(
    names(rhs_table),
    c(cbind_ids, cbind_wts)
  ),
  rhs_var)
  
  rhs_table = rhs_table[, selected_cols, with = FALSE]
  
  # Merge lhs_table to table with rhs_var:
  common_cols = intersect(names(lhs_table), names(rhs_table))
  message("Joining ",
          rhs_var,
          " to table on ",
          paste0(common_cols, collapse = ", "))
  merge_t = merge(
    lhs_table,
    rhs_table,
    by = common_cols,
    all.x = TRUE,
    all.y = FALSE
  )
  
  return(merge_t)
}
