#' Bind a column from one table to another
#'
#' @param lhs_table Table to bind a column to in data.table format
#' @param rhs_var Variable to bind to the lhs_table.
#' @param variable_list A variable list with descriptions and table locations
#'  of variables.
#' @param return_weight_cols If true binds weight variable along with rhs_var
#'  to lhs_table. Default is FALSE.
#' @param ... Additional arguments passed to \code{link{hts_get_keycols}}
#' 
#' @return Inputted table with inputted variable binded.
#' @export
#'
#' @examples
#' 
#' require(data.table)
#' hts_cbind_var(lhs_table = trip, rhs_var = 'num_people', variable_list = variable_list)
#' hts_cbind_var(lhs_table = trip, rhs_var = 'num_people', 
#' variable_list = variable_list, return_weight_cols = TRUE)
#'
hts_cbind_var = function(lhs_table,
                         rhs_var,
                         variable_list = variable_list,
                         return_weight_cols = FALSE,
                         ...) {
  
  var_location =
    hts_find_var(rhs_var, variables_dt = variable_list)
  
  rhs_table =data.table::copy(get(as.character(var_location)))
  
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
  selected_cols = c(hts_get_keycols(rhs_table,
                                  ids = TRUE,
                                  weights = return_weight_cols), rhs_var)
  
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
