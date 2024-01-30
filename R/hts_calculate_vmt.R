#' Calculate vmt for each trip
#'
#' @param trip_dt Data table of trip data
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
#' test_data$trip[, vmt := 
#'  hts_calculate_vmt(
#'   trip_dt = test_data$trip,
#'   mode_cols = c('mode_1', 'mode_2'),
#'   miles_col = 'distance_miles',
#'   vehicle_modes = c(6, 7, 10)
#'  )
#' ]
#' test_data$trip[, vmt := 
#'  hts_calculate_vmt(
#'   trip_dt = test_data$trip,
#'   mode_cols = 'mode_type',
#'   miles_col = 'distance_miles',
#'   vehicle_modes = 8,
#'   occupancy_var = 'num_travelers'
#'  )
#' ]


hts_calculate_vmt = function(trip_dt,
                             mode_cols,
                             miles_col,
                             vehicle_modes,
                             occupancy_var = NULL){
  
  
  dt = copy(trip_dt)
  
  #TODO: allow for multiple mode columns
  
  
  if (length(mode_cols) > 1){
    
    in_modes = 0
    
    for (i in 1:length(mode_cols)){
      
      var_name = mode_cols[i]
      
      col = 1 * (dt[, get(var_name)] %in% vehicle_modes)
      
      in_modes = col + in_modes
      
    }
    
    dt[, in_vehicle_modes := 1*(in_modes > 0)]
    
  } else {
    
    dt[, in_vehicle_modes := 1 * (get(mode_cols) %in% vehicle_modes)]
    
  }
  
  if (!is.null(occupancy_var)){
    
    vmt = dt[, in_vehicle_modes * (get(miles_col) / get(occupancy_var))]
    
  }else{
    
    vmt = dt[, in_vehicle_modes] * dt[, get(miles_col)]
    
    
  }
  
  return(vmt[])
  
}
