#' Prepare datasets for trip rate calculations
#'
#' @param summarize_by Name of the variable to summarize trip rates by. Default
#'  is NULL.
#' @param variables_dt List of variable locations and descriptions in data.table
#'  format.
#' @param tripdat Dataset of trips in data.table format.
#' @param daydat Dataset of days in data.table format.
#' @param remove_outliers Boolean whether or not to remove outliers from dataset.
#'  Default is TRUE.
#' @param threshold Threshold to define outliers. Default is 0.975.
#' @param weighted Whether the data is weighted. Default is TRUE.
#' 
#' @return List of binned number of trips with key columns and summarize by variable,
#' unbinned number of trips with key columns and summarize by variable, and a
#' breakdown of outliers if removed.
#' @export
#'
#' @examples
#' 
#' require(data.table)
#' require(stringr)
#' hts_prep_triprate(variables_dt = variable_list,
#'                   tripdat = trip,
#'                   daydat = day)
#' hts_prep_triprate(summarize_by = 'age',
#'                   variables_dt = variable_list,
#'                   tripdat = trip,
#'                   daydat = day)
hts_prep_triprate = function(summarize_by = NULL,
                             variables_dt = variable_list,
                             tripdat = trip,
                             daydat = day,
                             remove_outliers = TRUE,
                             threshold = 0.975,
                             weighted = TRUE,
                             hts_data) {
  
  tripratekeys = c("hh_id", "person_id", "day_id")
  trip_subset_cols = hts_get_keycols(tripdat)
  day_subset_cols = hts_get_keycols(daydat)
  
  if (weighted & (!"trip_weight" %in% trip_subset_cols |
                  !"day_weight" %in% day_subset_cols)) {
    stop("Trip/Day weight not found - are these data weighted?")
  }
  
  day_control = daydat[, ..day_subset_cols]
  
  trip_control = merge(day_control,
                       tripdat[, ..trip_subset_cols],
                       all.x = TRUE)
  
  if (length(summarize_by) == 0) {
    
    if (weighted) {
      triprate_dt = tripdat[, .(num_trips = sum(trip_weight)),
                            by = tripratekeys]
    }
    
    if (!weighted) {
      triprate_dt = tripdat[, .(triprate_binned = .N),
                            by = tripratekeys]
    }
    
    join_vars = names(triprate_dt)[names(triprate_dt) %in% names(day_control)]
    
    triprate_dt = merge(day_control,
                        triprate_dt,
                        all.x = TRUE,
                        all.y = FALSE)
    
    # fill in with zeros for zero trips on a given day:
    triprate_dt[, `:=` (
      num_trips = nafill(num_trips, fill = 0)
    )]
    
    if (weighted){
      
      # calculate trip rate
      triprate_dt[, trip_rate := 
                    ifelse(num_trips == 0, 0, num_trips / day_weight)]
      
      triprate_dt[, num_trips := NULL]
      
      setnames(triprate_dt, 'trip_rate', 'num_trips')
    }
  }
  
  if (length(summarize_by) > 0) {
    
    byvar_dt = hts_prep_byvar(summarize_by, variables_dt = variables_dt, hts_data = hts_data)
    
    merge_cols = names(byvar_dt)[names(byvar_dt) %in% names(trip_control)]
    
    triprate_dt = merge(trip_control, byvar_dt, by = merge_cols)
    
    triprate_cols = hts_get_keycols(triprate_dt)
    
    triprate_cols = triprate_cols[!triprate_cols %in% c("trip_id", "trip_weight")]
    
    triprate_cols_all = c(triprate_cols, summarize_by)
    
    if (weighted) {
      triprate_dt = triprate_dt[, .(num_trips = sum(trip_weight)),
                                by = triprate_cols_all]
    }
    
    if (!weighted) {
      triprate_dt = triprate_dt[, .(triprate_binned = .N),
                                by = triprate_cols_all]
    }
    
    # fill in with zeros for zero trips on a given day:
    triprate_dt[, `:=` (
      num_trips = nafill(num_trips, fill = 0)
    )]
    
    # If one of the by-variables is in trip table, need to expand to
    # include all levels of the variable for every trip, and fill with zeros:
    if ("trip_id" %in% names(byvar_dt)) {
      # fill in with zeros for zero trips for a given level of xt_var using dcast:
      dcast_formula =
        paste0(paste0(triprate_cols, collapse = " + "),
               " ~ ",
               paste0(summarize_by, collapse = " + "))
      
      triprate_cast = dcast(triprate_dt,
                            dcast_formula,
                            value.var = "num_trips",
                            fill = 0)
      
      # Remove columns where NA levels of factors were generated during dcast:
      na_filled_cols = names(triprate_cast)[names(triprate_cast) %like% "_NA"]
      
      if (length(na_filled_cols) > 0) {
        triprate_cast[, c(na_filled_cols) := NULL]
      }
      
      # transform back to long format, with separate cols for weighted & unwt. trip rates:
      triprate_dt = data.table::melt(
        triprate_cast,
        id.vars = triprate_cols,
        value.name = "num_trips"
      )
      
      # Relabel xtab trip vars after melting:
      if(length(summarize_by) > 1){
        triprate_dt[, c(summarize_by) := tstrsplit(variable, "_")]
        triprate_dt[, variable := NULL]
      }
      
      if(length(summarize_by) == 1){
        setnames(triprate_dt, old = "variable", new = summarize_by)
      }
      
      triprate_dt = triprate_dt[]
      
    }
    
    if (weighted){
      
      # calculate trip rate
      triprate_dt[, trip_rate := 
                    ifelse(num_trips == 0, 0, num_trips / day_weight)]
      
      triprate_dt[, num_trips := NULL]
      
      setnames(triprate_dt, 'trip_rate', 'num_trips')
    }
    
  }
  
  # remove outliers
  if (remove_outliers){
    
    out = hts_remove_outliers(triprate_dt, 
                              numvar = 'num_trips',
                              threshold = threshold)
    
    triprate_dt = out[['dt']]
    
    outlier_table = out[['outlier_description']]
    
  }
  
  # Bin trips:
  triprate_binned = hts_bin_var(prepped_dt = triprate_dt,
                                numvar = "num_trips",
                                nbins = 7)
  
  prepped_dt_ls = list("num" = triprate_dt,
                       "cat" = triprate_binned)
  
  # Append outliers:
  if (remove_outliers) {
    prepped_dt_ls = list(
      "cat" = triprate_binned,
      "num" = triprate_dt,
      "outliers" = outlier_table)
  }
  
  
  return(prepped_dt_ls)
  
}
