#' Prepare datasets for VMT rate calculations
#'
#' @param summarize_by Name of the variable to summarize vmt rates by. Default
#'  is NULL.
#' @param variables_dt List of variable locations and descriptions in data.table
#'  format.
#' @param trip_name Name of the trip dataset in hts_data.
#' @param day_name Name of the day dataset in hts_data.
#' @param ids name of unique identifier in each table in hts_data
#' @param dist_var name of distance variable in trip table, preferably miles for VMT
#' @param mode_var name of mode variable in trip table
#' @param veh_regex regular expression pattern capturing vehicular modes in mode_var
#' @param wts name of weight column in each table in hts_data
#' @param remove_outliers Boolean whether or not to remove outliers from dataset.
#'  Default is TRUE.
#' @param threshold Threshold to define outliers. Default is 0.975.
#' @param weighted Whether the data is weighted. Default is TRUE.
#' @param strataname  Name of strata name to bring in. Default is NULL.
#' @param hts_data List containing household, person, day, trip, and vehicle
#'  datasets in data.table format.
#'
#' @return List of binned vehicle miles traveled with key columns and summarize by variable,
#' unbinned vehicle miles traveled with key columns and summarize by variable, and a
#' breakdown of outliers if removed.
#' 
#' @rawNamespace import(data.table, except = c(month, year))
#' @export
hts_prep_vmtrate = function(summarize_by = NULL,
                            variables_dt = variable_list,
                            trip_name = "trip",
                            day_name = "day",
                            ids = c("hh_id", "person_id", "day_id", "trip_id", "vehicle_id"),
                            traveler_count_var = NULL,
                            dist_var = NULL, 
                            mode_var = NULL,
                            mode_regex = NULL,
                            wts = c("hh_weight", "person_weight", "day_weight", "trip_weight", "hh_weight"),
                            remove_outliers = FALSE,
                            threshold = 0.975,
                            weighted = TRUE,
                            strataname=NULL,
                            hts_data = list(
                             "hh" = hh,
                             "person" = person,
                             "day" = day,
                             "trip" = trip,
                             "vehicle" = vehicle)) {
  # Check variable_list first
  variables_dt = hts_validate_variable_list(variables_dt, hts_data)
  
  tripdat = hts_data[[trip_name]]
  daydat = hts_data[[day_name]]
  
  trip_index = which(names(hts_data) == trip_name)
  day_index = which(names(hts_data) == day_name)
  
  # Get ids
  trip_id = ids[trip_index]
  day_id = ids[day_index]
  
  # Get weights
  trip_wt = wts[trip_index]
  day_wt = wts[day_index]
  
  vmtratekeys = intersect(names(tripdat), c(ids[-trip_index], summarize_by))
  trip_subset_cols = intersect(names(tripdat), 
                               c(ids, wts, summarize_by, traveler_count_var, dist_var, mode_var))
  day_subset_cols = intersect(names(daydat), c(ids, wts, summarize_by))
  
  if (weighted & (!trip_wt %in% trip_subset_cols |
                  !day_wt %in% day_subset_cols)) {
    stop("Trip/Day weight not found - are these data weighted?")
  }
  
  day_control = daydat[, day_subset_cols, with = FALSE]
  
  trip_control = merge(day_control,
                       tripdat[, trip_subset_cols, with = FALSE],
                       all.x = TRUE
  )
  
  if (length(summarize_by) == 0) {
    if (weighted) {
      vmtrate_dt = tripdat[grepl(mode_regex, as.character(get(mode_var))), 
                           .(vmt = sum(get(trip_wt) * get(dist_var)/get(traveler_count_var))),
                            by = vmtratekeys
      ]
    }
    
    if (!weighted) {
      vmtrate_dt = tripdat[grepl(mode_regex, as.character(get(mode_var))),
                           .(vmt = sum(get(dist_var)/get(traveler_count_var))),
                            by = vmtratekeys
      ]
    }
    
    join_vars = names(vmtrate_dt)[names(vmtrate_dt) %in% names(day_control)]
    
    vmtrate_dt = merge(day_control,
                        vmtrate_dt,
                        all.x = TRUE,
                        all.y = FALSE
    )
    
    # fill in with zeros for zero trips on a given day:
    vmtrate_dt[, `:=`(
     vmt = nafill(vmt, fill = 0)
    )]
    
    if (weighted) {
      # calculate vmt rate
      vmtrate_dt[, vmt_rate :=
                    ifelse(vmt == 0, 0, vmt / get(day_wt))]
      
      vmtrate_dt[, vmt := NULL]
      
      setnames(vmtrate_dt, "vmt_rate", "vmt")
    }
  }
  
  # If summarize_by is in day and trip set location to day for correct merge
  day_trip_vars = variables_dt[
    shared_name %in% summarize_by &
      get(day_name) == 1 & 
      get(trip_name) == 1, shared_name]
  
  if (length(day_trip_vars) > 0) {
    
    setnames(variables_dt, trip_name, 'trip_table')
    
    variables_dt[shared_name %in% day_trip_vars, trip_table := 0]
    
    setnames(variables_dt, 'trip_table', trip_name)
    
  }
  
  if (length(summarize_by) > 0) {
    byvar_dt = hts_prep_byvar(summarize_by,
                              variables_dt = variables_dt,
                              hts_data = hts_data,
                              byvar_ids = ids,
                              byvar_wts = wts
    )
    
    merge_cols = names(byvar_dt)[names(byvar_dt) %in% names(trip_control)]
    
    vmtrate_dt = merge(trip_control, byvar_dt, by = merge_cols, all.x = TRUE)
    
    vmtrate_cols = intersect(names(vmtrate_dt), c(ids, wts))
    
    vmtrate_cols = vmtrate_cols[!vmtrate_cols %in% c(trip_id, trip_wt)]
    
    vmtrate_cols_all = c(vmtrate_cols, summarize_by)
    
    if (weighted) {
      vmtrate_dt = vmtrate_dt[grepl(mode_regex, as.character(get(mode_var))), 
                              .(vmt = sum(get(trip_wt) * get(dist_var)/get(traveler_count_var))),
                              by = vmtrate_cols_all
      ]
    }

    if (!weighted) {
      vmtrate_dt = vmtrate_dt[grepl(mode_regex, as.character(get(mode_var))), 
                              .(vmt = sum(!is.na(get(..trip_id)) * get(dist_var)/get(traveler_count_var))),
                                by = vmtrate_cols_all
      ]
    }
    
    # fill in with zeros for zero trips on a given day:
    vmtrate_dt[, `:=`(
      vmt = nafill(vmt, fill = 0)
    )]
    
    # If one of the by-variables is in trip table, need to expand to
    # include all levels of the variable for every trip, and fill with zeros:
    if (trip_id %in% names(byvar_dt)) {
      # fill in with zeros for zero trips for a given level of xt_var using dcast:
      dcast_formula =
        paste0(
          paste0(vmtrate_cols, collapse = " + "),
          " ~ ",
          paste0(summarize_by, collapse = " + ")
        )
      
      vmtrate_cast = dcast(vmtrate_dt,
                           dcast_formula,
                           value.var = "vmt",
                           fill = 0
      )
      
      # Remove columns where NA levels of factors were generated during dcast:
      na_filled_cols = names(vmtrate_cast)[names(vmtrate_cast) %like% "_NA" | names(vmtrate_cast) == "NA"]
      
      if (length(na_filled_cols) > 0) {
        vmtrate_cast[, c(na_filled_cols) := NULL]
      }

      # transform back to long format, with separate cols for weighted & unwt. vmt rates:
      vmtrate_dt = data.table::melt(
        vmtrate_cast,
        id.vars = vmtrate_cols,
        value.name = "vmt"
      )
      
      # Relabel xtab trip vars after melting:
      if (length(summarize_by) > 1) {
        vmtrate_dt[, c(summarize_by) := tstrsplit(variable, "_")]
        vmtrate_dt[, variable := NULL]
      }
      
      if (length(summarize_by) == 1) {
        setnames(vmtrate_dt, old = "variable", new = summarize_by)
      }
      
      vmtrate_dt = vmtrate_dt[]
    }
    
    if (weighted) {
      # calculate vmt rate
      vmtrate_dt[, vmt_rate :=
                    ifelse(vmt == 0, 0, vmt/ get(day_wt))]
      
      # Save vehicle miles traveled under a different name
      setnames(vmtrate_dt, "vmt", "vehicle_miles_wtd")
      
      setnames(vmtrate_dt, "vmt_rate", "vmt")
    }
  }
  
  # remove outliers
  if (remove_outliers) {
    out = hts_remove_outliers(vmtrate_dt,
                              numvar = "vmt",
                              threshold = threshold
    )
    
    vmtrate_dt = out[["dt"]]
    
    outlier_table = out[["outlier_description"]]
  }
  
  # survey strata
  if(!is.null(strataname)){
    dts = hts_cbind_var(
      lhs_table = vmtrate_dt,
      rhs_var = strataname,
      hts_data = hts_data,
      variable_list = variables_dt,
      cbind_ids = ids,
      cbind_wts = wts
    )
    vmtrate_dt = dts
  }
  
  # Bin trips:
  vmtrate_binned = hts_bin_var(
    prepped_dt = vmtrate_dt,
    numvar = "vmt",
    nbins = 7
  )
  
  if (weighted) {
    setnames(vmtrate_dt, "vmt", "vmt_wtd", skip_absent = TRUE)
    setnames(vmtrate_binned, "vmt", "vmt_wtd", skip_absent = TRUE)
  } else {
    setnames(vmtrate_dt, "vmt", "vmt_unwtd", skip_absent = TRUE)
    setnames(vmtrate_binned, "vmt", "vmt_unwtd", skip_absent = TRUE)
  }
  
  prepped_dt_ls = list(
    "num" = vmtrate_dt,
    "cat" = vmtrate_binned
  )
  
  # Append outliers:
  if (remove_outliers) {
    prepped_dt_ls = list(
      "cat" = vmtrate_binned,
      "num" = vmtrate_dt,
      "outliers" = outlier_table
    )
  }
  
  return(prepped_dt_ls)
}

## quiets concerns of R CMD check
utils::globalVariables(c("..trip_id","trip_weight", "vmt", "vmt_rate", "day_weight", "trip_table"))
