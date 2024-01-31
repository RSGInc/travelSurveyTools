#' Prepare variable to be summarized
#'
#' @param summarize_by Name of the variable to be prepped. Default is NULL.
#' @param variables_dt List of variable locations and descriptions in data.table
#'  format.
#' @param hts_data List containing household, person, day, trip, and vehicle
#'  datasets in data.table format.
#' @param byvar_ids unique identifiers for each table in hts_data
#' @param byvar_wts weight column for each table in hts_data 
#' @param ... Additional parameters to pass to \code{link{hts_melt_vars}}
#'
#' @return Data table containing the variable to be summarized and other key
#'  columns.
#' @export
#'
#' @examples
#'
#' hts_prep_byvar(summarize_by = 'age',
#'                variables_dt = variable_list,
#'                hts_data = list('hh' = hh,
#'                             'person' = person,
#'                             'day' = day,
#'                             'trip' = trip,
#'                             'vehicle' = vehicle))
#' hts_prep_byvar(summarize_by = 'race',
#'                variables_dt = variable_list,
#'                hts_data = list('hh' = hh,
#'                             'person' = person,
#'                             'day' = day,
#'                             'trip' = trip,
#'                             'vehicle' = vehicle))
#'
hts_prep_byvar = function(summarize_by = NULL,
                          variables_dt = variable_list,
                          hts_data,
                          byvar_ids = c('hh_id', 'person_id', 'day_id', 'trip_id', 'vehicle_id'),
                          byvar_wts = c('hh_weight', 'person_weight', 'day_weight', 'trip_weight', 'hh_weight'),
                          ...) {
  
  # For each variables in trip table:
  byvar_dt_ls = list()
  
  for (b in seq_along(summarize_by)) {
    byvar = summarize_by[[b]]
    
    byvar_loc = hts_find_var(byvar, variables_dt = variables_dt)
    
    tbl_idx = which(names(hts_data) == byvar_loc)
    
    byvar_dt_v = data.table::copy(hts_data[[byvar_loc]])
    
    # Check that specified id column exists in var_dt
    stopifnot("table id not in byvar_dt_v, make id_cols ordered list of ids in each table of data" =
                byvar_ids[[tbl_idx]] %in% names(byvar_dt_v))
    
    # Is this a shared variable?
    byvar_is_shared = variables_dt[shared_name == byvar, is_checkbox][1] == 1
    
    # Is this a numeric variable?
    byvar_is_numeric = variables_dt[shared_name == summarize_by[[b]], data_type][[1]] == "numeric"
    
    if (byvar_is_shared) {
      
      byvar_dt_v =
        hts_melt_vars(
          shared_name = summarize_by[[b]],
          wide_dt = byvar_dt_v,
          ids = byvar_ids,
          shared_name_vars = NULL,
          variables_dt = variables_dt,
          to_single_row = TRUE,
          ...
        )
      
    }
    
    if (byvar_is_numeric) {
      byvar_dt_v = hts_bin_var(prepped_dt = byvar_dt_v,
                               numvar = byvar,
                               nbins = 7)
    }
    
    if (!is.null(byvar_wts)){
      
      table_idx = which(names(hts_data) == byvar_loc)
      wtname = byvar_wts[table_idx]
      
    } else {
      wtname = NULL
    }
    
    
    if (!byvar_is_shared) {
      byvar_cols = c(intersect(c(byvar_ids, wtname), names(byvar_dt_v)), byvar)
      
      byvar_dt_v = byvar_dt_v[, byvar_cols, with=FALSE]
      
    }
    
    byvar_dt_ls[[b]] = byvar_dt_v
    
  }
  

  byvar_dt = Reduce(function(x, y)
    merge(x, y, all.x = FALSE, all.y = FALSE, by = intersect(names(x),
                                                             names(y))
    ),
    byvar_dt_ls)
  
  return(byvar_dt[])
}


## quiets concerns of R CMD check
utils::globalVariables(c("variables_list", "is_checkbox", "data_type"))
