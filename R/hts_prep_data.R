#' Prepare datasets to make summaries
#'
#' @param summarize_var Name of the variable to summarize. Default is NULL
#' @param summarize_by Name of the variable to summarize the summarize_var by.
#'  Default is NULL.
#' @param variables_dt List of variable locations and descriptions in data.table
#'  format.
#' @param data List of household, person, vehicle, day, and trip tables in
#'  data.table format.
#' @param id_cols name of unique identifier for each table in hts_data
#' @param weighted Whether the data is weighted. Default is TRUE.
#' @param wt_cols weight name for each table in hts_data
#' @param remove_outliers Whether to remove outliers for numeric variable. Default
#'  is TRUE.
#' @param threshold Threshold to define outliers. Default is 0.975.
#' @param remove_missing Whether to remove missing values from the summary.
#'  Default is TRUE.
#' @param missing_values Missing values to remove. Default is 995.
#' @param not_imputable Value representing 'Not imputable' to remove. Default
#'  is -1.
#' @param strataname  Name of strata name to bring in. Default is NULL.
#'
#' @return List containing the categorical and numeric datasets of the summary
#' variables and key columns, and either whether the summarize variable is shared
#' or a breakdown of outliers, depending on if the summarize variable is
#' categorical or numeric.
#' @export
#'
#' @examples
#'
#' require(data.table)
#' require(stringr)
#' hts_prep_data(summarize_var = 'age',
#'               variables_dt = variable_list,
#'               data = list('hh' = hh,
#'                             'person' = person,
#'                             'day' = day,
#'                             'trip' = trip,
#'                             'vehicle' = vehicle))
#' hts_prep_data(summarize_var = 'speed_mph',
#'               summarize_by = 'age',
#'               variables_dt = variable_list,
#'               data = list('hh' = hh,
#'                             'person' = person,
#'                             'day' = day,
#'                             'trip' = trip,
#'                             'vehicle' = vehicle))
#'                             
#'                             
#' hts_prep_data(summarize_var = 'employment',
#'               summarize_by = c('age', 'race'),
#'               variables_dt = variable_list,
#'               data = list('hh' = hh,
#'                             'person' = person,
#'                             'day' = day,
#'                             'trip' = trip,
#'                             'vehicle' = vehicle))
hts_prep_data = function(summarize_var = NULL,
                         summarize_by = NULL,
                         variables_dt = variable_list,
                         data = hts_data,
                         id_cols = c('hh_id', 'person_id', 'day_id', 'trip_id', 'vehicle_id'),
                         weighted = TRUE,
                         wt_cols = c('hh_weight', 'person_weight', 'day_weight', 'trip_weight', 'hh_weight'),
                         remove_outliers = TRUE,
                         threshold = 0.975,
                         remove_missing = TRUE,
                         missing_values = c("Missing Response", "995"),
                         not_imputable = -1,
                         strataname = NULL) {
  # tictoc::tic("Total Time")
  
  # Message:
  msg_pt1 = paste0("Creating a summary of ",
                   hts_find_var(summarize_var, variables_dt = variables_dt), " ", summarize_var)
  
  
  if (!is.null(summarize_by)){
    
    byvarlocs = lapply(summarize_by, hts_find_var)
    
    for(b in 1:length(byvarlocs)) {
      byvarlocs[b] = paste0(byvarlocs[[b]], " ", summarize_by[[b]])
    }
    
    byvarlocs = unlist(byvarlocs)
    
    msg_pt2 = ifelse(length(summarize_by) > 0,
                     paste0("broken down by ",
                            paste0(byvarlocs, collapse = " and ")),
                     "")
  } else {
    msg_pt2 = NULL
  }
  message(paste0(msg_pt1, " ", msg_pt2))
  # TODO: Could we put id and weight cols in a snippet or some such?
  # Or in a settings/options for these functions?
  
  if (remove_missing){
    
    data = hts_remove_missing_data(hts_data = data,
                                   variables_dt = variables_dt,
                                   summarize_var = summarize_var,
                                   summarize_by = summarize_by,
                                   ids = id_cols,
                                   missing_values = missing_values,
                                   not_imputable = not_imputable)
  }
  
  # Find location of summary variable:
  var_location = hts_find_var(summarize_var, variables_dt = variables_dt)
  
  tbl_idx = which(names(data) == var_location)
  
  # Select table where this variable lives:
  var_dt = data[[var_location]]
  
  # Is this a shared variable?
  var_is_shared = variables_dt[shared_name == summarize_var, is_checkbox][1] == 1

  # If yes, expand summarize_var:
  if (var_is_shared) {
    
    summarize_var = variables_dt[shared_name == summarize_var, variable]

    for(i in 1:length(summarize_var)){
      
      if(var_dt[,class(get(summarize_var[i]))] != 'integer'){
        
        message("Checkbox variables must have integer values")
        
        stop()
        
      }
      
    }
    
    
  }
  
  # only keep ids that are in var_dt 
  id_cols = intersect(id_cols, names(var_dt))

  # Subset table to these column(s):
  wtname = wt_cols[tbl_idx]
  
  if (weighted){
    
    subset_cols = c(id_cols, summarize_var, wtname)
    
  } else {
    
    subset_cols = c(id_cols, summarize_var)
    
  }
  
  var_dt = var_dt[, subset_cols, with=FALSE]
  
  # If shared variable, melt var_dt:
  if (var_is_shared) {
    
    shared_name = variables_dt[variable == summarize_var[[1]],
                               shared_name]
    
    if (weighted){
      
      var_dt = hts_melt_vars(
        shared_name = shared_name,
        wide_dt = var_dt,
        variables_dt = variables_dt,
        shared_name_vars = summarize_var,
        ids = c(id_cols, wtname),
        remove_missing = TRUE,
        checkbox_label_sep = ":",
        missing_values = c("Missing Response", "995"),
        to_single_row = FALSE
      )
      
    } else {
      
      var_dt = hts_melt_vars(
        shared_name = shared_name,
        wide_dt = var_dt,
        variables_dt = variables_dt,
        shared_name_vars = summarize_var,
        ids = id_cols,
        remove_missing = TRUE,
        checkbox_label_sep = ":",
        missing_values = c("Missing Response", "995"),
        to_single_row = FALSE
      ) 
      
    }


    summarize_var = shared_name
    
    setnames(var_dt, shared_name, 'shared_name')
    
    # make factor levels
    var_dt$shared_name = factor(var_dt$shared_name, levels = unique(var_dt$shared_name))
    
    setnames(var_dt, 'shared_name', shared_name)
    
  }
  
  # Identify, then bin, if summarize_var is numeric:
  v_class = variables_dt[shared_name == summarize_var, data_type][[1]]
  
  if (!v_class %in% c("integer", "numeric")) {
    var_dt_num = NULL
    var_dt_cat = var_dt
    
  }
  
  if (v_class %in% c("integer", "numeric")) {
    
    # remove outliers
    if (remove_outliers){
      
      out = hts_remove_outliers(var_dt,
                                numvar = summarize_var,
                                threshold = threshold)
      
      var_dt = out[['dt']]
      
      outlier_table = out[['outlier_description']]
      
    }
    
    # save a copy of the un-binned data:
    var_dt_num = data.table::copy(var_dt)
    
    
    # bin the data for categorical summaries:
    var_dt_cat = hts_bin_var(prepped_dt = var_dt,
                             numvar = summarize_var,
                             nbins = 7)
    
  }
  
  # Summarize-by variables:
  if (length(summarize_by) == 0) {
    
    num_res = var_dt_num
    cat_res = var_dt_cat
    
  }
  
  if (length(summarize_by) > 0) {


    for (i in 1:length(summarize_by)){
      
      var = summarize_by[i]
      
      byvar_location = hts_find_var(var, variables_dt = variables_dt)
      
      # Select table where this variable lives:
      byvar_table = data[[byvar_location]]
      
      byvar_is_shared = variables_dt[shared_name == var, is_checkbox][1] == 1
      
      if (byvar_is_shared) {
        
        var = variables_dt[shared_name == var, variable][1]
        
      }
      
      if(byvar_is_shared & byvar_table[,class(get(var))] != 'integer'){
        
        message("Checkbox variables must have integer values")
        
        stop()
        
      }
      
    }

    byvar_dt = hts_prep_byvar(summarize_by,
                              variables_dt = variables_dt,
                              hts_data = data,
                              byvar_ids = id_cols,
                              byvar_wts = wt_cols)
    
    # Merge by var and summarize var:
    allow_cartesian_setting = FALSE
    
    if (var_is_shared == TRUE) {
      allow_cartesian_setting = TRUE
    }
    
    cat_res = merge(var_dt_cat,
                    byvar_dt,
                    all.x = FALSE, all.y = FALSE,
                    allow.cartesian = allow_cartesian_setting)
    
    setcolorder(cat_res, intersect(c(id_cols, wt_cols, summarize_var, summarize_by), names(cat_res)))
    
    if (v_class %in% c("integer", "numeric")) {
      num_res = merge(var_dt_num,
                      byvar_dt,
                      all.x = FALSE, all.y = FALSE,
                      allow.cartesian = allow_cartesian_setting)
      
      setcolorder(num_res, intersect(c(id_cols, wt_cols, summarize_var, summarize_by), names(cat_res)))
      
    }
    
    if (!v_class %in% c("integer", "numeric")) {
      num_res = NULL
    }
    
    
  }

  if (!is.null(strataname)) {
    
    if(!is.null(cat_res)){
      
      cat_res = hts_cbind_var(lhs_table = cat_res,
                              rhs_var = strataname,
                              hts_data = data,
                              variable_list = variables_dt,
                              cbind_ids = id_cols,
                              cbind_wts = wt_cols)
      
    }
    
    if(!is.null(num_res)){
      
      num_res = hts_cbind_var(lhs_table = num_res,
                              rhs_var = strataname,
                              hts_data = data,
                              variable_list = variables_dt,
                              cbind_ids = id_cols,
                              cbind_wts = wt_cols)
      
    }
  }
  
  prepped_dt_ls = list("cat" = cat_res,
                       "num" = num_res,
                       "var_is_shared" = var_is_shared)
  
  # Append outliers:
  if (v_class %in% c("integer", 'numeric') & remove_outliers) {
    prepped_dt_ls = list(
      "cat" = cat_res,
      "num" = num_res,
      "outliers" = outlier_table)
  }
  
  
  return(prepped_dt_ls)
}


## quiets concerns of R CMD check
utils::globalVariables(c("hts_data", "is_checkbox", "data_type"))
