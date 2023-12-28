#' Prepare datasets to make summaries
#'
#' @param summarize_var Name of the variable to summarize. Default is NULL
#' @param summarize_by Name of the variable to summarize the summarize_var by.
#' Default is NULL.
#' @param variables_dt List of variable locations and descriptions in data.table
#' format.
#' @param data List of household, person, vehicle, day, and trip tables in
#' data.table format.
#' @param weighted Whether the data is weighted. Default is TRUE.
#' @param remove_outliers Whether to remove outliers for numeric variable. Default
#' is TRUE.
#' @param threshold Threshold to define outliers. Default is 0.975.
#' 
#' @return List containing the categorical and numeric datasets of the summary
#' variables and key columns, and either whether the summarize variable is shared
#' or a breakdown of outliers, depending on if the summarize variable is
#' categorical or numeric.
#' @export
#'
#' @examples
#' set.seed(45)
#' require(data.table)
#' require(stringr)
#' person = data.table(
#'               hh_id = rep(1:10,2),
#'               person_id = 1:20,
#'               age = sample(1:10, size = 20, replace = TRUE),
#'               num_jobs = sample(1:10, size = 20, replace = TRUE))
#' day = data.table(
#'               hh_id = rep(1:10,4),
#'               person_id = rep(1:20,2),
#'               day_id = 1:40,
#'               day_weight = sample(1:10, size = 40, replace = TRUE))
#' trip = data.table(
#'               hh_id = rep(1:10,8),
#'               person_id = rep(1:20,4),
#'               trip_id = 1:80,
#'               day_id = rep(1:40,2),
#'               trip_weight = sample(1:10, size = 80, replace = TRUE))
#' hts_data = list(person = person, day = day, trip = trip)
#' variable_list = data.table(
#'       variable = c('age', 'num_jobs'),
#'       hh = c(0,0),
#'       person = c(1,1),
#'       vehicle = c(0,0),
#'       day = c(0,0),
#'       trip = c(0,0),
#'       shared_name = c('age', 'num_jobs'),
#'       description = c('Age', 'Number of jobs'),
#'       is_checkbox = c(0, 0),
#'       data_type = c('integer/categorical', 'numeric'))
#'hts_prep_data(summarize_var = 'age', variables_dt = variable_list)
#'hts_prep_data(summarize_var = 'num_jobs', summarize_by = 'age', variables_dt =
#'variable_list)
hts_prep_data = function(summarize_var = NULL,
                         summarize_by = NULL,
                         variables_dt = variable_list,
                         data = hts_data,
                         weighted = TRUE,
                         remove_outliers = TRUE,
                         threshold = 0.975) {
  
  # TODO: Could we put id and weight cols in a snippet or some such? 
  # Or in a settings/options for these functions?
  
  # Find location of summary variable:
  var_location = hts_find_var(summarize_var, variables_dt = variables_dt)
  
  # Select table where this variable lives:
  var_dt = data[[var_location]]
  
  # Is this a shared variable?
  var_is_shared = variables_dt[shared_name == summarize_var, is_checkbox][1] == 1
  
  # If yes, expand summarize_var:
  if (var_is_shared) {
    
    summarize_var = variables_dt[shared_name == summarize_var, variable]
    
  }
  
  # Subset table to these column(s):
  subset_cols = c(hts_get_keycols(var_dt), summarize_var)
  
  var_dt = var_dt[, ..subset_cols]
  
  # If shared variable, melt var_dt:
  if (var_is_shared) {
    
    shared_name = variables_dt[variable == summarize_var[[1]],
                               shared_name]
    
    var_dt = hts_melt_vars(
      shared_name = shared_name,
      wide_dt = var_dt,
      variables_dt = variables_dt,
      shared_name_vars = summarize_var,
      remove_missing = TRUE,
      checkbox_label_sep = ":",
      missing_values = c("Missing Response", "995"),
      to_single_row = FALSE
    )
    
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
    
    byvar_dt = hts_prep_byvar(summarize_by, variables_dt = variables_dt)
    
    # Merge by var and summarize var:
    allow_cartesian_setting = FALSE
    
    if (var_is_shared == TRUE) {
      allow_cartesian_setting = TRUE
    }
    
    cat_res = merge(var_dt_cat,
                    byvar_dt,
                    all.x = FALSE, all.y = FALSE,
                    allow.cartesian = allow_cartesian_setting)
    
    if (v_class %in% c("integer", "numeric")) {
      num_res = merge(var_dt_num,
                      byvar_dt,
                      all.x = FALSE, all.y = FALSE,
                      allow.cartesian = allow_cartesian_setting)
      
    }
    
    if (!v_class %in% c("integer", "numeric")) {
      num_res = NULL
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
