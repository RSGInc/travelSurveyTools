#' Prepare variable to be summarized
#'
#' @param summarize_by Name of the variable to be prepped. Default is NULL.
#' @param variables_dt List of variable locations and descriptions in data.table
#' format.
#' @param ... Additional parameters to pass to \code{link{hts_melt_vars}}
#' 
#' @return Data table containing the variable to be summarized and other key
#' columns.
#' @export
#'
#' @examples
#' set.seed(45)
#' require(data.table)
#' require(stringr)
#' person = data.table(
#'               hh_id = sample(1:10, size = 20, replace = TRUE),
#'               person_id = 1:20,
#'               race_1 = sample(0:1, size = 20, replace = TRUE),
#'               race_2 = sample(0:1, size = 20, replace = TRUE),
#'               age = sample(1:10, size = 20, replace = TRUE))
#' hts_data = list(person = person)
#' variable_list = data.table(
#'       variable = c('race_1', 'race_2', 'age'),
#'       hh = c(0,0,0),
#'       person = c(1,1,1),
#'       vehicle = c(0,0,0),
#'       day = c(0,0,0),
#'       trip = c(0,0,0),
#'       shared_name = c('race', 'race', 'age'),
#'       description = c('Race: White', 'Race: Asian', 'Age'),
#'       is_checkbox = c(1,1,0),
#'       data_type = rep('integer/categorical', 3)) 
#' hts_prep_byvar(summarize_by = 'age', variables_dt = variable_list)
#' hts_prep_byvar(summarize_by = 'race', variables_dt = variable_list)
#'
hts_prep_byvar = function(summarize_by = NULL,
                          variables_dt = variables_list,
                          ...) {
  # For each variables in trip table:
  byvar_dt_ls = list()
  
  for (b in seq_along(summarize_by)) {
    byvar = summarize_by[[b]]
    
    byvar_loc = hts_find_var(byvar, variables_dt = variables_dt)
    
    byvar_dt_v = data.table::copy(get(as.character(byvar_loc)))
    
    # Is this a shared variable?
    byvar_is_shared = variables_dt[shared_name == byvar, is_checkbox][1] == 1
    
    # Is this a numeric variable?
    byvar_is_numeric = variables_dt[shared_name == summarize_by[[b]], data_type][[1]] == "numeric"
    
    if (byvar_is_shared) {
      
      byvar_dt_v =
        hts_melt_vars(
          shared_name = summarize_by[[b]],
          wide_dt = byvar_dt_v,
          shared_name_vars = NULL,
          variables_dt = variable_list,
          remove_missing = TRUE,
          missing_values = c("Missing Response", "995"),
          checkbox_label_sep = ":",
          to_single_row = TRUE
        )
      
    }
    
    if (byvar_is_numeric) {
      byvar_dt_v = hts_bin_var(prepped_dt = byvar_dt_v,
                               numvar = byvar,
                               nbins = 7)
    }
    
    if (!byvar_is_shared) {
      byvar_cols = c(hts_get_keycols(byvar_dt_v), byvar)
      
      byvar_dt_v = byvar_dt_v[, ..byvar_cols]
      
    }
    
    byvar_dt_ls[[b]] = byvar_dt_v
    
  }
  
  byvar_dt = Reduce(function(x, y)
    merge(x, y, all.x = FALSE, all.y = FALSE),
    byvar_dt_ls)
  
  return(byvar_dt[])
}
