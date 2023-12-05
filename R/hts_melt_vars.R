hts_melt_vars = function(shared_name = NULL,
                         wide_dt = NULL,
                         shared_name_vars = NULL,
                         variables_dt = variable_list,
                         hts_data = hts_data,
                         remove_missing = TRUE,
                         missing_values = c("Missing Response", "995"),
                         checkbox_label_sep = ":",
                         to_single_row = FALSE) {
  
  if(is.null(wide_dt)){
    
    # Find location of summary variable:
    var_location = hts_find_var(shared_name, variables_dt = variables_dt)
    
    # Select table where this variable lives:
    wide_dt = hts_data[[var_location]]
    
  }
  
  if(is.null(shared_name_vars)){
    
    sn = shared_name
    shared_name_vars = variables_dt[shared_name == sn, variable]

  }
  
  melted_dt = data.table::melt(
    wide_dt,
    id.vars = hts_get_keycols(wide_dt),
    measure.vars = shared_name_vars,
    variable.name = "variable",
    value.name = "value"
  )
  
  if (remove_missing == TRUE) {
    
    melted_dt = melted_dt[!value %in% missing_values]
    
  }
  
  # append description:
  melted_dt[variables_dt, description := i.description, on = .(variable = variable)]
  
  # If condensing multiple checkboxes to single response:
  if (to_single_row) {
    
    sn = shared_name
    
    # shared_description:
    shared_description = stringr::str_split_i(variables_dt[shared_name == sn, description][1],
                                              pattern = checkbox_label_sep,
                                              i = 1)
    
    shared_description_twoormore = paste0(shared_description, ": ", "Two or more")
    
    # two or more checked:
    melted_dt[, num_checked := sum(value),
             by =  c(hts_get_keycols(wide_dt))]
    
    # make factor levels
    melted_dt$description = factor(melted_dt$description, levels = unique(melted_dt$description))
    
    if(melted_dt[num_checked >= 2, .N] > 0){
      
      bottom_levels = levels(melted_dt$description)[levels(melted_dt$description) %like% 'Prefer not|Prefers not|Other']
      
      top_levels = levels(melted_dt$description)[!levels(melted_dt$description) %in% bottom_levels]
      
      melted_dt[num_checked >= 2,
                description := shared_description_twoormore]
      
      melted_dt$description = factor(melted_dt$description, levels = c(top_levels, shared_description_twoormore, bottom_levels))
      
    }
    
    melted_dt[, num_checked := NULL]
    
    melted_dt = melted_dt[value == 1]
    
    melted_dt[, `:=` (variable = NULL, value = NULL)]
    
    melted_dt = unique(melted_dt)

  }
  
  # remove everything before checkbox_label_sep from description
  if(length(levels(melted_dt$description)) != 0){
    
    levels = gsub(str_glue(".*{checkbox_label_sep} "), "", levels(melted_dt$description))
  
    melted_dt[, description := gsub(str_glue(".*{checkbox_label_sep} "), "", description)]
    
    melted_dt$description = factor(melted_dt$description, levels = levels)
    
  } else{
    
    melted_dt[, description := gsub(str_glue(".*{checkbox_label_sep} "), "", description)]
    
  }
  
  data.table::setnames(melted_dt,
                       old = "description",
                       new = shared_name,
                       skip_absent = TRUE)
  
  return(melted_dt)
  
}
