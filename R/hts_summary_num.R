hts_summary_num = function(prepped_dt,
                           summarize_var = NULL,
                           summarize_by = NULL,
                           weighted = TRUE,
                           values_dt = value_labels,
                           wtname = NULL,
                           strataname = NULL) {
  num_so_ls = list()
  
  num_so_ls[["unwtd"]] = srvyr::as_survey_design(prepped_dt, w = NULL)
  
  if (weighted == TRUE) {
    num_so_ls[["wtd"]] = hts_to_so(prepped_dt, strata = strataname)
  }
  
  
  num_summary_ls = list()
  
  for (w in seq_along(num_so_ls)) {
    wt_type = names(num_so_ls)[[w]]
    
    # Set the variance type:
    variance_type = "se"
    
    # Calculate survey proportions:
    num_summary_wttype =
      num_so_ls[[wt_type]] %>%
      group_by_at(unlist(summarize_by)) %>%
      summarize(
        count =   length(get(summarize_var)),
        min =     min(get(summarize_var), na.rm = TRUE),
        max =     max(get(summarize_var), na.rm = TRUE),
        mean =    survey_mean(get(summarize_var), vartype = variance_type, na.rm = TRUE),
        median =  survey_median(get(summarize_var), vartype = NULL, na.rm = TRUE)
      ) %>%
      setDT()
    
    num_summary_wttype = factorize_df(
      df = num_summary_wttype,
      vals_df = values_dt,
      variable_colname = "variable",
      value_colname = "value",
      value_label_colname = "label",
      value_order_colname = "val_order",
      verbose = FALSE
    )
    
    num_summary_ls[[wt_type]] = num_summary_wttype
    

  }
  
  return(num_summary_ls)
  
}