hts_summary_cat = function(prepped_dt,
                           summarize_var = NULL,
                           summarize_by = NULL,
                           weighted = TRUE,
                           se = FALSE,
                           wtname = NULL,
                           strataname = NULL,
                           variable_list = NULL,
                           values_dt = value_labels,
                           digits = 4) {
  # Using strata?
  if (!is.null(strataname)) {
    prepped_dt = hts_cbind_var(lhs_table = prepped_dt,
                               rhs_var = strataname,
                               variable_list = variable_list)
  }
  
  cat_so_ls = list()
  
  
  if (weighted == TRUE & se == TRUE) {
    cat_so_ls[["wtd"]] = hts_to_so(
      prepped_dt,
      strata = strataname,
      weighted = weighted)
  } else if (weighted == TRUE) {
    cat_so_ls[["wtd"]] = prepped_dt
  }
  
  cat_so_ls[["unwtd"]] = prepped_dt
  
  cat_summary_ls = list()
  
  groupbyvars = c(
    summarize_by,
    summarize_var,
    # if summarize_var is a checkbox var, include _checkbox_value col:
    "value"
  )
  
  groupbyvars = groupbyvars[groupbyvars %in% names(prepped_dt)]
  
  for (wt_type in names(cat_so_ls)) {
    
    # Set the variance type:
    variance_type = NULL
    
    if (wt_type == "wtd") {
      
      
      if (se){
        
        variance_type = "se"
        
        # tictoc::tic()
        # Calculate survey proportions & estimates:
        
        weight_name = hts_get_keycols(prepped_dt,
                                      weights = TRUE,
                                      id = FALSE,
                                      priority = TRUE)
        
        cat_summary_w =
          cat_so_ls[["wtd"]] %>%
          group_by_at(unlist(groupbyvars)) %>%
          summarize(
            count = length(get(summarize_var)),
            prop =  round(survey_prop(
              proportion = FALSE, 
              vartype = variance_type
            ), 3),
            est =   round(survey_total(vartype = variance_type))
          ) %>%
          setDT()
        # tictoc::toc()
      } else {
        
        weight_name = hts_get_keycols(prepped_dt,
                                      weights = TRUE,
                                      id = FALSE,
                                      priority = TRUE)
        
        
        cat_summary_w = cat_so_ls[["wtd"]][,.(
          count = .N,
          est = round(
            sum(
              get(weight_name)
            )
          )
        ),
        groupbyvars
        ]
        
        if (is.null(summarize_by)){
          
          cat_summary_w[, prop := round(est/ sum(est), digits)]
          
        } else {
          
          cat_summary_w[, prop := round(est/ sum(est), digits), summarize_by]
          
        }
        
        setcolorder(cat_summary_w, c(groupbyvars, 'count', 'prop', 'est'))
        
      }
      
    } else {

      cat_summary_w = cat_so_ls[["unwtd"]][, .(count = .N), groupbyvars]
      
      if (is.null(summarize_by)){
        
        cat_summary_w[, prop := round(count/ sum(count), digits)]
        
      } else {
        
        cat_summary_w[, prop := round(count/ sum(count), digits), summarize_by]
        
      }
      
      setcolorder(cat_summary_w, c(groupbyvars, 'count', 'prop'))
      
    }
    
    # Only get "selected" rows: 
    if("value" %in% groupbyvars){
      
      cat_summary_w = cat_summary_w[value == 1]
      cat_summary_w[, value := NULL]
      
      # determine if checkbox for ordering later
      is_checkbox = TRUE
      
      # recalculate prop without value == 0
      if (is.null(summarize_by)){
        
        cat_summary_w[, prop := round(count/ sum(count), digits)]
        
      } else {
        
        cat_summary_w[, prop := round(count/ sum(count), digits), summarize_by]
        
      }
      
    } else{
      
      is_checkbox = FALSE
      
    }
    
    cat_summary_w = factorize_df(
      df = cat_summary_w,
      vals_df = values_dt,
      variable_colname = "variable",
      value_colname = "value",
      value_label_colname = "label",
      value_order_colname = "val_order",
      verbose = FALSE
    )
    
    # Skip reordering if var is a checkbox
    if(!is_checkbox){
      
      if (is.null(summarize_by)){
        
        cat_summary_w = cat_summary_w[order(get(groupbyvars[1]))]
        
      } else {
        
        cat_summary_w = cat_summary_w[order(
          get(groupbyvars[1]),
          get(groupbyvars[2])
          )
        ]
        
      }
    }
    
    if(is_checkbox & !is.null(summarize_by)){
      
      cat_summary_w = cat_summary_w[order(get(groupbyvars[1]),
                                          get(groupbyvars[2]))]
      
    }
    
    cat_summary_ls[[wt_type]] = cat_summary_w[]
    
    if (wt_type == 'wtd'){
      
      cat_summary_ls$weight_name = weight_name
      
    }
    
  }
  
  return(cat_summary_ls)
  
}
