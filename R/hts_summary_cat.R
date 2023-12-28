#' Summarize a categorical variable
#'
#' @param prepped_dt Dataset containing the summary variables and key columns in
#' data.table format.
#' @param summarize_var Name of the categorical variable to summarize. Default is NULL.
#' @param summarize_by Name of the variable to summarize the summarize_var by.
#' Default is NULL.
#' @param weighted Whether the data is weighted. Default is TRUE.
#' @param se Whether to calculate standard error. Default is FALSE.
#' @param wtname Name of the weight column to use. Default is NULL.
#' @param strataname  Name of strata name to bring in. Default is NULL.
#' @param variable_list Dataset of variable locations and descriptions in
#' data.table format.
#' @param values_dt Dataset of values and value labels for all variables in
#' data.table format.
#' 
#' @return List of unweighted and weighted categorical summaries including counts
#' and proportions.
#' @export
#'
#' @examples
#' set.seed(45)
#' require(data.table)
#' require(stringr)
#' require(dplyr)
#' require(srvyr)
#' person = data.table(
#'               hh_id = rep(1:10,2),
#'               person_id = 1:20,
#'               age = sample(1:3, size = 20, replace = TRUE),
#'               employment = sample(1:2, size = 20, replace = TRUE),
#'               person_weight = sample(1:10, size = 20, replace = TRUE))
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
#'       variable = c('age', 'employment'),
#'       hh = c(0,0),
#'       person = c(1,1),
#'       vehicle = c(0,0),
#'       day = c(0,0),
#'       trip = c(0,0),
#'       shared_name = c('age', 'employment'),
#'       description = c('Age', 'Employment status'),
#'       is_checkbox = c(0, 0),
#'       data_type = c('integer/categorical', 'integer/categorical'))
#' value_labels = data.table(
#'       variable = c(rep('age', 3), rep('employment', 2)),
#'       value = c(1,2,3,1,2),
#'       label = c('Under 30', '30-65', 'Over 65', 'Employed', 'Unemployed'),
#'       val_order = c(1,2,3,4,5))
#' DT = hts_prep_data(summarize_var = 'age', variables_dt = variable_list)$cat
#' hts_summary_cat(prepped_dt = DT, summarize_var = 'age', values_dt =
#' value_labels)
#' DT = hts_prep_data(summarize_var = 'age', summarize_by = 'employment', 
#' variables_dt = variable_list)$cat
#' hts_summary_num(prepped_dt = DT, summarize_var = 'age', summarize_by = 'employment',
#' values_dt = value_labels)
hts_summary_cat = function(prepped_dt,
                           summarize_var = NULL,
                           summarize_by = NULL,
                           weighted = TRUE,
                           se = FALSE,
                           wtname = NULL,
                           strataname = NULL,
                           variable_list = NULL,
                           values_dt = value_labels) {
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
            prop =  survey_prop(
              proportion = FALSE, 
              vartype = variance_type
            ),
            est =   survey_total(vartype = variance_type)
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
          est = 
            sum(
              get(weight_name)
            )
        ),
        groupbyvars
        ]
        
        if (is.null(summarize_by)){
          
          cat_summary_w[, prop := est/ sum(est)]
          
        } else {
          
          cat_summary_w[, prop := est/ sum(est), summarize_by]
          
        }
        
        setcolorder(cat_summary_w, c(groupbyvars, 'count', 'prop', 'est'))
        
      }
      
    } else {

      cat_summary_w = cat_so_ls[["unwtd"]][, .(count = .N), groupbyvars]
      
      if (is.null(summarize_by)){
        
        cat_summary_w[, prop := count/ sum(count)]
        
      } else {
        
        cat_summary_w[, prop := count/ sum(count), summarize_by]
        
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
        
        cat_summary_w[, prop := count/ sum(count)]
        
      } else {
        
        cat_summary_w[, prop := count/ sum(count), summarize_by]
        
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
