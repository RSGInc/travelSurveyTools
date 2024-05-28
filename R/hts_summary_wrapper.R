#' Make household travel survey summaries-- runs hts_prep_variable and hts_summary
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
#' @param trip_name Name of the trip dataset in hts_data.
#' @param day_name Name of the day dataset in hts_data.
#' @param strataname  Name of strata name to bring in. Default is NULL.
#' @param se Whether to calculate standard error. Default is FALSE. Will be set
#' to FALSE if weighted is FALSE.
#' @param wtname Name of the weight column to use. Default is NULL. Must be specified
#' when weighted = TRUE.
#' @param checkbox_valname Name of the column with the checkbox value. Default is 'value'.
#'  Must be provided if summarize_var is a checkbox variable.
#' @param checkbox_yesval Value of checkbox_valname that indicates it was selected.
#'  Default is 1. Must be provided if summarize_var is a checkbox variable.
#' @param remove_outliers Whether to remove outliers for numeric variable. Default
#'  is TRUE.
#' @param threshold Threshold to define outliers. Default is 0.975.
#' @param remove_missing Whether to remove missing values from the summary.
#'  Default is TRUE.
#' @param not_imputable Value representing 'Not imputable' to remove. Default
#'  is -1.
#' @param missing_values Missing values to remove. Default is 995.
#'
#' @return A list containing (if applicable) categorical and numeric summaries of the
#'  specified variable(s), as well as sample sizes and whether or not the summarized
#'  variable is a shared checkbox variable.
#'  To access the categorical/numeric df use output$summary.
#'  To access the weighted df use output$summary$wtd, and output$summary$unwtd for the
#'  unweighted df.
#'  To access the weight name use output$summary$weight_name.
#'  To access sample sizes use output$n_ls.
#'  To access weighted and unweighted sample sizes respectively, use output$n_ls$wtd
#'  and output$n_ls$unwtd.
#' @export
#'
#' @examples
#'
#'
#' hts_summary_wrapper(
#' summarize_var = 'employment',
#' summarize_by = 'age',
#' wtname = 'person_weight')
#' 
#' 
#' hts_summary_wrapper(
#' summarize_var = 'race',
#' summarize_by = c('age', 'employment'),
#' wtname = 'person_weight',
#' )
#' 
#' hts_summary_wrapper(
#' summarize_var = 'num_trips',
#' summarize_by = 'age',
#' wtname = 'person_weight')
#' 
#' 


hts_summary_wrapper = function(
    summarize_var = NULL,
    summarize_by = NULL,
    variables_dt = variable_list,
    data = list(
      "hh" = hh,
      "person" = person,
      "day" = day,
      "trip" = trip,
      "vehicle" = vehicle
    ),
    id_cols = c("hh_id", "person_id", "day_id", "trip_id", "vehicle_id"),
    weighted = TRUE,
    wt_cols = c("hh_weight", "person_weight", "day_weight", "trip_weight", "hh_weight"),
    trip_name = "trip",
    day_name = "day",
    strataname = NULL,
    se = FALSE,
    wtname = NULL,
    checkbox_valname = "value",
    checkbox_yesval = 1,
    remove_outliers = TRUE,
    threshold = 0.975,
    remove_missing = TRUE,
    not_imputable = -1,
    missing_values = c("Missing Response", "995")
){
  
  
  # Decide what prep function to run
  if (summarize_var != 'num_trips'){
    
    prepped_dt_ls = hts_prep_variable(
      summarize_var = summarize_var,
      summarize_by = summarize_by,
      variables_dt = variables_dt,
      data = data,
      id_cols = id_cols,
      weighted = weighted,
      wt_cols = wt_cols,
      remove_outliers = remove_outliers,
      threshold = threshold,
      remove_missing = remove_missing,
      missing_values = missing_values,
      not_imputable = not_imputable,
      strataname = strataname
    ) 
    
  } else {
    
    prepped_dt_ls = hts_prep_triprate(
      summarize_by = summarize_by,
      variables_dt = variable_list,
      trip_name = trip_name,
      day_name = day_name,
      ids = id_cols,
      wts = wt_cols,
      remove_outliers = remove_outliers,
      threshold = threshold,
      weighted = weighted,
      hts_data = data
    )
    
  }
  
  # If a checkbox variable use chexkbox for summarize_vartype
  if (variable_list[shared_name == summarize_var, .N] > 1){
    
    summarize_vartype = 'checkbox'
    
  } else {
    
    summarize_vartype = 'categorical'
    
  }
  
  prepped_dt = prepped_dt_ls$cat
  
  # if we prepped a triprate rename summarize_var for hts_summary
  if (summarize_var == 'num_trips'){
    
    
    if (weighted){
      
      # summarize_var = 'num_trips_wtd'
      
      setnames(prepped_dt, 'num_trips_wtd', 'num_trips')
      
    } else {
      
      # summarize_var = 'num_trips_unwtd'
      
      setnames(prepped_dt, 'num_trips_unwtd', 'num_trips')
      
    }
    
  }
  
  # run hts_summary
  output_ls_cat = hts_summary(
    prepped_dt,
    summarize_var = summarize_var,
    summarize_by = summarize_by,
    summarize_vartype = summarize_vartype,
    id_cols = id_cols,
    weighted = weighted,
    se = se,
    wtname = wtname,
    strataname = strataname,
    checkbox_valname = checkbox_valname,
    checkbox_yesval = checkbox_yesval
  )
  
  if (!is.null(prepped_dt_ls$num)){
    
    prepped_dt = prepped_dt_ls$num
    
    # if we prepped a triprate rename summarize_var for hts_summary
    if (summarize_var == 'num_trips'){
      
      
      if (weighted){
        
        # summarize_var = 'num_trips_wtd'
        
        setnames(prepped_dt, 'num_trips_wtd', 'num_trips')
        
      } else {
        
        # summarize_var = 'num_trips_unwtd'
        
        setnames(prepped_dt, 'num_trips_unwtd', 'num_trips')
        
      }
      
      
    }
    
    
    output_ls_num = hts_summary(
      prepped_dt,
      summarize_var = summarize_var,
      summarize_by = summarize_by,
      summarize_vartype = 'numeric',
      id_cols = id_cols,
      weighted = weighted,
      se = se,
      wtname = wtname,
      strataname = strataname,
      checkbox_valname = checkbox_valname,
      checkbox_yesval = checkbox_yesval
    )
  } else {
    
    output_ls_num = NULL
    
  }
  
  output_ls = list(
    'cat' = output_ls_cat,
    'num' = output_ls_num
  )
  
  
  return(output_ls)
  
  
}

## quiets concerns of R CMD check
utils::globalVariables(c("hts_data", "is_checkbox", "data_type", "old_weight"))