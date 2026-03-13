#' Make household travel survey summaries-- runs hts_prep_variable and hts_summary
#' @param summarize_var Name of the variable to summarize. Default is NULL
#' @param summarize_by Name of the variable to summarize the summarize_var by.
#'  Default is NULL.
#' @param variables_dt List of variable locations and descriptions in data.table
#'  format.
#' @param vals_df A dataframe of variable labels (i.e., factor levels and
#'  labels) with the format as specified below. Passed to factorize_column
#'  function.
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
#' @param checkbox_valname Name of the column with the checkbox value. Default is 'value'.
#'  Must be provided if summarize_var is a checkbox variable.
#' @param checkbox_yesval Value of checkbox_valname that indicates it was selected.
#'  Default is 1. Must be provided if summarize_var is a checkbox variable.
#' @param value_label_colname The name of the value label column in vals_df. Default is 'value_label'
#' @param remove_outliers Whether to remove outliers for numeric variable. Default
#'  is TRUE.
#' @param threshold Threshold to define outliers. Default is 0.975.
#' @param remove_missing Whether to remove missing values from the summary.
#'  Default is TRUE.
#' @param not_imputable Value representing 'Not imputable' to remove. Default
#'  is -1.
#' @param missing_values Missing values to remove. Default is 995.
#'
#' @return A structured list with three top-level elements:
#'  \describe{
#'    \item{meta}{Summary metadata, including `target` information for the
#'    summarized variable, `group_by` variables, `source_tables`, and survey
#'    `design` fields such as weight and strata variables.}
#'    \item{diagnostics}{Wrapper-level diagnostic information, currently
#'    including sample sizes in `n_ls` plus `notes` and `warnings`.}
#'    \item{summaries}{Computed summary outputs. `summaries$categorical`
#'    contains the categorical summary payload, and `summaries$numeric`
#'    contains the numeric summary payload when available.}
#'  }
#'
#'  The internal structure of `summaries$categorical` and
#'  `summaries$numeric` is unchanged from the existing summary helpers.
#' @export
#'
#' @examples
#'
#'
#' hts_summary_wrapper(
#' summarize_var = 'employment',
#' summarize_by = 'income_detailed')
#' 
#' 
#' hts_summary_wrapper(
#' summarize_var = 'race',
#' summarize_by = c('age', 'employment')
#' )
#' 
#' hts_summary_wrapper(
#' summarize_var = 'num_trips',
#' summarize_by = 'age')
#' 
#' 

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return(y)
  }

  x
}

hts_wrapper_meta_value <- function(var_rows, col_name, default = NULL) {
  if (!col_name %in% names(var_rows)) {
    return(default)
  }

  value <- var_rows[[col_name]][!is.na(var_rows[[col_name]])][1]
  value %||% default
}

hts_wrapper_meta <- function(
    summarize_var,
    summarize_by,
    variables_dt,
    data,
    day_name,
    weight_var,
    strataname
) {
  var_rows <- data.table::copy(
    variables_dt[shared_name == summarize_var | variable == summarize_var]
  )

  meta_tables <- unique(stats::na.omit(vapply(
    c(summarize_var, summarize_by %||% character()),
    FUN = function(x) {
      if (identical(x, "num_trips")) {
        return(day_name)
      }

      hts_find_var(x, data = data, variables_dt = variables_dt)
    },
    FUN.VALUE = character(1)
  )))

  list(
    target = list(
      variable = summarize_var,
      variable_label = hts_wrapper_meta_value(var_rows, "label"),
      question_text = hts_wrapper_meta_value(var_rows, "question_text"),
      variable_description = hts_wrapper_meta_value(var_rows, "description"),
      variable_logic = hts_wrapper_meta_value(var_rows, "logic"),
      data_type = hts_wrapper_meta_value(var_rows, "data_type"),
      shared_name = hts_wrapper_meta_value(var_rows, "shared_name", summarize_var),
      is_checkbox = isTRUE(var_rows$is_checkbox[1] == 1)
    ),
    group_by = list(
      variables = summarize_by %||% character()
    ),
    source_tables = meta_tables,
    design = list(
      weight_var = weight_var %||% NULL,
      psu_var = NULL,
      strata_var = strataname %||% NULL
    )
  )
}


hts_summary_wrapper = function(
    summarize_var = NULL,
    summarize_by = NULL,
    variables_dt = variable_list,
    vals_df = value_labels,
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
    checkbox_valname = "value",
    checkbox_yesval = 1,
    value_label_colname = 'label',
    remove_outliers = TRUE,
    threshold = 0.975,
    remove_missing = TRUE,
    not_imputable = -1,
    missing_values = c("Missing Response", "995")
){
  variables_dt = hts_validate_variable_list(variables_dt, data)
  
  
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
      variables_dt = variables_dt,
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
  
  # If a checkbox variable use checkbox for summarize_vartype
  if (variables_dt[shared_name == summarize_var, .N] > 1){
    
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
  
  
  # Determine what weight to use
  if (summarize_var == 'num_trips'){
    
    weight = 'day_weight'
    
  } else {
  
  id_counts = prepped_dt[, lapply(.SD, uniqueN),
                         .SDcols = intersect(id_cols, names(prepped_dt))]
  
  max_id = which.max(id_counts)
  
  weight = wt_cols[[max_id]]
  
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
    wtname = weight,
    strataname = strataname,
    checkbox_valname = checkbox_valname,
    checkbox_yesval = checkbox_yesval
  )
  
  #return variables used
  output_ls_cat$summarize_var = summarize_var
  output_ls_cat$summarize_by = summarize_by
  
  if (!is.null(output_ls_cat$summary$wtd)){
    
    output_ls_cat$summary$wtd = factorize_df(
      output_ls_cat$summary$wtd,
      vals_df = vals_df,
      value_label_colname = value_label_colname,
      verbose = FALSE
    )
    
    
  }
  
  output_ls_cat$summary$unwtd = factorize_df(
    output_ls_cat$summary$unwtd,
    vals_df = vals_df,
    value_label_colname = value_label_colname,
    verbose = FALSE
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
      wtname = weight,
      strataname = strataname,
      checkbox_valname = checkbox_valname,
      checkbox_yesval = checkbox_yesval
    )
    
    #return variables used
    output_ls_cat$summarize_var = summarize_var
    output_ls_cat$summarize_by = summarize_by
    
    if (!is.null(output_ls_num$summary$wtd)){
      
      output_ls_num$summary$wtd = factorize_df(
        output_ls_num$summary$wtd,
        vals_df = vals_df,
        value_label_colname = value_label_colname,
        verbose = FALSE
      )
      
      
    }
    
    output_ls_num$summary$unwtd = factorize_df(
      output_ls_num$summary$unwtd,
      vals_df = vals_df,
      value_label_colname = value_label_colname,
      verbose = FALSE
    )
    
    
    
  } else {
    
    output_ls_num = NULL
    
  }
  
  output_ls = list(
    'cat' = output_ls_cat,
    'num' = output_ls_num
  )
  
  obj = list(
    meta = hts_wrapper_meta(
      summarize_var = summarize_var,
      summarize_by = summarize_by,
      variables_dt = variables_dt,
      data = data,
      day_name = day_name,
      weight_var = weight,
      strataname = strataname
    ),
    diagnostics = list(
      n_ls = output_ls_cat$n_ls %||% output_ls_num$n_ls %||% NULL,
      notes = character(),
      warnings = character()
    ),
    summaries = list(
      categorical = output_ls_cat,
      numeric = output_ls_num
    )
  )

  return(obj)
  
  
}

## quiets concerns of R CMD check
utils::globalVariables(c("hts_data", "is_checkbox", "data_type", "old_weight"))
