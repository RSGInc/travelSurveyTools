#' Make household travel survey summaries
#'
#' @param prepped_dt A prepared dataset in data.table format with 
#' the variable to summarize, the variable to summarize by, and the weights, 
#' if used.
#' @param summarize_var Name of the variable to summarize. Default is NULL.
#' @param summarize_by Name of the variable to summarize the summarize_var by.
#'  Default is NULL.
#' @param summarize_vartype String; one of either 'categorical' (when the 
#' variable being summarized is categorical), 'checkbox' (when the variable being
#' summarized is derived from a multiple response, aka select-all-that-apply question) 
#' or 'numeric', when the variable being summarized is numeric. 
#' @param weighted Whether the data is weighted. Default is TRUE.
#' @param se Whether to calculate standard error. Default is FALSE. Will be set 
#' to FALSE if weighted is FALSE.
#' @param wtname Name of the weight column to use. Default is NULL. Must be specified
#' when weighted = TRUE.
#' @param strataname  Name of strata name to bring in. Default is NULL.
#' @param checkbox_valname Name of the column with the checkbox value. Default is NULL.
#'  Must be provided if summarize_var is a checkbox variable.
#' @param checkbox_yesval Value of checkbox_valname that indicates it was selected.
#'  Default is NULL. Must be provided if summarize_var is a checkbox variable.
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
#' require(data.table)
#' require(stringr)
#' require(dplyr)
#' require(srvyr)
#' DT = hts_prep_data(summarize_var = 'age',
#'                    summarize_by = 'employment',
#'                    variables_dt = variable_list,
#'                    data = list('hh' = hh,
#'                                'person' = person,
#'                                'day' = day,
#'                                'trip' = trip,
#'                                'vehicle' = vehicle))$cat
#' output = hts_summary(prepped_dt = DT,
#'               summarize_var = 'age',
#'               summarize_by = 'employment',
#'               summarize_vartype = 'categorical',
#'               wtname = 'person_weight')
#'
#' DT = hts_prep_data(summarize_var = 'speed_mph',
#'                    summarize_by = 'age', 
#'                    variables_dt = variable_list,
#'                    data = list('hh' = hh,
#'                                'person' = person,
#'                                'day' = day,
#'                                'trip' = trip,
#'                                'vehicle' = vehicle))$num
#' output = hts_summary(prepped_dt = DT,
#'               summarize_var = 'speed_mph',
#'               summarize_by = 'age',
#'               summarize_vartype = 'numeric',
#'               wtname = 'trip_weight')
#'
#' DT = hts_prep_data(summarize_var = 'race',
#'                    summarize_by = 'age', 
#'                    variables_dt = variable_list,
#'                    data = list('hh' = hh,
#'                                'person' = person,
#'                                'day' = day,
#'                                'trip' = trip,
#'                                'vehicle' = vehicle))$cat
#' output = hts_summary(prepped_dt = DT,
#'               summarize_var = 'race',
#'               summarize_by = 'age',
#'               summarize_vartype = 'categorical',
#'               wtname = 'person_weight',
#'               checkbox_valname = 'value',
#'               checkbox_yesval = 1)             


hts_summary = function(
    prepped_dt, 
    summarize_var,
    summarize_by = NULL,
    summarize_vartype = 'categorical',
    weighted = TRUE,
    se = FALSE,
    wtname = NULL,
    strataname = NULL, 
    checkbox_valname = NULL,
    checkbox_yesval = NULL) {
  
  # FIXME consider a labels = T/F argument here

  # For instances where num obs is singular inside a sub-strata, adjust:
  options(survey.lonely.psu = "adjust")
  
  if ( summarize_vartype == 'checkbox' & 
       (is.null(checkbox_valname) | is.null(checkbox_yesval))) {
    
    stop("Must provide checkbox_valname and checkbox_yesval if summarize_vartype is checkbox.")
  }
  
  if ( weighted & is.null(wtname)) {
    
    message("Weight not specified; setting weighted = FALSE")
    
    weighted = FALSE
    
  }

  # Think we should put in hts_summary_num and hts_summary_cat instead
  # if ( !weighted & se ){
  #   
  #   message("Standard errors require weighted data; setting se = FALSE. 
  #           Set weighted = TRUE and specify a wtname if standard errors are desired.")
  # 
  #   se = FALSE
  # 
  #   
  # }

  cat_ns =  hts_get_ns(
    prepped_dt = prepped_dt,
    weighted = weighted
  )
  
  # something here to check if the number of unique values is more than 20
  # and summarize_vartype is categorical
  
  # if the variable to summarize is categorical, use cat_summary
  if (summarize_vartype %in% c('categorical', 'checkbox')) {
    summary = hts_summary_cat(
      prepped_dt = prepped_dt,
      summarize_var = summarize_var,
      summarize_by = summarize_by,
      weighted = weighted,
      se = se,
      wtname = wtname,
      strataname = strataname, 
      checkbox_valname = checkbox_valname,
      checkbox_yesval = checkbox_yesval
    )
  }

  if (summarize_vartype == 'numeric') {
    summary = hts_summary_num(
      prepped_dt = prepped_dt,
      summarize_var = summarize_var,
      summarize_by = summarize_by,
      weighted = weighted,
      se = se,
      wtname = wtname,
      strataname = strataname
    )
  }
  
  summary_ls = list(
    'n_ls' = cat_ns,
    'summary' = summary
  )
  return(summary_ls)

}

## quiets concerns of R CMD check
utils::globalVariables(c("value_labels"))

