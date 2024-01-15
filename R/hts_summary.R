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
#'  
#' @return A data.table containing 
#' @export
#'
#' @examples
#'
#' require(data.table)
#' require(stringr)
#' require(dplyr)
#' require(srvyr)
#' hts_summary(hts_data = list('hh' = hh,
#'                             'person' = person,
#'                             'day' = day,
#'                             'trip' = trip,
#'                             'vehicle' = vehicle),
#'             summarize_var = 'age',
#'             summarize_by =  'employment',
#'             variables_dt = variable_list)
#' hts_summary(hts_data = list('hh' = hh,
#'                             'person' = person,
#'                             'day' = day,
#'                             'trip' = trip,
#'                             'vehicle' = vehicle),
#'             summarize_var = 'speed_mph',
#'             summarize_by =  'age',
#'             variables_dt = variable_list)

hts_summary = function(
    prepped_dt, 
    summarize_var,
    summarize_by,
    summarize_vartype = 'categorical',
    weighted = TRUE,
    se = FALSE,
    wtname = NULL,
    strataname = NULL) {
  
  # FIXME consider a labels = T/F argument here

  # For instances where num obs is singular inside a sub-strata, adjust:
  options(survey.lonely.psu = "adjust")
  
  if ( weighted & is.null(wtname)) {
    
    message("Weight not specified; setting weighted = FALSE")
    
    weighted = FALSE
    
  }

  if ( !weighted & se ){
    
    message("Standard errors require weighted data; setting se = FALSE. 
            Set weighted = TRUE and specify a wtname if standard errors are desired.")

    se = FALSE

    
  }

  cat_ns =  hts_get_ns(
    prepped_dt_ls[["cat"]],
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
      strataname = strataname
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
  
  return(summary)

}

## quiets concerns of R CMD check
utils::globalVariables(c("value_labels"))

