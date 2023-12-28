#' Summarize a numeric variable
#'
#' @param prepped_dt Dataset containing the summary variables and key columns in
#' data.table format.
#' @param summarize_var Name of the numeric variable to summarize. Default is NULL.
#' @param summarize_by Name of the variable to summarize the summarize_var by.
#' Default is NULL.
#' @param weighted Whether the data is weighted. Default is TRUE.
#' @param values_dt Dataset of values and value labels for all variables in
#' data.table format.
#' @param wtname Name of the weight column to use. Default is NULL.
#' @param strataname  Name of strata name to bring in. Default is NULL.
#' 
#' @return List of unweighted and weighted numeric summaries including count, min,
#' max, mean, se, and median.
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
#'               num_jobs = sample(1:10, size = 20, replace = TRUE),
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
#' value_labels = data.table(
#'       variable = rep('age', 3),
#'       value = c(1,2,3),
#'       label = c('Under 30', '30-65', 'Over 65'),
#'       val_order = c(1,2,3))
#' DT = hts_prep_data(summarize_var = 'num_jobs', variables_dt = variable_list)$num
#' hts_summary_num(prepped_dt = DT, summarize_var = 'num_jobs', values_dt =
#' value_labels)
#' DT = hts_prep_data(summarize_var = 'num_jobs', summarize_by = 'age', 
#' variables_dt = variable_list)$num
#' hts_summary_num(prepped_dt = DT, summarize_var = 'num_jobs', summarize_by = 'age',
#' values_dt = value_labels)
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