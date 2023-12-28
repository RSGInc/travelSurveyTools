#' Make household travel survey summaries
#'
#' @param hts_data List containing household, person, day, trip, and vehicle 
#' datasets in data.table format.
#' @param summarize_var Name of the variable to summarize. Default is NULL.
#' @param summarize_by Name of the variable to summarize the summarize_var by.
#' Default is NULL.
#' @param variables_dt Dataset of variable locations and descriptions in
#' data.table format.
#' @param var_description_sep Character used in the desciption of the variable
#' list to separate the name from the description. Default is ':'.
#' @param values_dt Dataset of values and value labels for all variables in
#' data.table format.
#' @param weighted Whether the data is weighted. Default is TRUE.
#' @param wtname Name of the weight column to use. Default is NULL.
#' @param strataname  Name of strata name to bring in. Default is NULL.
#' @param remove_outliers Whether to remove outliers from the data. Default is
#' TRUE.
#' @param se Whether to calculate standard error. Default is FALSE.
#' @param threshold Threshold to define an outlier. Default is 0.975.
#' @param remove_missing Whether to remove missing values from the summary.
#' Default is TRUE.
#' @param missing_value Missing value to remove. Default is 995.
#' @param not_imputable Value respresenting 'Not imputable' to remove. Default
#' is -1.
#' 
#' @return List containing sample sizes, categorical summary, numeric summary,
#' outlier breakdown, whether or not the summarized variable is a checkbox,
#' and the summary variables and their descriptions.
#' @export
#'
#' @examples
#' set.seed(45)
#' require(data.table)
#' require(stringr)
#' require(dplyr)
#' require(srvyr)
#' hh = data.table(
#'               hh_id = 1:10)
#' person = data.table(
#'               hh_id = rep(1:10,2),
#'               person_id = 1:20,
#'               age = sample(1:3, size = 20, replace = TRUE),
#'               employment = sample(1:2, size = 20, replace = TRUE),
#'               num_jobs = sample(1:5, size = 20, replace = TRUE),
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
#' vehicle = data.table(
#'               hh_id = 1:10,
#'               vehicle_id = 1:10)
#' hts_data = list(hh = hh,person = person, day = day, trip = trip, vehicle = vehicle)
#' variable_list = data.table(
#'       variable = c('age', 'employment', 'num_jobs'),
#'       hh = c(0,0,0),
#'       person = c(1,1,1),
#'       vehicle = c(0,0,0),
#'       day = c(0,0,0),
#'       trip = c(0,0,0),
#'       shared_name = c('age', 'employment', 'num_jobs'),
#'       description = c('Age', 'Employment status', 'Number of jobs'),
#'       is_checkbox = c(0, 0, 0),
#'       data_type = c('integer/categorical', 'integer/categorical', 'numeric'))
#' value_labels = data.table(
#'       variable = c(rep('age', 3), rep('employment', 2)),
#'       value = c(1,2,3,1,2),
#'       label = c('Under 30', '30-65', 'Over 65', 'Employed', 'Unemployed'),
#'       val_order = c(1,2,3,4,5))
#' hts_summary(hts_data = hts_data, summarize_var = 'age', summarize_by = 
#' 'employment', variables_dt = variable_list)
#' hts_summary(hts_data = hts_data, summarize_var = 'num_jobs', summarize_by = 
#' 'age', variables_dt = variable_list)
hts_summary = function(
    hts_data = list('hh' = hh,
                    'person' = person,
                    'day' = day,
                    'trip' = trip,
                    'vehicle' = vehicle,
                    'location' = location),
    summarize_var = NULL,
    summarize_by = NULL,
    variables_dt = variable_list,
    var_description_sep = ":",
    values_dt = value_labels,
    weighted = TRUE,
    wtname = NULL,
    strataname = NULL,
    remove_outliers = TRUE,
    se = FALSE,
    threshold = 0.975,
    remove_missing = TRUE,
    missing_value = 995,
    not_imputable = -1) {

  # TODO: Add filter values (e.g., missing)
  # TODO: Ability to filter variables from shared_name checkbox variables (e.g., "none of the above")
  # TODO: Ability to specify hh/person/trip/etc. tables so that custom subsetting can occur outside of function

  # tictoc::tic("Total Time")
  # Message:
  msg_pt1 = paste0("Creating a summary of ",
                   hts_find_var(summarize_var, variables_dt = variables_dt), " ", summarize_var)


  if (!is.null(summarize_by)){

    byvarlocs = lapply(summarize_by, hts_find_var)

    for(b in 1:length(byvarlocs)) {
      byvarlocs[b] = paste0(byvarlocs[[b]], " ", summarize_by[[b]])
    }

    byvarlocs = unlist(byvarlocs)

    msg_pt2 = ifelse(length(summarize_by) > 0,
                     paste0("broken down by ",
                            paste0(byvarlocs, collapse = " and ")),
                     "")
  } else {
    msg_pt2 = NULL
  }
  message(paste0(msg_pt1, " ", msg_pt2))

  # For instances where num obs is singular inside a sub-strata, adjust:
  options(survey.lonely.psu = "adjust")

  #remove missing response data

  if (remove_missing){

    hts_data = hts_remove_missing_data(hts_data = hts_data,
                                       variables_dt = variables_dt,
                                       summarize_var = summarize_var,
                                       summarize_by = summarize_by,
                                       missing_value = missing_value,
                                       not_imputable = not_imputable)
  }

  # tictoc::tic("!num_trips -> hts_prep_data")

  if(summarize_var == "num_trips"){

    prepped_dt_ls = hts_prep_triprate(
      summarize_by = summarize_by,
      remove_outliers = remove_outliers,
      threshold = threshold,
      trip = hts_data$trip,
      day = hts_data$day
    )

  }

  if(!summarize_var == "num_trips"){

    prepped_dt_ls = hts_prep_data(
      summarize_var = summarize_var,
      summarize_by = summarize_by,
      remove_outliers = remove_outliers,
      threshold = threshold,
      data = hts_data,
      variables_dt = variables_dt
    )

  }


  # tictoc::toc(log = TRUE)

  # Counts:
  cat_ns =  hts_get_ns(
    prepped_dt_ls[["cat"]],
    weighted = weighted
  )

  # tictoc::tic("!num_trips -> hts_summary_cat")

  if ( weighted == FALSE & se == TRUE){

    message("Standard errors require weighted data; setting se = FALSE")

    se = FALSE

  }


  # Summary:
  cat_summary = hts_summary_cat(
    prepped_dt = prepped_dt_ls[["cat"]],
    summarize_var = summarize_var,
    summarize_by = summarize_by,
    weighted = weighted,
    wtname = wtname,
    strataname = strataname,
    values_dt = values_dt,
    se = se
  )

  # tictoc::toc(log = TRUE)

  if (!is.null(prepped_dt_ls[["num"]])) {
    num_summary = hts_summary_num(
      prepped_dt = prepped_dt_ls[["num"]],
      summarize_var = summarize_var,
      summarize_by = summarize_by,
      weighted = weighted,
      wtname = wtname,
      strataname = strataname,
      values_dt = values_dt
    )

  }

  if (is.null(prepped_dt_ls[["num"]])) {
    num_summary = NULL
  }

  # Get variable descriptions
  summarize_var_desc = variables_dt[shared_name == summarize_var, description]

  if(length(summarize_var_desc) > 1){
    summarize_var_desc = unique(tstrsplit(summarize_var_desc, var_description_sep, keep = 1)[[1]])
  }

  if(!is.null(summarize_by)){
    summarize_by_desc = variables_dt[shared_name == summarize_by, description]

    if(length(summarize_by_desc) > 1){
      summarize_by_desc = unique(tstrsplit(summarize_by_desc, var_description_sep, keep = 1)[[1]])
    }
  } else if (is.null(summarize_by)){
    summarize_by_desc = NULL
  }


  summary_ls = list(
    "n_ls" = cat_ns,
    "cat_summary" = cat_summary,
    "num_summary" = num_summary,
    'outliers' = prepped_dt_ls[['outliers']],
    "var_is_shared" = prepped_dt_ls[['var_is_shared']],
    "summarize_var" = summarize_var,
    "summarize_var_desc" = summarize_var_desc,
    "summarize_by" = summarize_by,
    "summarize_by_desc" = summarize_by_desc
  )


  return(summary_ls)

}
