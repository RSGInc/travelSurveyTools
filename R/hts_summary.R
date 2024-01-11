#' Make household travel survey summaries
#'
#' @param hts_data List containing household, person, day, trip, and vehicle
#'  datasets in data.table format.
#' @param summarize_var Name of the variable to summarize. Default is NULL.
#' @param summarize_by Name of the variable to summarize the summarize_var by.
#'  Default is NULL.
#' @param variables_dt Dataset of variable locations and descriptions in
#'  data.table format.
#' @param var_description_sep Character used in the description of the variable
#'  list to separate the name from the description. Default is ':'.
#' @param values_dt Dataset of values and value labels for all variables in
#'  data.table format.
#' @param weighted Whether the data is weighted. Default is TRUE.
#' @param wtname Name of the weight column to use. Default is NULL.
#' @param strataname  Name of strata name to bring in. Default is NULL.
#' @param remove_outliers Whether to remove outliers from the data. Default is
#'  TRUE.
#' @param se Whether to calculate standard error. Default is FALSE.
#' @param threshold Threshold to define an outlier. Default is 0.975.
#' @param remove_missing Whether to remove missing values from the summary.
#'  Default is TRUE.
#' @param missing_value Missing value to remove. Default is 995.
#' @param not_imputable Value representing 'Not imputable' to remove. Default
#'  is -1.
#'
#' @return List containing sample sizes, categorical summary, numeric summary,
#' outlier breakdown, whether or not the summarized variable is a checkbox,
#' and the summary variables and their descriptions.
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
      day = hts_data$day,
      hts_data = hts_data
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

## quiets concerns of R CMD check
utils::globalVariables(c("value_labels"))

