#' Remove missing data for summary variables
#'
#' @param hts_data List containing household, person, day, trip, and vehicle
#'  datasets in data.table format.
#' @param variables_dt A variable list with descriptions and table locations
#'  of variables.
#' @param summarize_var Variable to be summarized that has it's missing data
#'  removed.
#' @param summarize_by Variable being summarized by that has it's missing data
#'  removed. Default is NULL.
#' @param missing_value Missing value that will be removed. Default is 995.
#' @param not_imputable Value meaning not_imputable that will be removed. Default
#'  is -1.
#'
#' @return Inputted list of datasets without missing values for specified variables.
#' @export
#'
#' @examples
#'
#' require(data.table)
#' hts_remove_missing_data(hts_data = list('hh' = hh,
#'                                         'person' = person,
#'                                         'day' = day,
#'                                         'trip' = trip,
#'                                         'vehicle' = vehicle),
#'                         variables_dt = variable_list,
#'                         summarize_var = 'num_people',
#'                         summarize_by = 'mode_type')
#'
hts_remove_missing_data = function(hts_data,
                                   variables_dt,
                                   summarize_var,
                                   summarize_by = NULL,
                                   missing_value = 995,
                                   not_imputable = -1){

  summarize_var_loc = hts_find_var(summarize_var)

  #get variable or first occurrence for checkbox
  summarize_var_name = variables_dt[shared_name == summarize_var, variable][1]

  summarize_var_tbl = hts_data[[summarize_var_loc]][
    !get(summarize_var_name) %in% c(missing_value, not_imputable) |
      is.na(get(summarize_var_name))]

  summarize_var_id = hts_get_keycols(summarize_var_tbl,
                                     ids = TRUE,
                                     weights = FALSE,
                                     priority = TRUE)

  hts_data = hts_filter_data(
    hts_data = hts_data,
    ids = summarize_var_tbl[, get(summarize_var_id)],
    id_type = stringr::str_remove(summarize_var_id, '_id')
  )

  if (!is.null(summarize_by)){

    summarize_by_loc = hts_find_var(summarize_by)

    #get variable or first occurrence for checkbox
    summarize_by_name = variables_dt[shared_name == summarize_by, variable][1]

    summarize_by_tbl = hts_data[[summarize_by_loc]][
      !get(summarize_by_name) %in% c(missing_value, not_imputable) |
        is.na(get(summarize_by_name))]

    summarize_by_id = hts_get_keycols(summarize_by_tbl,
                                      ids = TRUE,
                                      weights = FALSE,
                                      priority = TRUE)

    hts_data = hts_filter_data(
      hts_data = hts_data,
      ids = summarize_by_tbl[, get(summarize_by_id)],
      id_type = stringr::str_remove(summarize_by_id, '_id')
    )

  }

  return(hts_data)

}
