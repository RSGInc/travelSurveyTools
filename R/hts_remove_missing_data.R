#' Remove missing data for summary variables
#'
#' @param hts_data List containing household, person, day, trip, and vehicle
#'  datasets in data.table format.
#' @param variables_dt A variable list with descriptions and table locations
#'  of variables.
#' @param summarize_var Variable to be summarized that has it's missing data
#'  removed.
#' @param ids names of unique identifiers for each table in hts_data
#' @param summarize_by Variable being summarized by that has it's missing data
#'  removed. Default is NULL.
#' @param missing_values Missing values that will be removed. Defaults are 995 and
#'  'Missing Response'.
#' @param not_imputable Value meaning not_imputable that will be removed. Default
#'  is -1.
#'
#' @return Inputted list of datasets without missing values for specified variables.
#' @export
#'
#' @examples
#'
#' require(data.table)
#' hts_remove_missing_data(
#'   hts_data = list(
#'     "hh" = hh,
#'     "person" = person,
#'     "day" = day,
#'     "trip" = trip,
#'     "vehicle" = vehicle
#'   ),
#'   variables_dt = variable_list,
#'   summarize_var = "speed_mph",
#'   summarize_by = "mode_type"
#' )
#'
hts_remove_missing_data = function(hts_data,
                                    variables_dt,
                                    summarize_var,
                                    ids = c("hh_id", "person_id", "day_id", "trip_id", "vehicle_id"),
                                    summarize_by = NULL,
                                    missing_values = c("Missing Response", "995"),
                                    not_imputable = -1) {
  summarize_var_loc = hts_find_var(summarize_var, data = hts_data, variables_dt = variables_dt)

  # get variable or first occurrence for checkbox
  summarize_var_name = variables_dt[shared_name == summarize_var, variable][1]

  summarize_var_tbl = hts_data[[summarize_var_loc]][
    !get(summarize_var_name) %in% c(missing_values, not_imputable) |
      is.na(get(summarize_var_name))
  ]

  # get ids that are in this table
  ids_in_table = intersect(ids, names(summarize_var_tbl))

  # get id with the most unique counts to filter on
  max_index = which.max(
    sapply(summarize_var_tbl[, ids_in_table, with = FALSE], function(x) length(unique(x)))
  )

  summarize_var_id = ids_in_table[max_index]


  hts_data = hts_filter_data(
    hts_data = hts_data,
    ids = summarize_var_tbl[, get(summarize_var_id)],
    id_name = summarize_var_id
  )

  if (!is.null(summarize_by)) {
    for (i in 1:length(summarize_by)) {
      summarize_by_loc = hts_find_var(summarize_by[i], data = hts_data, variables_dt = variables_dt)

      # get variable or first occurrence for checkbox
      summarize_by_name = variables_dt[shared_name == summarize_by[i], variable][1]

      summarize_by_tbl = hts_data[[summarize_by_loc]][
        !get(summarize_by_name) %in% c(missing_values, not_imputable) |
          is.na(get(summarize_by_name))
      ]

      # get id with the most unique counts to filter on


      # get ids that are in this table
      ids_in_table = intersect(ids, names(summarize_by_tbl))

      max_index = which.max(
        sapply(summarize_by_tbl[, ids_in_table, with = FALSE], function(x) length(unique(x)))
      )

      summarize_by_id = ids_in_table[max_index]


      hts_data = hts_filter_data(
        hts_data = hts_data,
        ids = summarize_by_tbl[, get(summarize_by_id)],
        id_name = summarize_by_id
      )
    }
  }

  return(hts_data)
}

## quiets concerns of R CMD check
utils::globalVariables(c("ids", "ids_in_table"))
