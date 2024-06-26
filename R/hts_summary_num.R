#' Summarize a numeric variable
#'
#' @param prepped_dt A prepared dataset in data.table format with
#' the variable to summarize, the variable to summarize by, and the weights,
#' if used.
#' @param summarize_var Name of the variable to summarize. Default is NULL.
#' @param summarize_by Name of the variable to summarize the summarize_var by.
#'  Default is NULL.
#' @param weighted Whether the data is weighted. Default is TRUE.
#' @param se Whether to calculate standard error. Default is FALSE. Will be set
#' to FALSE if weighted is FALSE.
#' @param wtname Name of the weight column to use. Default is NULL. Must be specified
#' when weighted = TRUE.
#' @param strataname  Name of strata name to bring in. Default is NULL.
#'
#' @importFrom srvyr survey_mean
#' @importFrom srvyr survey_median
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by_at
#'
#' @return List of unweighted and weighted numeric summaries including count, min,
#'  max, mean, se, and median.
#' @export
#'
#' @examples
#'
#' require(data.table)
#' require(stringr)
#' require(dplyr)
#' require(srvyr)
#' DT = hts_prep_variable(
#'   summarize_var = "speed_mph",
#'   variables_dt = variable_list,
#'   data = list(
#'     "hh" = hh,
#'     "person" = person,
#'     "day" = day,
#'     "trip" = trip,
#'     "vehicle" = vehicle
#'   )
#' )$num
#' hts_summary_num(
#'   prepped_dt = DT,
#'   summarize_var = "speed_mph",
#'   wtname = "trip_weight"
#' )
#' DT = hts_prep_variable(
#'   summarize_var = "speed_mph",
#'   summarize_by = "age",
#'   variables_dt = variable_list,
#'   data = list(
#'     "hh" = hh,
#'     "person" = person,
#'     "day" = day,
#'     "trip" = trip,
#'     "vehicle" = vehicle
#'   )
#' )$num
#' hts_summary_num(
#'   prepped_dt = DT,
#'   summarize_var = "speed_mph",
#'   summarize_by = "age",
#'   wtname = "trip_weight"
#' )
hts_summary_num = function(prepped_dt,
                            summarize_var = NULL,
                            summarize_by = NULL,
                            weighted = TRUE,
                            se = FALSE,
                            wtname = NULL,
                            strataname = NULL) {
  if (!weighted & se) {
    message("Standard errors require weighted data; setting se = FALSE.
            Set weighted = TRUE and specify a wtname if standard errors are desired.")

    se = FALSE
  }

  num_so_ls = list()

  num_so_ls[["unwtd"]] = srvyr::as_survey_design(prepped_dt, w = NULL)

  if (weighted == TRUE) {
    num_so_ls[["wtd"]] = hts_to_so(prepped_dt, strataname = strataname, wtname = wtname)
  }


  num_summary_ls = list()

  for (w in seq_along(num_so_ls)) {
    wt_type = names(num_so_ls)[[w]]

    # Set the variance type:
    variance_type = "se"

    if (se) {
      # Calculate survey proportions:
      num_summary_wttype =
        num_so_ls[[wt_type]] |>
        group_by_at(unlist(summarize_by)) |>
        summarize(
          count = length(get(summarize_var)),
          min = min(get(summarize_var), na.rm = TRUE),
          max = max(get(summarize_var), na.rm = TRUE),
          mean = survey_mean(get(summarize_var), vartype = variance_type, na.rm = TRUE),
          median = survey_median(get(summarize_var), vartype = NULL, na.rm = TRUE)
        ) |>
        setDT()

      num_summary_ls[[wt_type]] = num_summary_wttype
    } else {
      # Calculate survey proportions:
      num_summary_wttype =
        num_so_ls[[wt_type]] |>
        group_by_at(unlist(summarize_by)) |>
        summarize(
          count = length(get(summarize_var)),
          min = min(get(summarize_var), na.rm = TRUE),
          max = max(get(summarize_var), na.rm = TRUE),
          mean = survey_mean(get(summarize_var), vartype = NULL, na.rm = TRUE),
          median = survey_median(get(summarize_var), vartype = NULL, na.rm = TRUE)
        ) |>
        setDT()

      num_summary_ls[[wt_type]] = num_summary_wttype
    }
  }

  if (weighted) {
    num_summary_ls$weight_name = wtname
  }

  return(num_summary_ls)
}

## quiets concerns of R CMD check
utils::globalVariables(c("value_labels"))
