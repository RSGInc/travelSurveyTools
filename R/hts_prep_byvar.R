#' Prepare variable to be summarized
#'
#' @param summarize_by Name of the variable to be prepped. Default is NULL.
#' @param variables_dt List of variable locations and descriptions in data.table
#'  format.
#' @param ... Additional parameters to pass to \code{link{hts_melt_vars}}
#'
#' @return Data table containing the variable to be summarized and other key
#'  columns.
#' @export
#'
#' @examples
#'
#' hts_prep_byvar(summarize_by = 'age',
#'                variables_dt = variable_list,
#'                hts_data = list('hh' = hh,
#'                             'person' = person,
#'                             'day' = day,
#'                             'trip' = trip,
#'                             'vehicle' = vehicle))
#' hts_prep_byvar(summarize_by = 'race',
#'                variables_dt = variable_list,
#'                hts_data = list('hh' = hh,
#'                             'person' = person,
#'                             'day' = day,
#'                             'trip' = trip,
#'                             'vehicle' = vehicle))
#'
hts_prep_byvar = function(summarize_by = NULL,
                          variables_dt = variables_list,
                          hts_data = hts_data,
                          ...) {

  # For each variables in trip table:
  byvar_dt_ls = list()

  for (b in seq_along(summarize_by)) {
    byvar = summarize_by[[b]]

    byvar_loc = hts_find_var(byvar, variables_dt = variables_dt)

    byvar_dt_v = data.table::copy(hts_data[[byvar_loc]])

    # Is this a shared variable?
    byvar_is_shared = variables_dt[shared_name == byvar, is_checkbox][1] == 1

    # Is this a numeric variable?
    byvar_is_numeric = variables_dt[shared_name == summarize_by[[b]], data_type][[1]] == "numeric"

    if (byvar_is_shared) {

      byvar_dt_v =
        hts_melt_vars(
          shared_name = summarize_by[[b]],
          wide_dt = byvar_dt_v,
          shared_name_vars = NULL,
          variables_dt = variable_list,
          remove_missing = TRUE,
          missing_values = c("Missing Response", "995"),
          checkbox_label_sep = ":",
          to_single_row = TRUE
        )

    }

    if (byvar_is_numeric) {
      byvar_dt_v = hts_bin_var(prepped_dt = byvar_dt_v,
                               numvar = byvar,
                               nbins = 7)
    }

    if (!byvar_is_shared) {
      byvar_cols = c(hts_get_keycols(byvar_dt_v), byvar)

      byvar_dt_v = byvar_dt_v[, byvar_cols, with=FALSE]

    }

    byvar_dt_ls[[b]] = byvar_dt_v

  }

  byvar_dt = Reduce(function(x, y)
    merge(x, y, all.x = FALSE, all.y = FALSE),
    byvar_dt_ls)

  return(byvar_dt[])
}


## quiets concerns of R CMD check
utils::globalVariables(c("variables_list", "is_checkbox", "data_type"))
