#' Factorize a column.
#'
#' Factorize a column. This function is typically not called directly, but is
#' called as part of the factorize_df function.
#'
#' The function expects a values dataframe (vals_df) with columns for the variable name, the value, the value label,
#' and the value order (can be the same as the value itself)
#'
#' The "factorize" functions were borrowed and updated from the
#' 'tmr.Rite.out.tester' package by Matt Landis.
#'
#' @param x A vector (or variable) to be 'factorized' or labeled. X is often a
#'  single variable or column within a dataframe.
#' @param var_str The name of the vector or variable to be factorized (e.g.,
#'  "sample_segment").
#' @param vals_df A dataframe of variable labels (i.e., factor levels and
#'  labels) with the format as specified below.
#' @param variable_colname The name of the variable column in vals_df. Default is 'variable'.
#' @param value_colname The name of the value column in vals_df. Default is 'value',
#' @param value_label_colname The name of the value label column in vals_df. Default is 'value_label'
#' @param value_order_colname The name of the value order column in vals_df (can be the value column itself). Default is 'value'.
#' @param extra_labels Pass a vector of the names of other variables in the
#'  values dataframe to use when labeling. Common uses include missing values,
#'  universal values (e.g., "Total" or "Subtotal" row), or similar variables
#'  (e.g., "mode_1" to reuse the mode_1 labels for mode_2). Default is NULL.
#' @param add_na TRUE or FALSE setting for whether to add NA as an explicit
#'  level in the factor. Default setting of TRUE.
#' @return A 'factorized' version of the vector it was passed (i.e., a factor).
#'  This is returned invisibly.
#' @examples
#'
#' factored = factorize_column(
#'   x = hh$income_detailed,
#'   var_str = "income_detailed",
#'   vals_df = value_labels,
#'   extra_labels = "Missing",
#'   value_label_colname = 'label'
#' )
#'
#' @export factorize_column
factorize_column = function(
  x,
  var_str,
  vals_df,
  variable_colname = 'variable',
  value_colname = 'value',
  value_label_colname = 'value_label',
  value_order_colname = 'value',
  extra_labels = NULL,
  add_na = TRUE
) {
  vals_df = data.table::data.table(vals_df)

  # sort the vals_df to ensure the ordered factor is ordered correctly
  vals_df = vals_df[with(vals_df,
                          order(get(variable_colname),
                                get(value_order_colname),
                                get(value_colname),
                                get(value_label_colname))), ]

  # select levels and labels for this variable
  levels  = vals_df[get(variable_colname) == var_str, get(value_colname)]
  labels  = vals_df[get(variable_colname) == var_str, get(value_label_colname)]

  # add extra labels, if provided
  if ( !missing(extra_labels) ) {
    levels = c(levels, vals_df[get(variable_colname) %in% extra_labels,  get(value_colname)])
    labels = c(labels, vals_df[get(variable_colname) %in% extra_labels,  get(value_label_colname)])
  }

  # only apply labels if there are labels specific to this variable
  if (var_str %in% unique(vals_df[, get(variable_colname)])) {

    #perform QC checks before factorizing

    # check for duplicate labels (gets a warning)
    if ( any(duplicated(labels)) ) {
      warning('Duplicated labels in variable "', var_str, '". Labels: ',
              paste(labels, collapse = '; '))
              }

    # check for duplicate values (or "levels") (gets a warning)
    if ( any(duplicated(levels)) ) {
      warning('Duplicated values/levels in variable "', var_str, '". Values: ',
              paste(levels, collapse = '; '))
              }

    # check for missing levels (except NA) (gets a warning)
    if ( any(!(unique(x) %in% c(levels, NA))) ) {
      missing_levels = unique(x)[!unique(x) %in% c(levels, NA)]

      levels = c(levels, missing_levels)
      labels = c(labels, extra_labels)

      warning('Missing labels in variable "', var_str, '". Values missing labels: ',
              paste(missing_levels, collapse = '; '))
      }

    # TODO add checks after making variable a factor to ensure it's right?

    y = factor(x, levels = levels, labels = labels, ordered = TRUE)

    # if NAs are desired.
    if (add_na) {y = addNA(y)}
    #if (add_na) {forcats::fct_explicit_na(y, na_level = "(Missing - NA)")}

  } else {

    # if there are no labels in the codebook for this variable, it is returned unchanged.
    y = x
  }
  return(invisible(y))
}

