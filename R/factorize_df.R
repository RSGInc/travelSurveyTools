#' Factorize a dataframe
#'
#' Factorize a dataframe. The function loops over a dataframe (calling
#' factorize_column) and labels each variable for which you provide labels.
#'
#' The function expects a values dataframe (vals_df) in the following format:
#' variable (the character/string names of each variable), value (the integer
#' values for each variable), val_order (the sequential ordering of each
#' value), label (the strings or names to use in the levels of the factor).
#'
#' The "factorize" functions were borrowed and updated from the
#' 'tmr.Rite.out.tester' package by Matt Landis.
#'
#' @param df A dataframe to label
#' @param vals_df A dataframe of variable labels (i.e., factor levels and
#'  labels) with the format as specified below. Passed to factorize_column
#'  function.
#' @param verbose Prints which vars are labeled and unlabeled
#' @param ... Additional arguments passed to \code{\link{factorize_column}}
#' @return A factorized (i.e. labeled) version of the dataframe it was passed.
#' @examples
#'
#' hh_labeled <- factorize_df(
#'   df = hh,
#'   vals_df = value_labels,
#'   value_label_colname = "label",
#'   extra_labels = c("Missing")
#' )
#'
#' @export factorize_df
factorize_df <- function(df, vals_df, verbose = TRUE, ...) {
  if ("data.table" %in% class(df)) {
    df_is_dt <- TRUE
    df_labeled <- as.data.frame(df) # If df is a data.table, R crashes in Rstudio
    # on.exit(expr=data.table(df_labeled))
  } else {
    df_is_dt <- FALSE
  }

  for (i in 1:ncol(df)) {
    var_str <- names(df)[i]

    df_labeled[[var_str]] <- factorize_column(
      x = df[[var_str]],
      var_str = var_str,
      vals_df = vals_df,
      ...
    )
  }

  if (verbose) {
    # print which vars are labeled and unlabeled
    labeled_vars_in_df <- sort(colnames(df)[(colnames(df) %in% vals_df$variable)])
    unlabeled_vars_in_df <- sort(colnames(df)[!(colnames(df) %in% vals_df$variable)])

    message("\n Labeled vars: ")
    message(paste(sprintf("- %s", labeled_vars_in_df), collapse = "\n"), "\n")
    message("Unlabeled vars: ")
    message(paste(sprintf("- %s", unlabeled_vars_in_df), collapse = "\n"))
  }
  if (df_is_dt) {
    df_labeled <- data.table::data.table(df_labeled)
  }
  return(df_labeled)
}
