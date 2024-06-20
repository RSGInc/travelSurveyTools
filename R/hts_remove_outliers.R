#' Remove outliers from a numeric variable
#'
#' @param var_dt Dataset with a numeric variable to remove outliers from
#'  in data.table format.
#' @param numvar Numeric variable to remove outliers from. Default is NULL.
#' @param threshold Threshold to define what an outlier is. Default is .975.
#'
#' @return List of outliers removed and the dataset without the outliers.
#' @export
#'
#' @examples
#'
#' require(data.table)
#' 
#' hts_remove_outliers(var_dt = trip, numvar = "speed_mph")
#'
hts_remove_outliers = function(var_dt, numvar = NULL,
                                threshold = 0.975) {

  outlier_storage = list()

  outlier_cutoff = quantile(var_dt[, get(numvar)], threshold, na.rm = TRUE)

  outlier_table = data.table(
    threshold = threshold,
    num_removed = nrow(var_dt[get(numvar) >= outlier_cutoff]),
    min_outlier = min(var_dt[get(numvar) >= outlier_cutoff, get(numvar)]),
    max_outlier = max(var_dt[get(numvar) >= outlier_cutoff, get(numvar)])
  )

  if (outlier_table$num_removed > 0) {
    warning(stringr::str_glue(
      "{outlier_table$num_removed} outliers were removed based on the threshold of {threshold}."
    ))
  }

  outlier_storage[["outlier_description"]] = outlier_table

  outlier_storage[["dt"]] = var_dt[get(numvar) < outlier_cutoff]

  return(outlier_storage)
}
