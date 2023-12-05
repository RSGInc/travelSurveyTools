hts_remove_outliers = function (var_dt, numvar = NULL,
                                threshold = 0.975){
  
  outlier_storage = list()
  
  outlier_cutoff = quantile(var_dt[, get(numvar)], threshold, na.rm = TRUE)
  
  outlier_table = data.table(
    threshold = threshold,
    num_removed = nrow(var_dt[get(numvar) >= outlier_cutoff]),
    min_outlier = min(var_dt[get(numvar) >= outlier_cutoff, get(numvar)]),
    max_outlier = max(var_dt[get(numvar) >= outlier_cutoff, get(numvar)])
  )
  
  outlier_storage[['outlier_description']] = outlier_table
  
  outlier_storage[['dt']] = var_dt[get(numvar) < outlier_cutoff]
  
  return(outlier_storage)
}
