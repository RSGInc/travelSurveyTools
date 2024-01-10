#' Bin numeric variables
#'
#' @param prepped_dt Dataset containing variable to bin in data.table format
#' @param numvar Name of the numeric variable to bin
#' @param nbins Number of bins for variable. Defaults to 7.
#' 
#' @return Inputted dataset with the specified variable binned in data.table format.
#' @export
#'
#'  
#' @import stats
#'  
#' @examples
#' 
#' require(data.table)
#' require(stringr)
#' hts_bin_var(prepped_dt = hh, numvar = 'num_people')
#'
hts_bin_var = function(prepped_dt,
                       numvar,
                       nbins = 7) {
  
  # TODO: Allow user to specify bins directly to cut
  prepped_dt_binned = data.table::copy(prepped_dt)
  
  data.table::setnames(prepped_dt_binned, old = numvar, new = "numvar")
  
  # Reclassify outliers:
  q05 = round(stats::quantile(prepped_dt_binned[, numvar], na.rm = TRUE, 0.025))
  q95 = round(stats::quantile(prepped_dt_binned[, numvar], na.rm = TRUE, 0.975))
  
  prepped_dt_binned[, binned := ifelse(numvar >= q95, q95, numvar)]
  prepped_dt_binned[, binned := ifelse(numvar <= q05, q05, numvar)]
  
  
  # Create breaks:
  round_digits = ifelse(
    as.numeric(q95-q05) < 5, 
    1, 
    0)
  
  
  mid_breaks = seq(from = q05,
                   to = q95,
                   by = round((q95 - q05) / (nbins - 2), round_digits))
  
  min_break = ifelse(q05 == 0, -Inf, 0)
  
  all_breaks = c(min_break,
                 mid_breaks,
                 q95,
                 Inf)
  
  all_breaks = unique(all_breaks)
  
  prepped_dt_binned[, binned := cut(
    numvar,
    breaks = all_breaks,
    include.lowest = TRUE
  )
  ]
  
  # Clean up bin labels:
  binlabels = levels(prepped_dt_binned$binned)
  binlabels = stringr::str_remove(string = binlabels, pattern = "[(]")
  binlabels = stringr::str_remove(string = binlabels, pattern = "[]]")
  binlabels = stringr::str_replace(string = binlabels,
                                   pattern = ",Inf|, Inf",
                                   replacement = " or more")
  binlabels[[1]] =
    ifelse(q05 == 0,
           "Exactly 0",
           paste0(str_split_i(
             binlabels[[1]], i = 2, pattern = ","
           ), " or less"))
  
  # if ( prepped_dt_binned[binned %in% c('(-Inf,0]', '(-Inf, 0]'), .N] == 0 ){
  #   binlabels = binlabels[binlabels != 'Exactly 0']
  #   
  #   
  #   levels(prepped_dt_binned$binned) = levels(prepped_dt_binned$binned)[!levels(prepped_dt_binned$binned) %in% c('(-Inf,0]', '(-Inf, 0]')]
  #   
  # }
  
  
  binlabels = stringr::str_replace(string = binlabels,
                                   pattern = ",",
                                   replacement = "-")
  
  prepped_dt_binned[, binned := factor(binned, 
                                       levels = levels(prepped_dt_binned$binned),
                                       labels = binlabels)]
  
  levels(prepped_dt_binned$binned) = c(levels(prepped_dt_binned$binned),
                                       "Missing")
  
  prepped_dt_binned[is.na(binned), binned := "Missing"]
  
  # Return result:
  prepped_dt_binned[, numvar := NULL]
  data.table::setnames(prepped_dt_binned, old = "binned", new = numvar)
  
  return(prepped_dt_binned[])
  
}

## quiets concerns of R CMD check
utils::globalVariables(c("binned", "str_split_i"))
