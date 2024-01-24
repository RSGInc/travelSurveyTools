#' Get counts from dataset
#'
#' @param prepped_dt Dataset to pull counts from.
#' @param weighted Boolean whether to pull weighted estimates.
#' 
#' @return List of unweighted counts, weighted counts, and highest level unit.
#' @export
#'
#' @examples
#' 
#' require(data.table)
#' hts_get_ns(prepped_dt = day, weighted = TRUE)
#'
hts_get_ns = function(prepped_dt,
                      weighted,
                      ids,
                      wt_col
) {
  
  # Get ids that are in prepped_dt
  present_ids = intersect(names(prepped_dt), ids)
  
  ndt_ids = prepped_dt[, present_ids, with=FALSE]
  
  ns_unwtd = lapply(ndt_ids, function(x) uniqueN(x))

  n_names = paste('Count of unique', present_ids)
  
  names(ns_unwtd) = n_names
  
  #get weighted counts
  if (weighted){
    
    ns_wtd = prepped_dt[, sum(get(wt_col))]

    names(ns_wtd) = paste('Sum of', wt_col)
    
  } else {
    
    ns_wtd = NULL
    
  }
  
  ns = list(
    'unwtd' = ns_unwtd,
    'wtd' = ns_wtd
  )
  
  return(ns)
  
}
