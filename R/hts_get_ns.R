hts_get_ns = function(prepped_dt,
                      weighted
) {
  
  #get unweighted counts
  n_idcols = hts_get_keycols(prepped_dt, 
                             ids = TRUE,
                             weights = FALSE)
  
  ndt_ids = prepped_dt[, ..n_idcols]
  
  ns_unwtd = lapply(ndt_ids, function(x) uniqueN(x))
  
  
  n_names =
    paste0(stringr::str_to_title(
      stringr::str_replace(
        pattern = "hh",
        replacement = "household",
        string = stringr::str_remove(string = n_idcols, pattern = "_id")
      )
    ),
    "s")
  
  names(ns_unwtd) = n_names
  
  #get units
  id_names = c('trip_id', 'day_id', 'person_id', 'vehicle_id', 'hh_id')
  
  unit = NULL
  
  for (name in id_names)
    
    
    if (name %in% names(prepped_dt)){
      
      id = name
      
      unit = paste0(stringr::str_remove(string = id, pattern = "_id"),
                    "s")
      
      break
      
    }
  
  #get weighted counts
  if (weighted){
    
    n_wtcols = hts_get_keycols(prepped_dt, 
                               ids = FALSE,
                               weights = TRUE)
    
    ndt_wts = prepped_dt[, ..n_wtcols]
    
    ns_wtd = lapply(ndt_wts, function(x) sum(x))
    
    n_names =
      paste0(stringr::str_to_title(
        stringr::str_replace(
          pattern = "hh",
          replacement = "household",
          string = stringr::str_remove(string = n_wtcols, pattern = "_weight")
        )
      ),
      "s")
    
    names(ns_wtd) = n_names
    
  } else {
    
    ns_wtd = NULL
    
  }
  
  ns = list(
    'unwtd' = ns_unwtd,
    'wtd' = ns_wtd,
    'units' = unit
  )
  
  return(ns)
  
  
}
