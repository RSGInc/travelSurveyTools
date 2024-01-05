#' Transform travel survey data.table to a survey object
#'
#' @param prepped_dt Dataframe in data.table format to transform to survey object.
#' @param weighted If true creates a weighted survey object. Default is TRUE.
#' @param wtname Name of the weight column in the dataframe. If NULL searches for
#'  one of hh_weight, person_weight, day_weight, or trip_weight.
#' @param strataname Name of strata name to bring in. Default is NULL.
#' 
#' @return Inputted dataframe transformed into a survey object.
#' @export
#'
#' @examples
#' 
#' require(data.table)
#' hts_to_so(prepped_dt = trip)
#'
hts_to_so = function(prepped_dt,
                     weighted = TRUE,
                     wtname = NULL,
                     strataname = NULL) {
  
  
  wtcols = c('hh_weight', 'person_weight', 'day_weight', 'trip_weight')
  
  wso = data.table::copy(prepped_dt)
  
  if (weighted == TRUE) {
    wtnames = names(wso)[names(wso) %in% wtcols]
    
    wtname = hts_get_keycols(wso,
                             weights = TRUE,
                             ids = FALSE,
                             priority = TRUE)
    
    }
    
    
    if (!wtname %in% names(wso)) {
      stop(
        "Weights not found. Is this unweighted data? If so, specify weighted = FALSE in hts_prop_table."
      )
    }
    
    data.table::setnames(wso, wtname, "weight")
    
    # remove extra weight columns:
    if (length(wtnames[wtnames %in% names(wso)]) != 0) {
      wso[, (wtnames[wtnames %in% names(wso)]) := NULL]
    }
    
    # filter to where weight > 0 (for appropriate counts):
    wso = wso[weight > 0]
  
  # Weighted survey design object, w/ or w/o strata:
  if (weighted == TRUE) {
    
    if (!is.null(strataname)) {
      
      if (!strataname %in% names(wso)) {
        
        wso = hts_cbind_var(wso, strataname, variable_list = variable_list)
      }
      
      so = srvyr::as_survey_design(wso, w = weight, strata = strataname)
      
    } else{
      
      so = srvyr::as_survey_design(wso, w = weight)
      
    }
  }
  
  return(so)
}
