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
  
  # FIXME: Do I really need to copy prepped_dt here?
  wso = data.table::copy(prepped_dt)
  
  if (!weighted) {
    
    so = srvyr::as_survey_design(wso, w = NULL)
    
  } else if (weighted) {
    
    if (!wtname %in% names(wso)) {
      
      stop(paste0(wtname, " weight column not found."))
      
    }
    
    if (weighted &
        !is.null(strataname) & 
        !strataname %in% names(wso)) {
      
      stop(paste0(wtname, " strata column not found."))
      
    }
    
    data.table::setnames(wso, wtname, "weight")
    
    # filter to where weight > 0 (for appropriate counts):
    wso = wso[weight > 0]
    
    if (!is.null(strataname)) {
      
      so = srvyr::as_survey_design(wso, w = weight, strata = strataname)
      
    } else{
      
      so = srvyr::as_survey_design(wso, w = weight)
      
    }
  }
  
  return(so)
  
}

## quiets concerns of R CMD check
utils::globalVariables(c("weight"))

