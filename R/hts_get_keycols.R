#' Find key columns in table
#'
#' @param dt Dataset to find key columns of in data.table format
#' @param ids Boolean whether to return id columns. Default is TRUE.
#' @param weights Boolean whether to return weight columns. Default is TRUE.
#' @param priority Boolean whether to only return highest level weight/id.
#' Default is FALSE.
#' 
#' @return List of names of key columns in the dataset.
#' @export
#'
#' @examples
#' set.seed(45)
#' require(data.table)
#' DT = data.table(
#'       hh_id = sample(1:10, size = 30, replace = TRUE),
#'       trip_id = 1:30,
#'       mode = sample(1:10, size = 30, replace = TRUE),
#'       hh_weight = sample(100:200, size = 30, replace = TRUE),
#'       trip_weight = sample(100:200, size = 30, replace = TRUE))
#' hts_get_keycols(dt = DT)
#' hts_get_keycols(dt = DT, priority = TRUE)
#'
hts_get_keycols = function(dt,
                           ids = TRUE,
                           weights = TRUE,
                           priority = FALSE){
  
  idcols = c('trip_id', 'day_id', 'person_id','hh_id', 'vehicle_id')
  wtcols = c('trip_weight', 'day_weight', 'person_weight', 'hh_weight')
  
  idnames = c()
  wtnames = c()
  
  #get id(s)
  if(ids){
    
    # priority will select the highest level weight/id only
    if (priority){
      
      for (name in idcols){
        
        if (name %in% names(dt)){
          
          idnames = name
          break
        }
        
      }
      
    } else {
      
      idnames = c(names(dt)[names(dt) %in% idcols])
      
    }
  }
  
  #get weight(s)
  if(weights){
    
    if (priority){
      
      for (name in wtcols){
        
        if (name %in% names(dt)){
          
          wtnames = name
          break
        }
        
      }
      
    } else {
      
      wtnames = c(names(dt)[names(dt) %in% wtcols])
      
    }
  }
  
  names = c(idnames, wtnames)
  
  return(names)
  
}
