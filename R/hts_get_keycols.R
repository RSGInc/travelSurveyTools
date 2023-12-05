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
