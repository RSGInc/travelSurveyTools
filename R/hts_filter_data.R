#filers hts_data based on supplied ids
#hts_data is a list of dts


hts_filter_data = function(data = list('hh' = hh,
                                       'person' = person,
                                       'day' = day,
                                       'trip' = trip,
                                       'vehicle' = vehicle),
                           ids,
                           id_type = 'hh'){
  
  list2env(data, envir = environment())
  
  if (id_type == 'hh'){
    
    hh = hh[hh_id %in% ids]
    
    person = person[hh_id %in% ids]
    
    day = day[hh_id %in% ids]
    
    vehicle = vehicle[hh_id %in% ids]
    
    trip = trip[hh_id %in% ids]
    
  } else if (id_type == 'person'){
    
    person = person[person_id %in% ids]
    
    day = day[person_id %in% ids]
    
    trip = trip[person_id %in% ids]
    
    hh = hh[hh_id %in% person[, unique(hh_id)]]
    
    vehicle = vehicle[hh_id %in% hh$hh_id]
    
  } else if (id_type == 'day'){
    
    day = day[day_id %in% ids]
    
    trip = trip[day_id %in% ids]
    
    hh = hh[hh_id %in% day[, unique(hh_id)]]
    
    person = person[hh_id %in% hh$hh_id]
    
    vehicle = vehicle[hh_id %in% hh$hh_id]
    
  } else if (id_type == 'trip'){
    
    trip = trip[trip_id %in% ids]
    
    hh = hh[hh_id %in% trip[, unique(hh_id)]]
    
    person = person[hh_id %in% hh$hh_id]
    
    day = day[hh_id %in% hh$hh_id]
    
    vehicle = vehicle[hh_id %in% hh$hh_id]
    
  }
  
  hts_data = list(
    'hh' = hh,
    'person' = person,
    'day' = day,
    'trip' = trip,
    'vehicle' = vehicle
  )
  
  return(hts_data)
}
