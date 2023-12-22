#' Filter datasets to only keep specified ids
#'
#' @param data List of containing household, person, day, trip, and vehicle
#' tables in data.table format
#' @param ids List of ids to keep in all of the tables
#' @param id_type Type of id being used for filtering. Options are 'hh', 'person',
#' 'day', and 'trip'. Defaults to 'hh'.
#' 
#' @return Inputted list of tables filtered to the specified ids.
#' @export
#'
#' @examples
#' set.seed(45)
#' require(data.table)
#' DT = list(hh = data.table(
#'               hh_id = 1:10,
#'               num_people = sample(1:10, size = 10, replace = TRUE)),
#'       person = data.table(
#'               hh_id = sample(1:10, size = 20, replace = TRUE),
#'               person_id = 1:20,
#'               age = sample(1:5, size = 20, replace = TRUE)),
#'       day = data.table(
#'               hh_id = sample(1:10, size = 30, replace = TRUE),
#'               day_id = 1:30,
#'               dow = sample(1:7, size = 30, replace = TRUE)),
#'       trip = data.table(
#'               hh_id = sample(1:10, size = 30, replace = TRUE),
#'               trip_id = 1:30,
#'               mode = sample(1:10, size = 30, replace = TRUE)),
#'       vehicle = data.table(
#'               hh_id = sample(1:10, size = 20, replace = TRUE),
#'               vehicle_id = 1:20,
#'               fuel_type = sample(1:3, size = 20, replace = TRUE))) 
#' hts_filter_data(data = DT,
#'                 ids = DT$hh[num_people > 5, hh_id],
#'                 id_type = 'hh')
#'

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
