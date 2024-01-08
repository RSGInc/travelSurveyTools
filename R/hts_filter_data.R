#' Filter datasets to only keep specified ids
#'
#' @param data List of containing household, person, day, trip, and vehicle
#'  tables in data.table format
#' @param ids List of ids to keep in all of the tables
#' @param id_type Type of id being used for filtering. Options are 'hh', 'person',
#'  'day', and 'trip'. Defaults to 'hh'.
#'
#' @return Inputted list of tables filtered to the specified ids.
#' @export
#'
#' @examples
#'
#' require(data.table)
#' hts_filter_data(data = list('hh' = hh,
#'                             'person' = person,
#'                             'day' = day,
#'                             'trip' = trip,
#'                             'vehicle' = vehicle),
#'                 ids = hh[num_people > 5, hh_id],
#'                 id_type = 'hh')
#'

hts_filter_data = function(hts_data,
                           ids,
                           id_type = 'hh'){

  list2env(hts_data, envir = environment())

  if (id_type == 'hh'){

    hts_data[['hh']] = hts_data[['hh']][hh_id %in% ids]

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

  return(hts_data)
}
