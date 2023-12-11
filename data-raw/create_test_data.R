library(data.table)

get_pops_creds = function(what = c('user_name', 'password'), env = parent.frame()) {
  
  if ( !what %in% c('user_name', 'password')){
    stop('please specify what = "user_name" or "password"')
  }
  
  # On Azure Pipelines, pops creds are stored as variables in the environment
  # Keep this here so it will continue to function
  
  if (paste0('pops.', what) %in% names(env)) {
    
    value = get(paste0('pops.', what), envir = env)
    message('Using ', what, ' from environment')
    return(value)
    
  }
  
  #if ( !requireNamespace('keyring', quietly = TRUE) ) {
  #  return(value)
  #}
  
  try({
    value = keyring::key_get('pops', what)
  }, silent = TRUE)
  
  if ( !exists('value') || is.null(value) || value == '') {
    
    message(what, ' not found in keyring')
    
    value = readline(prompt = paste0('Please enter your POPS ', what, ': '))
    
    keyring::key_set_with_value(
      service = 'pops',
      username = what,
      password = value)
    
    value = keyring::key_get(
      'pops',
      what)
    
    message('Saved ', what, ' in default keyring')
    
  }
  
  return(value)
}

connect_to_pops = function(
    dbname,
    env = parent.frame() # needed for azure pipelines
){
  
  drv = RPostgres::Postgres()
  
  if (dbname == 'calgary') {
    
    host_name = '40.86.250.54'
    
  } else {
    
    host_name = 'pops.rsginc.com'
    
  }
  
  con = DBI::dbConnect(
    drv,
    host = host_name,
    user = get_pops_creds(what = 'user_name', env),
    password = get_pops_creds(what = 'password', env),
    dbname = dbname,
    port = "5432",
    sslcert = "",
    sslkey = "",
    sslmode = "require",
    bigint = c("numeric")
  )
  
  return(con)
}

sql_pops = function(
    sql,
    dbname
){
  
  con = connect_to_pops(dbname)
  
  qry = DBI::dbGetQuery(con,sql)
  
  dt = data.table::setDT(qry)
  
  DBI::dbDisconnect(con)
  
  return(dt)
}

hh = sql_pops('select * from psrc_2023.d_ex_hh_indri', 'psrc')
person = sql_pops('select * from psrc_2023.d_ex_person_indri', 'psrc')
day = sql_pops('select * from psrc_2023.d_ex_day_indri', 'psrc')
vehicle = sql_pops('select * from psrc_2023.d_ex_vehicle_indri', 'psrc')
trip = sql_pops('select * from psrc_2023.d_ex_trip_indri', 'psrc')
location = sql_pops('select * from psrc_2023.d_ex_location_indri', 'psrc')

ids_to_keep = sample(hh$hh_id, size = 1000)

hh_filtered = hh[hh_id %in% ids_to_keep]
person_filtered = person[hh_id %in% ids_to_keep]
day_filtered = day[hh_id %in% ids_to_keep]
vehicle_filtered = vehicle[hh_id %in% ids_to_keep]
trip_filtered = trip[hh_id %in% ids_to_keep]

names(hh_filtered)
hh_pii_cols = c('home_lon', 'home_lat', 'sample_home_lon', 'sample_home_lat')
keep_hh_cols = setdiff(names(hh_filtered), hh_pii_cols)
hh_filtered = hh_filtered[, ..keep_hh_cols]


names(person_filtered)
person_pii_cols = c('second_home_lon', 'second_home_lat', 'industry_other',
                    'work_lon', 'work_lat', 'school_lon', 'school_lat',
                    'ethnicity_other', 'race_other')
keep_person_cols = setdiff(names(person_filtered), person_pii_cols)
person_filtered = person_filtered[, ..keep_person_cols]


names(day_filtered)
day_pii_cols = c()


names(vehicle_filtered)
vehicle_pii_cols = c('other')
keep_vehicle_cols = setdiff(names(vehicle_filtered), vehicle_pii_cols)
vehicle_filtered = vehicle_filtered[, ..keep_vehicle_cols]


names(trip_filtered)
trip_pii_cols = c('o_lon', 'o_lat', 'd_lon', 'd_lat', 'mode_other_specify', 'd_purpose_other')
keep_trip_cols = setdiff(names(trip_filtered), trip_pii_cols)
trip_filtered = trip_filtered[, ..keep_trip_cols]



names(hh_filtered)
keep_hh_cols = c('hh_id', 'sample_segment', 'income_detailed', 'income_followup',
                 'num_people', 'residence_type')
hh_filtered = hh_filtered[, ..keep_hh_cols]


names(person_filtered)
keep_person_cols = c('hh_id', 'person_id', 'ethnicity_1', 'ethnicity_2',
                     'ethnicity_3', 'ethnicity_4', 'ethnicity_997',
                     'ethnicity_999', 'race_1', 'race_2', 'race_3',
                     'race_4', 'race_5', 'race_997', 'race_999', 'age', 'gender',
                     'employment', 'education')
person_filtered = person_filtered[, ..keep_person_cols]

names(day_filtered)
keep_day_cols = c('hh_id', 'person_id', 'day_id', 'delivery_2', 'delivery_3',
                  'delivery_4', 'delivery_5', 'delivery_6', 'delivery_7',
                  'delivery_8', 'delivery_996', 'begin_day', 'end_day')
day_filtered = day_filtered[, ..keep_day_cols]

names(trip_filtered)
keep_trip_cols = c('hh_id', 'person_id', 'day_id', 'trip_id', 'travel_date', 
                   'mode_type', 'd_purpose_category')
trip_filtered = trip_filtered[, ..keep_trip_cols]

names(vehicle_filtered)
keep_veh_cols = c('hh_id', 'vehicle_id', 'fuel_type')
vehicle_filtered = vehicle_filtered[, ..keep_veh_cols]

user = Sys.info()['user']

variable_list = readxl::read_xlsx(stringr::str_glue("C:/Users/{user}/Resource Systems Group, Inc/Transportation MR - Documents/210267 Metrolina HTS/Internal/3.DataAnalysis/1.Data/metrolina_pipeline_codebook.xlsx"),
                              sheet = 'variable_list')
setDT(variable_list)

variable_list = variable_list[variable %in% c(keep_hh_cols, keep_person_cols,
                                              keep_day_cols, keep_trip_cols,
                                              keep_veh_cols)]

variable_list[, c('location', 'linked_trip', 'label', 'write_to_export') := NULL]

value_labels = readxl::read_xlsx(stringr::str_glue("C:/Users/{user}/Resource Systems Group, Inc/Transportation MR - Documents/PSRC Survey Program/210252_PSRC_HTS/Internal/3.DataAnalysis/psrc_psrc_2023_pipeline_codebook.xlsx"),
                              sheet = 'value_labels')
setDT(value_labels)

value_labels = value_labels[variable %in% c(keep_hh_cols, keep_person_cols,
                                              keep_day_cols, keep_trip_cols,
                                              keep_veh_cols)]

hh = hh_filtered
usethis::use_data(hh, overwrite = TRUE)

person = person_filtered
usethis::use_data(person)

day = day_filtered
usethis::use_data(day)

trip = trip_filtered
usethis::use_data(trip)

vehicle = vehicle_filtered
usethis::use_data(vehicle)

usethis::use_data(variable_list)

usethis::use_data(value_labels)
