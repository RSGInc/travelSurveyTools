library(data.table)
library(stringr)
library(tmrtools)

### Read in data-----

# reading in data from POPS
dbname = 'psrc'
schema = 'psrc_2023'
con = connect_to_pops(dbname)

hh = read_from_db(con, str_glue('select * from {schema}.ex_hh_interim'), 
                  disconnect = FALSE)
person = read_from_db(con, str_glue('select * from {schema}.ex_person_interim'), 
                      disconnect = FALSE)
day = read_from_db(con, str_glue('select * from {schema}.ex_day_interim'), 
                   disconnect = FALSE)
vehicle = read_from_db(con, str_glue('select * from {schema}.ex_vehicle_interim'),
                       disconnect = FALSE)
trip = read_from_db(con, str_glue('select * from {schema}.ex_trip_interim'), 
                    disconnect = TRUE)

user = Sys.info()['user']

# reading in codebook from sharepoint
codebook_path = paste0('C:/Users/',
                       user,
                       '/Resource Systems Group, Inc/Transportation MR - Documents/',
                       'PSRC Survey Program/210252_PSRC_HTS/Internal/3.DataAnalysis/',
                       '1.Data/Codebook/PSRC_Combined_Codebook_2023_08162023_RSG.xlsx')

variable_list = readxl::read_xlsx(codebook_path,
                                  sheet = 'ex_variable_list_2023')
setDT(variable_list)

value_labels = readxl::read_xlsx(codebook_path,
                                 sheet = 'ex_value_labels_2023')
setDT(value_labels)


### Filter data----

# choose 1000 random hhs to keep
ids_to_keep = sample(hh$hh_id, size = 1000)

hh_filtered = hh[hh_id %in% ids_to_keep]
person_filtered = person[hh_id %in% ids_to_keep]
day_filtered = day[hh_id %in% ids_to_keep]
vehicle_filtered = vehicle[hh_id %in% ids_to_keep]
trip_filtered = trip[hh_id %in% ids_to_keep]

# filter out columns containing pii
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
trip_pii_cols = c('o_lon', 'o_lat', 'd_lon', 'd_lat', 'mode_other_specify',
                  'd_purpose_other')
keep_trip_cols = setdiff(names(trip_filtered), trip_pii_cols)
trip_filtered = trip_filtered[, ..keep_trip_cols]


# only keep specified columns in each table
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

# only keep variables in the codebook that remain in the dataset
variable_list = variable_list[variable %in% c(keep_hh_cols, keep_person_cols,
                                              keep_day_cols, keep_trip_cols,
                                              keep_veh_cols)]
variable_list[, location := NULL]
value_labels = value_labels[variable %in% c(keep_hh_cols, keep_person_cols,
                                            keep_day_cols, keep_trip_cols,
                                            keep_veh_cols)]

value_labels = value_labels[, c('variable', 'value', 'label')]

### Write data----

hh = hh_filtered
usethis::use_data(hh, overwrite = TRUE)

person = person_filtered
usethis::use_data(person, overwrite = TRUE)

day = day_filtered
usethis::use_data(day, overwrite = TRUE)

trip = trip_filtered
usethis::use_data(trip, overwrite = TRUE)

vehicle = vehicle_filtered
usethis::use_data(vehicle, overwrite = TRUE)

usethis::use_data(variable_list, overwrite = TRUE)

usethis::use_data(value_labels, overwrite = TRUE)
