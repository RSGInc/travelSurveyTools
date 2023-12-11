library(data.table)
library(tmrtools)

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

variable_list = read_codebook("C:/Users/jacob.moore/Resource Systems Group, Inc/Transportation MR - Documents/210267 Metrolina HTS/Internal/3.DataAnalysis/1.Data/metrolina_pipeline_codebook.xlsx",
                              value_labels = FALSE, sheet = 'variable_list')

variable_list = variable_list[variable %in% c(keep_hh_cols, keep_person_cols,
                                              keep_day_cols, keep_trip_cols,
                                              keep_veh_cols)]

variable_list[, c('location', 'linked_trip', 'label', 'write_to_export') := NULL]

value_labels = readxl::read_xlsx("C:/Users/jacob.moore/Resource Systems Group, Inc/Transportation MR - Documents/PSRC Survey Program/210252_PSRC_HTS/Internal/3.DataAnalysis/psrc_psrc_2023_pipeline_codebook.xlsx",
                              sheet = 'value_labels')
setDT(value_labels)

value_labels = value_labels[variable %in% c(keep_hh_cols, keep_person_cols,
                                              keep_day_cols, keep_trip_cols,
                                              keep_veh_cols)]

saveRDS(hh_filtered, 'hh.rds')
saveRDS(person_filtered, 'person.rds')
saveRDS(day_filtered, 'day.rds')
saveRDS(trip_filtered, 'trip.rds')
saveRDS(vehicle_filtered, 'vehicle.rds')
saveRDS(variable_list, 'variable_list.rds')
saveRDS(value_labels, 'value_labels.rds')
