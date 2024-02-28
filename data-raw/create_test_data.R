

# Libraries ====================================================================
library(data.table)
library(tmrtools) # internal RSG package for working with the database
library(readxl)
library(stringr)
library(usethis)

# Survey Data ==================================================================
tbl_names = c("hh", "person", "day", "trip", "vehicle")

hts_data = list()

hts_data = lapply(tbl_names, function(t) {
  query = stringr::str_glue('select * from psrc_2023.d_ex_{t}')
  db_tab = tmrtools::read_from_db(con = connect_to_pops(dbname = 'psrc'),
                                  query,
                                  disconnect = TRUE)
})

names(hts_data) = tbl_names


## Filter to random 1000 households --------------------------------------------
ids_to_keep = sample(hts_data$hh$hh_id, size = 1000)

hts_data = lapply(hts_data, function(dt)
  dt[hh_id %in% ids_to_keep])

# Create fake home counties and lat/lng for geographic analysis
hts_data$hh[, home_county := sample(1:3, size = nrow(hts_data$hh), replace = TRUE)]

hts_data$hh[, home_lat := sample(
  seq(
    from = 33.00000,
    to = 40.00000,
    length.out = nrow(hts_data$hh)
  ),
  size = nrow(hts_data$hh),
  replace = TRUE
)]

hts_data$hh[, home_lon := sample(
  seq(
    from = -100.00000,
    to = -80.00000,
    length.out = nrow(hts_data$hh)
  ),
  size = nrow(hts_data$hh),
  replace = TRUE
)]

## Choose a subset of columns --------------------------------------------------
keep_cols = c(
  'hh_id',
  'person_id',
  'day_id',
  'trip_id',
  'vehicle_id',
  
  # Household variables:
  'sample_segment',
  'income_detailed',
  'income_followup',
  'num_people',
  'residence_type',
  'home_county',
  'home_lat',
  'home_lon',
  
  # Person variables:
  'race_1',
  'race_2',
  'race_3',
  'race_4',
  'race_5',
  'race_997',
  'race_999',
  'ethnicity_1',
  'ethnicity_2',
  'ethnicity_3',
  'ethnicity_4',
  'ethnicity_997',
  'ethnicity_999',
  'age',
  'gender',
  'employment',
  'education',
  'job_type',
  
  # Day variables:
  'day_id',
  'delivery_2',
  'delivery_3',
  'delivery_4',
  'delivery_5',
  'delivery_6',
  'delivery_7',
  'delivery_8',
  'delivery_996',
  'begin_day',
  'end_day',
  
  # Trip variables:
  'travel_date',
  'mode_type',
  'mode_1',
  'mode_2',
  'd_purpose_category',
  'num_trips',
  'speed_mph',
  'distance_miles',
  'num_travelers',
  
  # Vehicle variables:
  'fuel_type',
  
  # Weights
  'hh_weight',
  'person_weight',
  'day_weight',
  'trip_weight'
)

hts_data = lapply(hts_data, function(dt) {
  keep_dt = names(dt)[names(dt) %in% keep_cols]
  dt = dt[, ..keep_dt]
})

# randomize ids and dates
hts_data$hh[, hh_id := sample(1000, nrow(hts_data$hh))]

hts_data$person[, hh_id := sample(1000, nrow(hts_data$person), replace = TRUE)]
hts_data$person[, person_id := sample(nrow(hts_data$person), nrow(hts_data$person))]

hts_data$day[, person_id := sample(nrow(hts_data$person), nrow(hts_data$day), replace = TRUE)]
hts_data$day[, day_id := sample(nrow(hts_data$day), nrow(hts_data$day))]
hts_data$day[, hh_id := NULL]
hts_data$day[, travel_date := sample(unique(hts_data$day[, travel_date]), nrow(hts_data$day), replace = TRUE)]
hts_data$day = merge(hts_data$day,
                     hts_data$person[, c('hh_id', 'person_id')],
                     by = 'person_id',
                     all.x = TRUE)

hts_data$trip[, day_id := sample(nrow(hts_data$day), nrow(hts_data$trip), replace = TRUE)]
hts_data$trip[, trip_id := sample(nrow(hts_data$trip), nrow(hts_data$trip))]
hts_data$trip[, c('hh_id', 'person_id', 'travel_date') := NULL]
hts_data$trip = merge(hts_data$trip, hts_data$day[, c('hh_id', 'person_id', 'day_id', 'travel_date')], by = 'day_id', all.x = TRUE)


hts_data$vehicle[, hh_id := sample(1000, nrow(hts_data$vehicle), replace = TRUE)]
hts_data$vehicle[, vehicle_id := sample(nrow(hts_data$vehicle), nrow(hts_data$vehicle))]

# recalculate num_people
num_people = hts_data$person[, .(num_people = .N), 'hh_id']
hts_data$hh[, num_people := NULL]
hts_data$hh = merge(hts_data$hh, num_people, by = 'hh_id', all.x = TRUE)
hts_data$hh[, num_people := ifelse(is.na(num_people), 0, num_people)]

# recalculate num_trips
num_trips_hh = hts_data$trip[, .(num_trips = .N), 'hh_id']
hts_data$hh[, num_trips := NULL]
hts_data$hh = merge(hts_data$hh, num_trips_hh, by = 'hh_id', all.x = TRUE)
hts_data$hh[, num_trips := ifelse(is.na(num_trips), 0, num_trips)]

num_trips_person = hts_data$trip[, .(num_trips = .N), 'person_id']
hts_data$person[, num_trips := NULL]
hts_data$person = merge(hts_data$person,
                        num_trips_person,
                        by = 'person_id',
                        all.x = TRUE)
hts_data$person[, num_trips := ifelse(is.na(num_trips), 0, num_trips)]

num_trips_day = hts_data$trip[, .(num_trips = .N), 'day_id']
hts_data$day[, num_trips := NULL]
hts_data$day = merge(hts_data$day, num_trips_day, by = 'day_id', all.x = TRUE)
hts_data$day[, num_trips := ifelse(is.na(num_trips), 0, num_trips)]

# Codebook =====================================================================
codebook_path = file.path(
  tmrtools::find_project_root('PSRC'),
  '210252_PSRC_HTS/Internal/3.DataAnalysis/1.Data/Codebook',
  'PSRC_Combined_Codebook_2023_08162023_RSG.xlsx'
)

variable_list = readxl::read_xlsx(codebook_path,
                                  sheet = "ex_variable_list_2023")

setDT(variable_list)

value_labels = readxl::read_xlsx(codebook_path,
                                 sheet = "ex_value_labels_2023")

setDT(value_labels)


## Filter to those variables present in test dataset ---------------------------
variable_list = variable_list[variable %in% keep_cols]
value_labels = value_labels[variable %in% keep_cols]

# add fake home_county labels to value_labels and remove real ones
value_labels = value_labels[variable != 'home_county']

county_labels = data.frame(
  variable = rep('home_county', 3),
  value = 1:3,
  label = c('Arike County', 'Clark County', 'Moore County')
)

value_labels = rbind(value_labels, county_labels, fill = TRUE)
## Subset to minimum required columns ------------------------------------------
variable_list = variable_list[, c(
  'variable',
  'is_checkbox',
  'hh',
  'person',
  'day',
  'trip',
  'vehicle',
  'location',
  'description',
  'data_type'
)]

# Requires a shared_name column
variable_list[, shared_name :=
                ifelse(is_checkbox == 1,
                       sub('_[^_]*$', '', variable),
                       variable)]

# Requires a val_order column
value_labels[, val_order := seq(1:nrow(value_labels))]

value_labels = value_labels[, c('variable', 'value', 'label', 'val_order')]


# randomize person vars
randomize_vars = c('education', 'age', 'gender',
                   names(hts_data$person)[names(hts_data$person) %like% 'race|ethnicity'])

for (var in randomize_vars) {
  setnames(hts_data$person, var, 'var')
  x = value_labels[variable == var, as.integer(value)]
  if (var %like% 'race|ethnicity') {
    x = x[x != 995]
  }
  hts_data$person[, var := sample(x = x,
                                  size = nrow(hts_data$person),
                                  replace = TRUE)]
  setnames(hts_data$person, 'var', var)
}

# if race_999 = 1, make other race vals 0
hts_data$person[race_999 == 1, c('race_1', 'race_2', 'race_3', 'race_4', 'race_5', 'race_997') := 0]

# if ethnicity_999 = 1, make other ethnicity vals 0
hts_data$person[ethnicity_999 == 1, c('ethnicity_1',
                                      'ethnicity_2',
                                      'ethnicity_3',
                                      'ethnicity_4',
                                      'ethnicity_997') := 0]

## Create dummy weights --------------------------------------------------------
hts_data$hh[, hh_weight :=
              sample(10:1000,
                     size = nrow(hts_data$hh),
                     replace = TRUE)]
hts_data$person[, person_weight :=
                  sample(10:1000,
                         size = nrow(hts_data$person),
                         replace = TRUE)]
hts_data$day[, day_weight :=
               sample(10:1000,
                      size = nrow(hts_data$day),
                      replace = TRUE)]
hts_data$trip[, trip_weight :=
                sample(10:1000,
                       size = nrow(hts_data$trip),
                       replace = TRUE)]
hts_data$vehicle = merge(hts_data$vehicle,
                         hts_data$hh[, c('hh_id', 'hh_weight')],
                         by = 'hh_id',
                         all.x = TRUE)

# only keep variables in the codebook that remain in the dataset
variable_list = variable_list[variable %in% keep_cols]
variable_list[, location := NULL]
variable_list[variable == 'num_people', data_type := 'numeric']
variable_list[, shared_name := ifelse(grepl(':', description),
                                      sub('_[^_]*$', '', variable), variable)]

value_labels = value_labels[variable %in% keep_cols]

value_labels[, value := as.numeric(value)]

setorder(value_labels, variable, value)

value_labels = value_labels[, c('variable', 'value', 'label')]
value_labels = value_labels[variable != 'age' |
                              !grepl('Age', label)]
value_labels[, val_order := seq_len(nrow(value_labels))]

# remove keys created from merging for use in vignettes
setkey(hts_data$hh, NULL)
setkey(hts_data$person, NULL)
setkey(hts_data$day, NULL)
setkey(hts_data$trip, NULL)
setkey(hts_data$vehicle, NULL)

### Write data----

hh = hts_data$hh
usethis::use_data(hh, overwrite = TRUE)

person = hts_data$person
usethis::use_data(person, overwrite = TRUE)

day = hts_data$day
usethis::use_data(day, overwrite = TRUE)

trip = hts_data$trip
usethis::use_data(trip, overwrite = TRUE)

vehicle = hts_data$vehicle
usethis::use_data(vehicle, overwrite = TRUE)

test_data = hts_data
# Write data ===================================================================
usethis::use_data(test_data, overwrite = TRUE)
usethis::use_data(variable_list, overwrite = TRUE)
usethis::use_data(value_labels, overwrite = TRUE)