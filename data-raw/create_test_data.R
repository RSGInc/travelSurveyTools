
# Libraries ====================================================================
library(data.table)
library(tmrtools) # internal RSG package for working with the database
library(readxl)

# Survey Data ==================================================================
tbl_names = c("hh", "person", "day", "trip", "vehicle")

hts_data = list()

hts_data = lapply(tbl_names, function(t) {
  query = stringr::str_glue('select * from psrc_2023.d_ex_{t}')
  db_tab = read_from_db(con = connect_to_pops(dbname = 'psrc'),
                        query,
                        disconnect = TRUE)
})

names(hts_data) = tbl_names


## Filter to random 1000 households --------------------------------------------
ids_to_keep = sample(hts_data$hh$hh_id, size = 1000)

hts_data = lapply(hts_data, function(dt) dt[hh_id %in% ids_to_keep])


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

  # Person variables:
  'race_1',
  'race_2',
  'race_3',
  'race_4',
  'race_5',
  'race_997',
  'race_999',
  'age',
  'gender',
  'employment',
  'education',

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
  'd_purpose_category',

  # Vehicle variables:
  'fuel_type'
)

hts_data = lapply(hts_data, function(dt) {
  keep_dt = names(dt)[names(dt) %in% keep_cols]
  dt = dt[, ..keep_dt]
})



# Codebook =====================================================================
codebook_path = file.path(
  tmrtools::find_project_root('PSRC'),
  '210252_PSRC_HTS/Internal/3.DataAnalysis/1.Data/Codebook',
  'PSRC_Combined_Codebook_2023_08162023_RSG.xlsx'
)

variable_list = readxl::read_xlsx(
  codebook_path,
  sheet = "ex_variable_list_2023"
)

setDT(variable_list)

value_labels = readxl::read_xlsx(
  codebook_path,
  sheet = "ex_value_labels_2023"
)

setDT(value_labels)


## Filter to those variables present in test dataset ---------------------------
variable_list = variable_list[variable %in% keep_cols]
value_labels = value_labels[variable %in% keep_cols]


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
  'description'
)]

value_labels = value_labels[, c('variable', 'value', 'label')]


# Write data ===================================================================
usethis::use_data(hts_data, overwrite = TRUE)
usethis::use_data(variable_list, overwrite = TRUE)
usethis::use_data(value_labels, overwrite = TRUE)
