#' Test household travel survey dataset
#'
#' @format ## `test_data`
#' A list containing the following datasets:
#'
#' @source A list of export tables compiled from internal RSG database on 11/22/2023
"test_data"
#' Test household dataset
#'
#' A dataset containing household level attributes of 1,000 households from the
#' 2023 Puget Sound Regional Council Household Travel Survey.
#' The variables are as follows:
#'
#' @format ## `hh`
#' A data frame with 1,000 rows and 6 columns:
#' \describe{
#'   \item{hh_id}{8 digit household ID}
#'   \item{sample_segment}{Sample segment}
#'   \item{income_detailed}{2022 household income (detailed categories), response codes}
#'   \item{income_followup}{2022 household income (broad categories), response codes}
#'   \item{num_people}{Household size, response codes}
#'   \item{residence_type}{Type of residence, response codes}
#' }
#' @source Export tables compiled from internal RSG database on 11/22/2023
"hh"

#' Test person dataset
#'
#' A dataset containing person level attributes of almost 2,000 persons from the
#' 2023 Puget Sound Regional Council Household Travel Survey.
#' The variables are as follows:
#'
#' @format ## `person`
#' A data frame with 1,999 rows and 19 columns:
#' \describe{
#'   \item{hh_id}{8 digit household ID}
#'   \item{person_id}{10 digit person ID}
#'   \item{ethnicity_1}{Not of Hispnic, Latino, or Spanish origin, response codes}
#'   \item{ethnicity_2}{Mexican, Mexican American, Chicano, response codes}
#'   \item{ethnicity_3}{Puerto Rican, response codes}
#'   \item{ethnicity_4}{Cuban, response codes}
#'   \item{ethnicity_997}{Another Hispanic, Latino, or Spanish origin, response codes}
#'   \item{ethnicity_999}{Prefer not to answer, response codes}
#'   \item{race_1}{African American or Black, response codes}
#'   \item{race_2}{Americian Indian or Alaska Native, response codes}
#'   \item{race_3}{Asian, response codes}
#'   \item{race_4}{Native Hawaiian or other Pacific Islander, response codes}
#'   \item{race_5}{White, response codes}
#'   \item{race_997}{Other race, response codes}
#'   \item{race_999}{Prefer not to answer, response codes}
#'   \item{age}{Age, response codes}
#'   \item{gender}{Gender, response codes}
#'   \item{employment}{Employment status, response codes}
#'   \item{education}{Highest level of education completed, response codes}
#' }
#' @source Export tables compiled from internal RSG database on 11/22/2023
"person"

#' Test day dataset
#'
#' A dataset containing information about participant travel days from the
#' 2023 Puget Sound Regional Council Household Travel Survey.
#' The variables are as follows:
#'
#' @format ## `day`
#' A data frame with 5,602 rows and 13 columns:
#' \describe{
#'   \item{hh_id}{8 digit household ID}
#'   \item{person_id}{10 digit person ID}
#'   \item{day_id}{12 digit day ID}
#'   \item{delivery_2}{Take-out/prepared food delivered to home, response codes}
#'   \item{delivery_3}{Someone came to do work at home (e.g., babysitter,
#'     housecleaning, lawn), response codes}
#'   \item{delivery_4}{Groceries delivered to home, response codes}
#'   \item{delivery_5}{Received packages at home (e.g., USPS, FedEx, UPS), response codes}
#'   \item{delivery_6}{Received personal packages at work, response codes}
#'   \item{delivery_7}{Received packages at another location (e.g.,
#'     Amazon Locker, package pick-up point), response codes}
#'   \item{delivery_8}{Other item delivered to home (e.g., appliance), response codes}
#'   \item{delivery_996}{None of the above, response codes}
#'   \item{begin_day}{Location at the beginning of the day, response codes}
#'   \item{end_day}{Location at the end of the day, response codes}
#' }
#' @source Export tables compiled from internal RSG database on 11/22/2023
"day"

#' Test trip dataset
#'
#' A dataset containing attributes of over 20,000 trips from the
#' 2023 Puget Sound Regional Council Household Travel Survey.
#' The variables are as follows:
#'
#' @format ## `trip`
#' A data frame with 21,378 rows and 7 columns:
#' \describe{
#'   \item{hh_id}{8 digit household ID}
#'   \item{person_id}{10 digit person ID}
#'   \item{day_id}{12 digit day ID}
#'   \item{trip_id}{13 digit trip ID}
#'   \item{travel_date}{Date of trip}
#'   \item{mode_type}{Type of transportation used for trip, response codes}
#'   \item{d_purpose_category}{Purpose for taking trip to destination, response codes}
#' }
#' @source Export tables compiled from internal RSG database on 11/22/2023
"trip"

#' Test vehicle dataset
#'
#' A dataset containing attributes of over 6,500 vehicles from the
#' 2023 Puget Sound Regional Council Household Travel Survey.
#' The variables are as follows:
#'
#' @format ## `vehicle`
#' A data frame with 1,366 rows and 3 columns:
#' \describe{
#'   \item{hh_id}{8 digit household ID}
#'   \item{vehicle_id}{10 digit vehicle ID}
#'   \item{fuel_type}{Type of fuel taken by vehicle, response codes}
#' }
#' @source Export tables compiled from internal RSG database on 11/22/2023
"vehicle"

#' List of variables
#'
#' A dataset containing information about all variables in data. This must contain one binary column indicating table location for each table in the data (ie., hh, person, day, trip, vehicle).
#'
#' @format ## `variable_list`
#' A data frame with 55 rows and 10 columns:
#' \describe{
#'   \item{variable}{Name of the variable}
#'   \item{is_checkbox}{The variable is a multiple response categorical variable question}
#'   \item{hh}{The variable exists in the hh table}
#'   \item{person}{The variable exists in the person table}
#'   \item{day}{The variable exists in the day table}
#'   \item{trip}{The variable exists in the trip table}
#'   \item{vehicle}{The variable exists in the vehicle table}
#'   \item{data_type}{Data type of the variable ("iteger/categorical", "numeric", "character")}
#'   \item{description}{A description of the variable}
#'   \item{shared_name}{Shared part of name for checkbox variable (e.g., race_1 -> race) or variable name (e.g., age -> age)}
#' }
"variable_list"

#' List of values and their labels
#'
#' A dataset containing the values for all variables found in variable_list
#' The variables are as follows:
#'
#' @format ## `value_labels`
#' A data frame with 210 rows and 3 columns:
#' \describe{
#'   \item{variable}{Name of the variable}
#'   \item{value}{The numeric value of the variable}
#'   \item{label}{What the numeric value of the variable represents}
#' }
"value_labels"
