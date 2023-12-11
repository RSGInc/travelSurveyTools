#' Test household dataset
#'
#' A dataset containing household level attributes of 1,000 households.
#' The variables are as follows:
#'
#' @format ## `hh`
#' A data frame with 1,000 rows and 6 columns:
#' \describe{
#'   \item{hh_id}{8 digit household ID}
#'   \item{sample_segment}{Sample segment}
#'   \item{income_detailed}{2022 household income (detailed categories)}
#'   \item{income_followup}{2022 household income (broad categories)}
#'   \item{num_people}{Household size}
#'   \item{residence_type}{Type of residence}
#' }
"hh"

#' Test person dataset
#'
#' A dataset containing person level attributes of almost 2,000 persons.
#' The variables are as follows:
#'
#' @format ## `person`
#' A data frame with 1,999 rows and 19 columns:
#' \describe{
#'   \item{hh_id}{8 digit household ID}
#'   \item{person_id}{10 digit person ID}
#'   \item{ethnicity_1}{Not of Hispnic, Latino, or Spanish origin}
#'   \item{ethnicity_2}{Mexican, Mexican American, Chicano}
#'   \item{ethnicity_3}{Puerto Rican}
#'   \item{ethnicity_4}{Cuban}
#'   \item{ethnicity_997}{Another Hispanic, Latino, or Spanish origin}
#'   \item{ethnicity_999}{Prefer not to answer}
#'   \item{race_1}{African American or Black}
#'   \item{race_2}{Americian Indian or Alaska Native}
#'   \item{race_3}{Asian}
#'   \item{race_4}{Native Hawaiian or other Pacific Islander}
#'   \item{race_5}{White}
#'   \item{race_997}{Other race}
#'   \item{race_999}{Prefer not to answer}
#'   \item{age}{Age}
#'   \item{gender}{Gender}
#'   \item{employment}{Employment status}
#'   \item{education}{Highest level of education completed}
#' }
"person"

#' Test day dataset
#'
#' A dataset containing information about participant travel days.
#' The variables are as follows:
#'
#' @format ## `day`
#' A data frame with 5,602 rows and 13 columns:
#' \describe{
#'   \item{hh_id}{8 digit household ID}
#'   \item{person_id}{10 digit person ID}
#'   \item{day_id}{12 digit day ID}
#'   \item{delivery_2}{Take-out/prepared food delivered to home}
#'   \item{delivery_3}{Someone came to do work at home (e.g., babysitter,
#'     housecleaning, lawn)}
#'   \item{delivery_4}{Groceries delivered to home}
#'   \item{delivery_5}{Received packages at home (e.g., USPS, FedEx, UPS)}
#'   \item{delivery_6}{Received personal packages at work}
#'   \item{delivery_7}{Received packages at another location (e.g.,
#'     Amazon Locker, package pick-up point)}
#'   \item{delivery_8}{Other item delivered to home (e.g., appliance)}
#'   \item{delivery_996}{None of the above}
#'   \item{begin_day}{Location at the beginning of the day}
#'   \item{end_day}{Location at the end of the day}
#' }
"day"

#' Test trip dataset
#'
#' A dataset containing attributes of over 20,000 trips.
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
#'   \item{mode_type}{Type of transportation used for trip}
#'   \item{d_purpose_category}{Purpose for taking trip to destination}
#' }
"trip"

#' Test vehicle dataset
#'
#' A dataset containing attributes of over 6,500 vehicles.
#' The variables are as follows:
#'
#' @format ## `vehicle`
#' A data frame with 1,366 rows and 3 columns:
#' \describe{
#'   \item{hh_id}{8 digit household ID}
#'   \item{vehicle_id}{10 digit vehicle ID}
#'   \item{fuel_type}{Type of fuel taken by vehicle}
#' }
"vehicle"

#' List of variables
#'
#' A dataset containing information about all variables existing in the hh, person,
#'  day, trip, and vehicle tables. The variables are as follows:
#'
#' @format ## `variable_list`
#' A data frame with 41 rows and 13 columns:
#' \describe{
#'   \item{order}{The order the variables are presented in}
#'   \item{source}{Where the variable was created}
#'   \item{variable}{Name of the variable}
#'   \item{is_checkbox}{The variable is a 'Select all that Apply' question}
#'   \item{hh}{The variable exists in the hh table}
#'   \item{person}{The variable exists in the person table}
#'   \item{day}{The variable exists in the day table}
#'   \item{trip}{The variable exists in the trip table}
#'   \item{vehicle}{The variable exists in the vehicle table}
#'   \item{data_type}{Data type of the variable}
#'   \item{description}{A description of the variable}
#'   \item{logic}{Conditions where the variable should have a value}
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