% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hts_prep_triprate.R
\name{hts_prep_triprate}
\alias{hts_prep_triprate}
\title{Prepare datasets for trip rate calculations}
\usage{
hts_prep_triprate(
  summarize_by = NULL,
  variables_dt = variable_list,
  trip_name = "trip",
  day_name = "day",
  ids = c("hh_id", "person_id", "day_id", "trip_id", "vehicle_id"),
  wts = c("hh_weight", "person_weight", "day_weight", "trip_weight", "hh_weight"),
  remove_outliers = FALSE,
  threshold = 0.975,
  weighted = TRUE,
  strataname = NULL,
  hts_data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle)
)
}
\arguments{
\item{summarize_by}{Name of the variable to summarize trip rates by. Default
is NULL.}

\item{variables_dt}{List of variable locations and descriptions in data.table
format.}

\item{trip_name}{Name of the trip dataset in hts_data.}

\item{day_name}{Name of the day dataset in hts_data.}

\item{ids}{name of unique identifier in each table in hts_data}

\item{wts}{name of weight column in each table in hts_data}

\item{remove_outliers}{Boolean whether or not to remove outliers from dataset.
Default is TRUE.}

\item{threshold}{Threshold to define outliers. Default is 0.975.}

\item{weighted}{Whether the data is weighted. Default is TRUE.}

\item{strataname}{Name of strata name to bring in. Default is NULL.}

\item{hts_data}{List containing household, person, day, trip, and vehicle
datasets in data.table format.}
}
\value{
List of binned number of trips with key columns and summarize by variable,
unbinned number of trips with key columns and summarize by variable, and a
breakdown of outliers if removed.
}
\description{
Prepare datasets for trip rate calculations
}
\examples{

require(data.table)
require(stringr)
hts_prep_triprate(
  variables_dt = variable_list,
  trip_name = "trip",
  day_name = "day",
  hts_data = list(
    "hh" = hh,
    "person" = person,
    "day" = day,
    "trip" = trip,
    "vehicle" = vehicle
  )
)
hts_prep_triprate(
  summarize_by = "age",
  variables_dt = variable_list,
  trip_name = "trip",
  day_name = "day",
  hts_data = list(
    "hh" = hh,
    "person" = person,
    "day" = day,
    "trip" = trip,
    "vehicle" = vehicle
  )
)
}
