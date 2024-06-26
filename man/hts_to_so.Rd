% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hts_to_so.R
\name{hts_to_so}
\alias{hts_to_so}
\title{Transform travel survey data.table to a survey object}
\usage{
hts_to_so(prepped_dt, weighted = TRUE, wtname = NULL, strataname = NULL)
}
\arguments{
\item{prepped_dt}{Dataframe in data.table format to transform to survey object.}

\item{weighted}{If true creates a weighted survey object. Default is TRUE.}

\item{wtname}{Name of the weight column in the dataframe. Defaults to NULL,
but must exist if weighted is true.}

\item{strataname}{Name of strata name to bring in. Default is NULL.}
}
\value{
Inputted dataframe transformed into a survey object.
}
\description{
Transform travel survey data.table to a survey object
}
\examples{

require(data.table)
hts_to_so(prepped_dt = trip, wtname = "trip_weight")

}
