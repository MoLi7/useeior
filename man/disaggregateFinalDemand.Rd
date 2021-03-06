% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DisaggregateFunctions.R
\name{disaggregateFinalDemand}
\alias{disaggregateFinalDemand}
\title{Disaggregate Final Demand based on specs}
\usage{
disaggregateFinalDemand(model, domestic = FALSE)
}
\arguments{
\item{model}{A complete EEIO model: a list with USEEIO model components and attributes.}
}
\value{
A standardized final demand table with old sectors removed and new sectors with manual and default allocations added.
}
\description{
Disaggregate Final Demand based on specs
}
