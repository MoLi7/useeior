% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DisaggregateFunctions.R
\name{DisaggAllocations}
\alias{DisaggAllocations}
\title{Allocate values specified by the .yml disaggregation specs to the correct places in a disaggregated row/column of the Use/Make tables.}
\usage{
DisaggAllocations(
  model,
  disagg,
  allocPercentages,
  vectorToDisagg,
  domestic = FALSE
)
}
\arguments{
\item{model}{A complete EEIO model: a list with USEEIO model components and attributes.}

\item{disagg}{Specifications for disaggregating the current Table}

\item{allocPercentages}{Dataframe. A subset of the dataframe that contains the percentages to allocate to specific industry and commodity combinations in the disaggregated vector. Parameter use coordinated with @param VectorToDisagg.}

\item{vectorToDisagg}{String. A parameter to indicate what table and what part of that table is being disaggregated (e.g. "MakeCol" or "Intersection")}

\item{domestic.}{Boolean. Flag to indicate where to use the DomesticUse or UseTransactions table}
}
\value{
A dataframe with the values specified in the disaggSpecs assigned to the correct Make or Use table indeces.
}
\description{
Allocate values specified by the .yml disaggregation specs to the correct places in a disaggregated row/column of the Use/Make tables.
}
