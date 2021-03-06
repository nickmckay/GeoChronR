% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping.functions.R
\name{plotTimeAvailabilityTs}
\alias{plotTimeAvailabilityTs}
\title{Create a Time availability plot}
\usage{
plotTimeAvailabilityTs(
  TS,
  age.range = NA,
  age.var = "age",
  group.var = "archiveType",
  step = NA
)
}
\arguments{
\item{TS}{a lipd TS object. This should probably be filtered to show just records you're interested in, as all we counted}

\item{age.range}{a two element vector that includes the range over which you want to count (default = NA, which calculates the full range in the TS)}

\item{age.var}{What age variable should be used (default = "age")}

\item{group.var}{What variable Should be used to Group the counts? (default = "archiveType")}

\item{step}{What step size should be used along the age axis? (default = NA, which will calculate 0.5\% of the age.range)}
}
\value{
a ggplot object
}
\description{
Create a Time availability plot
}
