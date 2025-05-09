% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{generateEnsembleFromUncertainty}
\alias{generateEnsembleFromUncertainty}
\title{Create an ensemble from an uncertainty and AR1 estimate}
\usage{
generateEnsembleFromUncertainty(
  variable,
  n.ens,
  sd,
  ar = sqrt(0.5),
  arima.order = c(1, 0, 0)
)
}
\arguments{
\item{variable}{LiPD variable object or vector of data}

\item{n.ens}{}

\item{sd}{}

\item{ar}{}

\item{arima.order}{}
}
\value{
a matrix of ensemble values
}
\description{
Create an ensemble from an uncertainty and AR1 estimate
}
