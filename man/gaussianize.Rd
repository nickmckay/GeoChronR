% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{gaussianize}
\alias{gaussianize}
\title{Gaussianize}
\usage{
gaussianize(X, jitter = FALSE)
}
\arguments{
\item{X}{data matrix}

\item{jitter}{boolean variable ; if TRUE, add jitter to data to prevent ties}
}
\value{
gaussianized data matrix
}
\references{
Emile-Geay, J., and M. Tingley (2016), Inferring climate variability from nonlinear proxies: application to palaeo-enso studies, Climate of the Past, 12 (1), 31–50, doi:10.5194/cp- 12-31-2016.
}
\author{
Julien Emile-Geay

Nick McKay
}