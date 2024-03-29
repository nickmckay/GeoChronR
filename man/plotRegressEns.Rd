% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.geoChronR.R
\name{plotRegressEns}
\alias{plotRegressEns}
\title{Plot ensemble regression results}
\usage{
plotRegressEns(
  reg.ens,
  alp = 0.2,
  quantiles = c(0.025, 0.5, 0.975),
  font.size = 10
)
}
\arguments{
\item{reg.ens}{output of regressEns()}

\item{alp}{Transparency of the scatter plot.}

\item{quantiles}{quantiles to calculate and plot}

\item{font.size}{Font size}
}
\value{
A list of ggplot objects
\itemize{
\item YPlot - ribbon plot of the prectictand timeseries over the interval of overlap
\item XPlot - ribbon plot of the predictor timeseries over the interval of overlap
\item scatterplot - ensemble scatter plot of the predictor and predictand timeseries over the interval of overlap
\item mHist - distribution of ensemble regression slopes
\item bHist - distribution of ensemble regression intercepts
\item modeledYPlot - ribbon plot of values modeled by the ensemble regression, incorporating age uncertainty in both the regression and the predictor timeseries
\item summaryPlot - grid.arrange object of all the regression plots
}
}
\description{
Creates a suite of plots to characterize the results of an ensemble regression.
}
\seealso{
Other plot: 
\code{\link{plotChron}()},
\code{\link{plotChronEns}()},
\code{\link{plotChronEnsDiff}()},
\code{\link{plotCorEns}()},
\code{\link{plotHistEns}()},
\code{\link{plotLine}()},
\code{\link{plotModelDistributions}()},
\code{\link{plotPcaEns}()},
\code{\link{plotPvalsEnsFdr}()},
\code{\link{plotScatterEns}()},
\code{\link{plotScreeEns}()},
\code{\link{plotSpectraEns}()},
\code{\link{plotSpectrum}()},
\code{\link{plotSummary}()},
\code{\link{plotSummaryTs}()},
\code{\link{plotTimeseriesEnsLines}()},
\code{\link{plotTimeseriesEnsRibbons}()},
\code{\link{plotTimeseriesStack}()},
\code{\link{plotTrendLinesEns}()}

Other regress: 
\code{\link{plotScatterEns}()},
\code{\link{plotTrendLinesEns}()},
\code{\link{regress}()},
\code{\link{regressEns}()}
}
\author{
Nick McKay
}
\concept{plot}
\concept{regress}
