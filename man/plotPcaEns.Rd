% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.geoChronR.R
\name{plotPcaEns}
\alias{plotPcaEns}
\title{Map ensemble pca loadings and plot PC timeseries}
\usage{
plotPcaEns(
  ens.pc.out,
  TS,
  map.type = "line",
  which.pcs = c(1, 2),
  f = 0.2,
  high.color = "red",
  low.color = "blue",
  dot.size = 5,
  restrict.map.range = TRUE,
  shape.by.archive = TRUE,
  projection = "mollweide",
  bound.circ = TRUE,
  probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
  which.leg = 1,
  legend.position = c(0.5, 0.5),
  color
)
}
\arguments{
\item{ens.pc.out}{results of pcaEns()}

\item{TS}{Timeseries object \url{http://nickmckay.github.io/LiPD-utilities/r/index.html#what-is-a-time-series} used in the pcaEns() analysis}

\item{map.type}{"google" or "line"}

\item{which.pcs}{vector of PCs to plot. Choose two. c(1,2) is default.}

\item{f}{zoom buffer for plotting}

\item{high.color}{color for the high end of the scale}

\item{low.color}{color for the low end of the scale}

\item{dot.size}{How big are the dots on the map}

\item{restrict.map.range}{TRUE or FALSE. Trim the size of the map to the points, for "line" map type}

\item{shape.by.archive}{TRUE or FALSE. Use archiveType to assign shapes.}

\item{projection}{Map project. All options on: ?mapproject}

\item{bound.circ}{For polar projects, draw a boundary circle? TRUE or FALSE}

\item{probs}{quantiles to calculate and plot in the PC timeseries}

\item{which.leg}{which map legend to include in the summary plot?}

\item{legend.position}{Where to put the map legend?}

\item{color}{deprecated. Use high.color and low.color instead}
}
\value{
A gridExtra ggplot object
}
\description{
Map ensemble pca loadings and plot PC timeseries
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
\code{\link{plotPvalsEnsFdr}()},
\code{\link{plotRegressEns}()},
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

Other pca: 
\code{\link{ar1Surrogates}()},
\code{\link{createSyntheticTimeseries}()},
\code{\link{pcaEns}()},
\code{\link{plotScreeEns}()}
}
\author{
Nick McKay
}
\concept{pca}
\concept{plot}
