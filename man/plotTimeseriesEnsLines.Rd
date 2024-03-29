% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.geoChronR.R
\name{plotTimeseriesEnsLines}
\alias{plotTimeseriesEnsLines}
\title{Plot an ensemble timeseries as a set of lines}
\usage{
plotTimeseriesEnsLines(
  add.to.plot = ggplot(),
  X,
  Y,
  alp = 0.2,
  color = "blue",
  n.ens.plot = 100,
  na.rm = TRUE
)
}
\arguments{
\item{add.to.plot}{A ggplot object to add these lines to. Default is ggplot() .}

\item{X}{A LiPD variable list to plot, including values, units, names, and more}

\item{Y}{A LiPD variable list to plot, including values, units, names, and more}

\item{alp}{Line transparency}

\item{color}{Either 1) A line color (following ggplot rules) to use for all lines (e.g., "blue"), 2) An RColorBrewer pallette to repeat over the lines (e.g. "Blues") or 3) a vector specifying the color for all lines (e.g., c("red","white","blue"))}

\item{n.ens.plot}{Whats the maximum number of lines to plot?}

\item{na.rm}{Remove NAs from X and Y? Set to FALSE to preserve line breaks where data are missing. (default = TRUE)}
}
\value{
A ggplot object
}
\description{
Plot an ensemble timeseries as a set of lines. Useful for displaying a handful of ensemble members to characterize individual paths.
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
\code{\link{plotRegressEns}()},
\code{\link{plotScatterEns}()},
\code{\link{plotScreeEns}()},
\code{\link{plotSpectraEns}()},
\code{\link{plotSpectrum}()},
\code{\link{plotSummary}()},
\code{\link{plotSummaryTs}()},
\code{\link{plotTimeseriesEnsRibbons}()},
\code{\link{plotTimeseriesStack}()},
\code{\link{plotTrendLinesEns}()}
}
\author{
Nick McKay
}
\concept{plot}
