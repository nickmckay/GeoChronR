% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.geoChronR.R
\name{plotPvalsEnsFdr}
\alias{plotPvalsEnsFdr}
\title{Plot the the p-values of an ensemble correlation analysis in a rank-pvalue plot}
\usage{
plotPvalsEnsFdr(cor.df, alpha = 0.05)
}
\arguments{
\item{cor.df}{A data.frame correlation r and p-values. Output from corEns()}

\item{alpha}{probability threshold}
}
\value{
A ggplot object
}
\description{
Plots the output of an ensemble correlation analysis as a rank-pvalue plot
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
}
\author{
Julien Emile-Geay
}
\concept{plot}
