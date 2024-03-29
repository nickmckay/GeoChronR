% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.geoChronR.R
\name{periodAnnotate}
\alias{periodAnnotate}
\title{Annotate plot of spectra with given periodicities}
\usage{
periodAnnotate(
  specPlot,
  periods,
  color = "orange",
  log10scale = T,
  y.lims = NULL,
  size = 4
)
}
\arguments{
\item{specPlot}{ggplot handle to figure containing spectrum}

\item{periods}{the periods to highlight in the spectrum}

\item{color}{the color of the text and lines}

\item{log10scale}{Use a log 10 scale TRUE (default) or FALSE}

\item{y.lims}{optionally specify y limits (default = NULL)}

\item{size}{font size for labels (default = 4)}
}
\value{
ggplot object of spectrum plot
}
\description{
Annotate plot of spectra (ensemble or otherwise) with vertical lines at specific periodicities (assumes log10 scaling)
}
\seealso{
Other plot help: 
\code{\link{AD2BP_trans}()},
\code{\link{BP2AD_trans}()},
\code{\link{axisLabel}()},
\code{\link{bin2d}()},
\code{\link{geoChronRPlotTheme}()},
\code{\link{getLegend}()},
\code{\link{getPlotRanges}()},
\code{\link{kde2d}()},
\code{\link{meltDistributionTable}()},
\code{\link{modeSelektor}()},
\code{\link{quantile2d}()},
\code{\link{reverselog10_trans}()}

Other spectra: 
\code{\link{ar1Surrogates}()},
\code{\link{computeSpectraEns}()},
\code{\link{createSyntheticTimeseries}()},
\code{\link{plotSpectrum}()},
\code{\link{reverselog10_trans}()}
}
\author{
Julien Emile-Geay
}
\concept{plot help}
\concept{spectra}
