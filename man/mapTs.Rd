% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping.functions.R
\name{mapTs}
\alias{mapTs}
\title{Map a TS object}
\usage{
mapTs(
  TS,
  color = "archiveType",
  size = 6,
  shape = 16,
  lat.range = NA,
  lon.range = NA,
  ...
)
}
\arguments{
\item{TS}{A list of LiPD objects}

\item{color}{variable (in TS) by which to color, or a string specifying a static ggplot color}

\item{size}{Size of the location marker}

\item{shape}{variable (in TS) by which to adjust shape, or an integer specifying a static ggplot shape}

\item{lat.range}{Latitudes to use to create baseMap range (default = NA, which determines from TS)}

\item{lon.range}{Longitudes to use to create baseMap range (default = NA, which determines from TS)}

\item{...}{
  Arguments passed on to \code{\link[=baseMap]{baseMap}}
  \describe{
    \item{\code{lat}}{latitude(s) range to map}
    \item{\code{lon}}{longitude(s) range to map}
    \item{\code{map.type}}{"stamen" or "line"}
    \item{\code{f}}{buffer for the map range}
    \item{\code{restrict.map.range}}{TRUE or FALSE. Trim the size of the map to the points, for "line" map type}
    \item{\code{bound.circ}}{Draw a boundary circle around a polar projection. TRUE or FALSE(default).}
    \item{\code{global}}{Should the scope of the map be global? TRUE or FALSE(default).}
    \item{\code{projection}}{Map project. All options on: ?mapproject}
    \item{\code{extend.range}}{increase the span of the map by this much (lat/long degrees)}
  }}
}
\value{
ggmap object
}
\description{
Create a stamen or line map of the location of a list of LiPD objects
}
\section{Long-form example}{

\href{http://nickmckay.github.io/GeoChronR/articles/TsFilteringAndMapping.html}{View a full-fledged example of how to use this function.}
}

\seealso{
Other mapping: 
\code{\link{baseMap}()},
\code{\link{mapLipd}()}
}
\author{
Nick McKay
}
\concept{mapping}
