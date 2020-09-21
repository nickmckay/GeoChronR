## ---- results = FALSE, warning = FALSE, message= FALSE------------------------
library(lipdR)
library(geoChronR)
library(ggplot2)
library(tidyverse)

## ---- results = 'hide', cache=TRUE--------------------------------------------
iso <- readLipd("http://lipdverse.org/iso2k/1_0_0/iso2k1_0_0.zip")

## -----------------------------------------------------------------------------
mapLipd(iso, global = TRUE,size = 3) + ggtitle("iso2k")

## -----------------------------------------------------------------------------
TS <- extractTs(iso)

## -----------------------------------------------------------------------------

iTS <- filterTs(TS, "paleoData_iso2kPrimaryTimeseries == TRUE")


## -----------------------------------------------------------------------------
lat <- pullTsVariable(iTS,"geo_latitude")
lon <- pullTsVariable(iTS,"geo_longitude")
var.name <- pullTsVariable(iTS,"paleoData_variableName")

index <- which(between(lat,10,50) & between (lon,70,170) & var.name == "d18O")

gTS <- iTS[index]

## -----------------------------------------------------------------------------
mapTs(gTS)

## -----------------------------------------------------------------------------
plotTimeAvailabilityTs(gTS,age.range = c(1,2000),age.var = "year")

## -----------------------------------------------------------------------------
summaryPlotTs(gTS,age.var = "year", age.range = c(0,2000))

