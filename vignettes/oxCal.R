## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(geoChronR)
library(lipdR)
library(oxcAAR)
library(dplyr)

## -----------------------------------------------------------------------------
 L <- readLipd("http://wiki.linked.earth/wiki/index.php/Special:WTLiPD?op=export&lipdid=BJ8-03-70GGC.Linsley.2010")

## -----------------------------------------------------------------------------

L <- runOxcal(L,
              lab.id.var = 'id', 
              age.14c.var = 'age14c', 
              age.14c.uncertainty.var = 'age14cuncertainty', 
              age.var = 'calendarage', 
              age.uncertainty.var = 'calendarageuncertainty', 
              depth.var = 'depth', 
              reservoir.age.14c.var = NULL, 
              reservoir.age.14c.uncertainty.var = NULL, 
              rejected.ages.var = NULL,
              static.reservoir.age = 70,
              static.reservoir.age.unc = 50,
              oxcal.path = "~/newOxCal",
              surface.age = 0,
              surface.age.unc = 25,
              depth.interval = 10,
              events.per.unit.length = .1)
              
                
                
              

## -----------------------------------------------------------------------------
plotChronEns(L)

## -----------------------------------------------------------------------------
L <- mapAgeEnsembleToPaleoData(L,age.var = "ageEnsemble")

## -----------------------------------------------------------------------------
ageEns <- selectData(L,var.name = "ageEnsemble")
SST <- selectData(L,var.name = "SST")


plotTimeseriesEnsRibbons(X = ageEns, Y = SST) %>% 
  plotTimeseriesEnsLines(X = ageEns, Y = SST,n.ens.plot = 10)

