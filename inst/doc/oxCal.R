## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(fig.width = 8,fig.height = 6) 

## ----message=FALSE,warning=FALSE,results='hide'-------------------------------
library(geoChronR)
library(lipdR)
library(oxcAAR)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
quickSetupOxcal(path = "~/OxCal")


## -----------------------------------------------------------------------------
 L <- readLipd("http://wiki.linked.earth/wiki/index.php/Special:WTLiPD?op=export&lipdid=BJ8-03-70GGC.Linsley.2010")

## ----message=FALSE,warning=FALSE----------------------------------------------
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
              oxcal.path = "~/OxCal",
              surface.age = 0,
              surface.age.unc = 25,
              depth.interval = 10,
              events.per.unit.length = .1)

## ----message=FALSE,warning=FALSE----------------------------------------------
plotChronEns(L)

## -----------------------------------------------------------------------------
L <- mapAgeEnsembleToPaleoData(L,age.var = "ageEnsemble")

## ----results='hide',message=FALSE---------------------------------------------
ageEns <- selectData(L,var.name = "ageEnsemble")
SST <- selectData(L,var.name = "SST")

## -----------------------------------------------------------------------------
plotTimeseriesEnsRibbons(X = ageEns, Y = SST) %>% 
  plotTimeseriesEnsLines(X = ageEns, Y = SST,n.ens.plot = 10)

