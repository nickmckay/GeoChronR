## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(fig.retina = 2, fig.width = 8,fig.height = 6) 
knitr::opts_knit$set(progress = FALSE,verbose = FALSE)


## ---- results = FALSE, warning = FALSE, message= FALSE------------------------

library(lipdR)
library(geoChronR)
library(magrittr)
library(dplyr)
library(purrr)

## -----------------------------------------------------------------------------
FD <- readLipd("http://lipdverse.org/geoChronR-examples/arc2k/Arctic2k.zip") 

## ----results="hide",fig.keep="all"--------------------------------------------
mapLipd(FD,map.type = "line",projection = "stereo",f = 0.1)

## ----results="hide"-----------------------------------------------------------
FD2 = purrr::map(FD,
                 mapAgeEnsembleToPaleoData,
                 strict.search = TRUE,
                 age.var = "ageEnsemble",
                 depth.var = NULL )

## -----------------------------------------------------------------------------
TS = extractTs(FD2)

## ----results="hide",fig.keep="all"--------------------------------------------
TS.filtered = filterTs(TS,"interpretation1_variable == T")

## ----results="hide",fig.keep="all"--------------------------------------------
tidyDf <- tidyTs(TS.filtered)

## -----------------------------------------------------------------------------
plotTimeseriesStack(tidyDf, 
                    color.var = "paleoData_variableName", 
                    color.ramp = c("DarkBlue","Orange","Black","Dark Green"),
                    line.size = .1, 
                    fill.alpha = .05,
                    lab.size = 2,
                    lab.space = 3)

## ----results="hide",cache=TRUE------------------------------------------------
binned.TS = binTs(TS.filtered,bin.vec = seq(1400,2000,by=5),time.var = "ageEnsemble")

## ----results="hide",warning=FALSE---------------------------------------------
pcout = pcaEns(binned.TS)

## -----------------------------------------------------------------------------
plotPCA = plotPcaEns(pcout,TS = TS.filtered,map.type = "line",projection = "stereo",bound.circ = T,restrict.map.range = T,f=.1,legend.position = c(0.5,.6),which.pcs = 1:2,which.leg = 2)

## -----------------------------------------------------------------------------
plotPCA$maps[[1]]

## -----------------------------------------------------------------------------
plotPCA$lines[[2]]

## -----------------------------------------------------------------------------
plotPCA$sampleDepth

## -----------------------------------------------------------------------------
var.names <- pullTsVariable(TS, "variableName")

## -----------------------------------------------------------------------------
var.names <- pullTsVariable(TS, "paleoData_variableName")

## -----------------------------------------------------------------------------
unique(var.names)

## ----results="hide"-----------------------------------------------------------
d18OTS = filterTs(TS,"paleoData_variableName == d18O")

## ----results="hide"-----------------------------------------------------------
tidyd18O <- tidyTs(d18OTS)


## -----------------------------------------------------------------------------
#arrange the tidy dataframe by record length
tidyd18O <- tidyd18O %>% 
  group_by(paleoData_TSid) %>% 
  mutate(range = max(year) - min(year)) %>% 
  arrange(range)


## -----------------------------------------------------------------------------
plotTimeseriesStack(tidyd18O, 
                    color.var = "paleoData_variableName", 
                    color.ramp = c("DarkBlue"),
                    line.size = .1, 
                    fill.alpha = .05,
                    lab.size = 2,
                    lab.space = 2,
                    lab.buff = 0.03)

## ----results="hide"-----------------------------------------------------------
binned.TS2 = binTs(d18OTS,bin.vec = seq(1400,2000,by=5),na.col.rm = T)

## ----results="hide"-----------------------------------------------------------
pcout2 = pcaEns(binned.TS2,pca.type = "cov")

## -----------------------------------------------------------------------------
plotPCA2 = plotPcaEns(pcout2,TS = d18OTS,map.type = "line",projection = "stereo",bound.circ = T,restrict.map.range = T,f=.2)

