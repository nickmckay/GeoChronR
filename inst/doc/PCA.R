## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 8,fig.height = 6) 
knitr::opts_knit$set(progress = FALSE,verbose = FALSE)


## ---- results = FALSE, warning = FALSE, message= FALSE------------------------
library(lipdR)
library(geoChronR)
library(magrittr)
library(dplyr)
library(purrr)

## -----------------------------------------------------------------------------
  FD <- lipdR::readLipd("http://lipdverse.org/geoChronR-examples/arc2k/Arctic2k.zip") 

## ----results="hide",fig.keep="all"--------------------------------------------
mapLipd(FD,map.type = "line",projection = "stereo",f = 0.1)

## ----results="hide",message=FALSE---------------------------------------------
FD2 = purrr::map(FD,
                 mapAgeEnsembleToPaleoData,
                 strict.search = TRUE,
                 age.var = "ageEnsemble",
                 depth.var = NULL )

## -----------------------------------------------------------------------------
TS <- extractTs(FD2)

## ----results="hide",fig.keep="all"--------------------------------------------
TS.filtered <- filterTs(TS,"interpretation1_variable == T")

## ----results="hide",fig.keep="all"--------------------------------------------
tidyDf <- lipdR::tidyTs(TS.filtered,age.var = "year")

## -----------------------------------------------------------------------------
plotTimeseriesStack(tidyDf, 
                    color.var = "paleoData_variableName", 
                    color.ramp = c("DarkBlue","Orange","Black","Dark Green"),
                    line.size = .1, 
                    fill.alpha = .05,
                    lab.size = 2,
                    lab.space = 3)

## ----results="hide",cache=TRUE------------------------------------------------
binned.TS <- binTs(TS.filtered,bin.vec = seq(1400,2000,by=5),time.var = "ageEnsemble")

## ----results="hide",warning=FALSE---------------------------------------------
pcout <- pcaEns(binned.TS)

## -----------------------------------------------------------------------------
plotScreeEns(pcout)

## -----------------------------------------------------------------------------
plotPCA <-  plotPcaEns(pcout,TS = TS.filtered,map.type = "line",projection = "stereo",bound.circ = T,restrict.map.range = T,f=.1,legend.position = c(0.5,.6),which.pcs = 1:2,which.leg = 2)

## -----------------------------------------------------------------------------
var.names <- pullTsVariable(TS, "variableName")

## -----------------------------------------------------------------------------
var.names <- pullTsVariable(TS, "paleoData_variableName")

## -----------------------------------------------------------------------------
unique(var.names)

## ----results="hide"-----------------------------------------------------------
d18OTS = filterTs(TS,"paleoData_variableName == d18O")

## ----results="hide"-----------------------------------------------------------
tidyd18O <- tidyTs(d18OTS,age.var = "year")


## -----------------------------------------------------------------------------
#arrange the tidy dataframe by record length
tidyd18O <- tidyd18O %>% #use the magrittr pipe for clarity
  group_by(paleoData_TSid) %>% #group the data by column
  mutate(duration = max(year) - min(year)) %>% #create a new column for the duration
  arrange(duration) # and arrange the data by duration


## -----------------------------------------------------------------------------
plotTimeseriesStack(tidyd18O, 
                    color.var = "paleoData_variableName", # Color the data by the variable name (all the same in the case)
                    color.ramp = "DarkBlue", #colors to use
                    line.size = .1, 
                    fill.alpha = .05,
                    lab.size = 2,
                    lab.space = 2,
                    lab.buff = 0.03)

## ----results="hide"-----------------------------------------------------------
binned.TS2 <- binTs(d18OTS,bin.vec = seq(1400,2000,by=5),na.col.rm = T)

## ----results="hide"-----------------------------------------------------------
pcout2 <- pcaEns(binned.TS2,pca.type = "cov")

## ----fig.width = 4,fig.height = 4---------------------------------------------
plotScreeEns(pcout2)

## ----fig.width = 8,fig.height = 6---------------------------------------------
plotPCA2 <-  plotPcaEns(pcout2,
                        TS = d18OTS,
                        which.pcs = 1:3,  
                        map.type = "line",
                        projection = "stereo",
                        bound.circ = T,
                        restrict.map.range = T,
                        f=.2)

