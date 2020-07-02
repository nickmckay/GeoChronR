## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(fig.retina = 2, fig.width = 8,fig.height = 6) 

## ---- results = FALSE, warning = FALSE, message= FALSE------------------------
library(lipdR) #to read and interact with LiPD data
library(geoChronR) #for plotting mostly
library(magrittr) #we'll be using the magrittr pipe ( %>% ) for simplicity
library(dplyr) #and dplyr for data.frame manipulation
library(ggplot2) #for plotting

## ---- results='hide'----------------------------------------------------------
D <- readLipd("http://lipdverse.org/geoChronR-examples/euro/Euro2k.zip") 

## -----------------------------------------------------------------------------
TS <- extractTs(D)

## ---- results='hide'----------------------------------------------------------
mts <- filterTs(TS, "paleoData_useInGlobalTemperatureAnalysis == TRUE") %>% 
  filterTs("geo_longitude > 0") %>% 
  filterTs("geo_latitude > 20")

## ---- results='hide'----------------------------------------------------------
tidyData <- tidyTs(mts)

## -----------------------------------------------------------------------------
#filter for plotting
plot.df <- tidyData %>% 
  filter(between(year,1600,2000)) %>% #only years from 1600 to 2000
  filter(between(geo_longitude,0,50)) %>%  #only European longitudes
  filter(between(geo_latitude,20,60)) %>%  #between 20 and 60 N
  filter(interpretation1_variable == "T") %>% #only variables sensitive temperature
  group_by(paleoData_TSid) %>% #group by column
  arrange(archiveType) #and sort by archiveType

## -----------------------------------------------------------------------------
#plot the stack and color by Archive type
plotTimeseriesStack(plot.df)

