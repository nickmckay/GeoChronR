#temporary script that loads in all functions in LiPDR and GeoChronR
#will be replaced by package loader soon

#load LiPD Package functions
cur.dir=getwd()
#setwd("~/GitHub/lipdR/R/")
setwd("~/Documents/Science/Research/GeoChronR/lipdR/R/")
files =  setdiff(dir(pattern="*.R"),dir(pattern="*.RData"))

for(f in 1:length(files)){
  source(files[f])
}

#geoChronR
setwd("~/Documents/Science/Research/GeoChronR/GeoChronR_dev/R/")
files =  setdiff(dir(pattern="*.R"),dir(pattern="*.RData",ignore.case = T))
files = setdiff(files,"sourceAll_jeg.R")

for(f in 1:length(files)){
  source(files[f])
}

setwd(cur.dir)