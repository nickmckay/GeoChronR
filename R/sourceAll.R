#temporary script that loads in all functions in LiPDR and GeoChronR
#will be replaced by package loader soon

#load LiPD Package functions

cur.dir=getwd()
setwd("~/GitHub/lipdR/R/")
#setwd("~/GitHub/geoChronR/lipd_R/R/")
files =  setdiff(dir(pattern="*.R"),dir(pattern="*.RData"))

for(f in 1:length(files)){
  source(files[f])
}

#geoChronR
setwd("~/GitHub/GeoChronR-package/R/")
files =  setdiff(dir(pattern="*.R"),dir(pattern="*.RData"))
files = setdiff(files,"sourceAll.R")

for(f in 1:length(files)){
  source(files[f])
}





setwd(cur.dir)

