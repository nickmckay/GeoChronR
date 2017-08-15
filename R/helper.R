#' @export
installLipd = function(){devtools::install_github("nickmckay/lipd-utilities",subdir = "R")}

#' @export

installGeoChronR= function(){devtools::install_github("nickmckay/geochRonR")}


#' @export
#' @title Convert years BP to Calendar year (AD/BC or CE)
#' @description Converts a LiPD variable list, or vector from BP to AD/BC/CE/BCE. Also deals with the lack of a year 0.
#' @param X A LiPD variable list or a vector of years BP 
#' @return X A LiPD variable list or a vector of Calendar years AD 

convertBP2AD = function(X){
if(is.list(X)){
  if(tolower(X$units)=="ad" | tolower(X$units)=="ce"){
    print("already in AD or CE, aborting...")
    return(X)
  }
  X$units = "AD"
  X$values=1950-X$values
  X$values[X$values <= 0]=X$values[X$values <= 0]-1#remember, there's no year 0
}else{
  X=1950-X
  X[X <= 0]=X[X <= 0]-1#remember, there's no year 0
  }
  return(X)
}



#' @title Clear all variables and functions from global environment
#' @desciption Removes all variables and functions from global environment. Use at your own risk.
#' @export
clearAll = function(){
  rm(list = ls(envir = .GlobalEnv),envir = .GlobalEnv)
}

#' @export
#' @title Detach all packages
#' @description Detachs all packages
detachAll = function(){
pkgs = names(sessionInfo()$otherPkgs)
pkgs = paste('package:', pkgs, sep = "") 
lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
}

#' @export
#' @title Setup GeoChronR for first time use
#' @description installs special packages. Should have to be run once.
setupGeoChronR = function(){
  install.packages("ggmap", type = "source")
  source("https://bioconductor.org/biocLite.R")
  biocLite("pcaMethods")
}

