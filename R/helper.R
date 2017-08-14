#' @export
installLipd = function(){devtools::install_github("nickmckay/lipd-utilities",subdir = "R")}

#' @export

installGeoChronR= function(){devtools::install_github("nickmckay/geochRonR")}


#' @export
convertBP2AD = function(X){

if(is.list(X)){
  if(tolower(X$units)=="ad" | tolower(X$units)=="ce"){
    print("already in AD or CE, aborting...")
    return(X)
  }
  
  X$units = "AD"
  X$values=1950-X$values
}else{
  X=1950-X
  }
  
  return(X)
 
}

#' @export
clearAll = function(){
  rm(list = ls(envir = .GlobalEnv),envir = .GlobalEnv)
}

#' @export
detachAll = function(){
pkgs = names(sessionInfo()$otherPkgs)
pkgs = paste('package:', pkgs, sep = "") 
lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
}

#' @export
#' @title Setup GeoChronR
#' 
#'

setupGeoChronR = function(){
  install.packages("ggmap", type = "source")
  source("https://bioconductor.org/biocLite.R")
  biocLite("pcaMethods")
}

