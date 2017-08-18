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
#' @description Removes all variables and functions from global environment. Use at your own risk.
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

#' @export
#' @title Gaussianize
#' @author Julien Emile-Geay 
#' @author Nick McKay
gaussianize <- function (X,jitter=FALSE){ 
  #   Transform each column of data matrix X to normality using the inverse
  #   Rosenblatt transform.
  #
  # inspired by split.m in normal.m by Van Albada, S.J., Robinson P.A. (2006)
  # Transformation of arbitrary distributions to the normal distribution with application to EEG
  # test-retest reliability. J Neurosci Meth, doi:10.1016/j.jneumeth.2006.11.004
  #
  #  Written 26/06/2015 by Julien Emile-Geay (USC)
  #translated to R and added jitter option by 29/06/2015 by Nick McKay (NAU) 
  
  if(!is.matrix(X)){
    X=as.matrix(X)
  }
  
  p=NCOL(X)
  n=NROW(X) 
  
  if(jitter){
    #add tiny random numbers to avoid ties
    X=array(rnorm(p*n,mean=0,sd=sd(as.vector(X))/1e6),c(n,p))+X
  }
  
  Xn    = matrix(0,n,p)
  for (j in 1:p){
    Z = X[,j]; nz = !is.na(Z); N = sum(nz,na.rm=TRUE); # guard against NaNs
    # Sort the data in ascending order and retain permutation indices
    R = rank(Z)
    # The cumulative distribution function
    CDF = R/N - 1/(2*N)
    # Apply the inverse Rosenblatt transformation
    Xn[nz,j] = qnorm(CDF)  # Xn is now normally distributed
  }
  
  return(Xn)
}






