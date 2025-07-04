#' Load a remote RData file
#'
#' @param useSavedNames use the names saved in the file?
#' @param url the url of a remote data file 
#'
#' @family utility
#' @return the loadeds object
#' @export
loadRemote <- function(url,useSavedNames = FALSE){
  download.file(url,destfile = file.path(tempdir(),"downloadData.RData"),method = "libcurl")
  varName <- load(file.path(tempdir(),"downloadData.RData"))
  if(useSavedNames){
    for(i in 1:length(varName)){
      assign(varName[i],get(varName[i]),envir = .GlobalEnv)
    }
  }else{
    return(get(varName))
  }
}

#' @export
#' @family utility
#' @title Makes a guess about the units of a time vector based on the values
#' @description Guesses year units based on some simple heuristics
#'
#' @param range.min What's the smallest range that you'd believe is yr not ka? (default = 25)
#' @param X A LiPD variable list or a vector of years BP 
#'
#' @return A *guessed* string unit label
heuristicUnits = function(X,range.min = 25){
  if(is.list(X)){
    if(all(is.na(X$values))){
      return(NA)
    }
    miv <- min(X$values,na.rm = TRUE)
    mav <- max(X$values,na.rm = TRUE)
    rv <- diff(range(X$values, na.rm = TRUE))  
  }else{
    if(all(is.na(X))){
      return(X)
    }
    miv <- min(X,na.rm = TRUE)
    mav <- max(X,na.rm = TRUE)
    rv <- diff(range(X, na.rm = TRUE))  
  }
  
  #See if the highest value would be in the future for AD
    todayCheckAD <-  mav > as.numeric(substring(date(),21))
  
  #See if the lowest values would be in the future for BP
    todayCheckBP <- miv < convertAD2BP(as.numeric(substring(date(),21)))
    
    #see if range of values implies ka not BP
    rangeCheck <- rv < range.min
    
    #now work out some scenarios
    if(todayCheckBP & !todayCheckAD){
        unitGuess <- "AD"
    }else if(!todayCheckBP & todayCheckAD){
      unitGuess <- "BP"
    }else if(rangeCheck){
      unitGuess <- "ka"
    }else if(todayCheckBP & todayCheckAD){
      unitGuess <- "somethings wrong here, doesn't seem to be AD, BP, or ka"
    }else{
      if(miv > 0 & mav > 1900){
        unitGuess <- "AD"
      }else if(miv >-50  & mav < 100){
          unitGuess <- "BP"
      }else{
        unitGuess <- "cant make a reasonable guess"
      }
    }
    
    return(unitGuess)
}


#' @export
#' @family utility
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

    if(!is.numeric(X$values)){
      print("non-numeric dates, aborting...")
      return(X)
    }
    if(all(is.na(X$values))){
      return(X)
    }
    if(grepl(pattern = "k",x = X$units,ignore.case = TRUE)){#in ka or kyr!
      #convert to BP first
      X$values <- X$values*1000
    }
    
    X$units = "AD"
    X$values=1950-X$values
    X$values[which(X$values <= 0)]=X$values[which(X$values <= 0)]-1#remember, there's no year 0
    
  }else{
    if(all(is.na(X)) | !is.numeric(X)){
      return(X)
    }
    X=1950-X
    X[which(X <= 0)]=X[which(X <= 0)]-1#remember, there's no year 0
    
  }
  return(X)
}


#' @export
#' @family utility
#' @title Convert years Calendar year (AD/BC or CE) to BP (1950)
#' @description Converts a LiPD variable list, or vector from AD/BC/CE/BCE to BP (1950). Also deals with the lack of a year 0.
#' @param X A LiPD variable list or a vector of years AD/BC/CE/BCE
#' @return X A LiPD variable list or a vector of BP
convertAD2BP = function(X){
  if(is.list(X)){
    if(tolower(X$units)=="bp"){
      print("already in BP, aborting...")
      return(X)
    }
    if(all(is.na(X$values))){
      return(X)
    }
    X$units = "BP"
    if(!is.numeric(X$values)){
      print("non-numeric dates, aborting...")
      return(X)
    }
    X$values=1950-X$values
    X$values[which(X$values <= 0)]=X$values[which(X$values <= 0)]+1#remember, there's no year 0
    
  }else{
    if(all(is.na(X))| !is.numeric(X)){
      return(X)
    }
      X[which(X <= 0)]=X[which(X <= 0)]+1#remember, there's no year 0
      X=1950-X
  }
  return(X)
}

# 'gaussianize' is inspired by split.m in normal.m by Van Albada & Robinson (2006)
# 
#  History: - Written 26/06/2015 by Julien Emile-Geay (USC)
#           - translated to R and added jitter option on 29/06/2015 by Nick McKay (NAU) 
#           - added capability to handle missing values on 27/06/2019 by Julien Emile-Geay 
#' @title Gaussianize
#' @name gaussianize
#' @author Julien Emile-Geay 
#' @author Nick McKay
#' @family utility
#' @param X data matrix (2D array)
#' @param jitter boolean variable ; if TRUE, add jitter to data to prevent ties
#' @return gaussianized data matrix, Xn
#' @description Transforms each column of data matrix X to normality using the inverse Rosenblatt transform. Tolerant to missing values (NA entries).
#' @references Emile-Geay, J., and M. Tingley (2016), Inferring climate variability from nonlinear proxies: application to palaeo-enso studies, Climate of the Past, 12 (1), 31–50, doi:10.5194/cp-12-31-2016.
#' @references Van Albada, S.J., Robinson P.A. (2006), Transformation of arbitrary distributions to the normal distribution with application to EEG test-retest reliability. J Neurosci Meth, doi:10.1016/j.jneumeth.2006.11.004 
#' @export
gaussianize <- function(X,jitter=FALSE){ 
  if(!is.matrix(X)){
    X=as.matrix(X)
  }
 
  p=NCOL(X); n=NROW(X) # dimensions
  
  if(jitter){
    X=array(rnorm(p*n,mean=0,sd=sd(as.vector(X))/1e6),c(n,p))+X #add tiny random numbers to avoid ties
  }
  
  Xn    = matrix(data = NA,nrow = n,ncol = p)
  for (j in 1:p){
    nz  <- which(!is.na(X[,j]))
    N   <- length(nz)
    R   <- rank(X[nz,j]) # Sort the data in ascending order and retain permutation indices
    CDF <-R/N - 1/(2*N) # Obtain cumulative distribution function
    Xn[nz,j] <- qnorm(CDF)  # Apply the inverse Rosenblatt transformation
  }
  
  return(Xn)
}

#' @title ModeSelektor
#' @name modeSelektor
#' @family plot help
#' @author Julien Emile-Geay 
#' @param x data vector
#' @return mode value and index. 
#' @description Finds the mode of a vector X. Astonishingly, this is not a standard R function.
#' @references https://www.modeselektor.com
#' @references https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
#' @export
modeSelektor <- function(x){ 
  ux <- unique(x)
  umode = ux[which.max(tabulate(match(x, ux)))]
  return(umode)
}


#' Simulate uncertainty in a dataset
#' @description Simulate uncertainty in a dataset given an estimate of standard deviation and an ARIMA model
#' @family utility
#' @param n length of output vector
#' @param sd standard deviation of the output vector
#' @param mean mean of the output vector
#' @param ar Autocorrelation coefficient to use for modelling uncertainty, what fraction of the uncertainties are autocorrelated? (default = sqrt(0.5); or 50 percent autocorrelated uncertainty)
#' @param arima.order Order to use for ARIMA model used in modelling uncertainty (default = c(1,0,0))
#' @param seed Optionally enter an integer to set a seed for the random number generation
#'
#' @return ts simulated from a from an ARIMA model with a defined mean and variance
#' @export
simulateAutoCorrelatedUncertainty <- function(n, sd, mean = 0, ar = sqrt(.5),arima.order = c(1,0,0),seed = NA){
  if(any(!is.na(seed))){
    set.seed(seed)
  }
  unc <- stats::arima.sim(list(order = arima.order, ar = ar), n = n)
  unc <- (scale(unc,center = TRUE, scale = TRUE) * sd) + mean
  unc <- as.matrix(unc)
  return(unc)
}

#' Create an ensemble from an uncertainty and AR1 estimate
#'
#' @param variable LiPD variable object or vector of data
#' @param n.ens Number of ensemble members to create (default = 1000)
#' @param sd Standard deviation of the uncertainty to simulate
#' @param ar Autocorrelation coefficient to use for modelling uncertainty, what fraction of the uncertainties are autocorrelated? (default = sqrt(0.5); or 50 percent autocorrelated uncertainty)
#' @param arima.order Order to use for ARIMA model used in modelling uncertainty (default = c(1,0,0))
#' @family utility
#' @return a matrix of ensemble values
#' @export
generateEnsembleFromUncertainty <- function(variable,n.ens,sd,ar = sqrt(.5),arima.order = c(1,0,0)){
  if(is.list(variable)){
    dat <- as.matrix(variable$values)
  }else{
    dat <- as.matrix(values)
  }
  
  if(nrow(dat) < ncol(dat)){
    dat <- t(dat)
  }
  
  if(ncol(dat) != 1){
    stop("variable should be a vector or 1 column matrix")
  }
  
  
  repMat <- matrix(rep(dat,times = n.ens),nrow = length(dat),ncol = n.ens,byrow = FALSE)
  nr <- nrow(repMat)

  unc <- purrr::map(seq_len(n.ens),\(x) simulateAutoCorrelatedUncertainty(seed = x,n = nr,sd = sd, ar = ar, arima.order = arima.order)) %>% 
    unlist()  %>%  
    matrix(nrow = nr,ncol = n.ens,byrow = FALSE)
  
  ens.out <- repMat + unc
  
  return(ens.out)
}





