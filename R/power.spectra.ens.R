
#' @export
#' @import dplR
#' @family spectra
#' @family pca
#' @family correlations
#' @title Create a synthetic timeseries that emulates the characteristics of a variable
#' @description create synthetic timeseries based on a timeseries. Useful for null hypothesis testing
#' @param time LiPD "variable list" or vector of year/age values
#' @param vals LiPD "variable list" or vector of values
#' @param detrend_bool boolean variable, indicating whether one should remove a linear trend (default) or not. Note: does not affect the AR(1) fit, but does affect the generated timeseries
#' @param method Method used for estimation. Possible values are "even" or "redfit"
#' @param nens Number of ensemble members to simulate
#' @return a vector or matrix of AR(1) surrogates for series X
ar1Surrogates = function(time,vals,detrend_bool=TRUE,method='redfit',nens=1){
  
  #check to see if time and values are "column lists"
  if(is.list(time)){time=time$values}
  if(is.list(vals)){vals=vals$values}
  
  #make them all matrices
  time = as.matrix(time)
  vals = as.matrix(vals)
  
  #find the NAs...
  tnai = which(is.na(time))
  vnai = which(is.na(vals))
  
  trend=predict(lm(vals~time))     #fit a linear trend
  # if detrend option is passed, use detrended values; otherwise, unchanged.
  if(detrend_bool){
    #remove the linear trend
    vals_used=vals-trend
  } else {vals_used=vals}
    
  #extract low-order moments
  m=mean(vals_used,na.rm=TRUE)
  s=sd(vals_used,na.rm=TRUE)
  if (method=='redfit') {
    redf.dat <- redfit(vals_used, time, nsim = 21) # 21 is minimum number you can get away with
    g = redf.dat$rho
  } else {
    fit = arima(x = vals_used, order = c(1, 0, 0)) # assumes evenly-spaced data
    g = as.numeric(fit$coef[1])
  }
  
  vals_syn = matrix(NA,nrow=nrow(time),ncol=nens)
 
  for(jj in 1:nens){  #go through ensemble members
    #generate a random series with fitted model parameters
    ar1=arima.sim(model=list(g),n=length(vals_used))
    
    ar1=scale(ar1)*s+m   #scale the same as original values
    ar1[vnai]=NA # put NA values in same place as original
    vals_syn[,jj]=ar1 
  }
  # if trend needs to stay in, add it back in
  if(!detrend_bool){
    vals_syn = vals_syn + replicate(nens,trend)
  }
  return(vals_syn)
}


#' @export
#' @family spectra
#' @title Calculate ensemble power spectra
#' @description Calculate ensemble power spectra using lomb-scargle
#' @param time LiPD "variable list" or vector of year/age values
#' @param values LiPD "variable list" or vector of values
#' @param max.ens Maximum number of ensemble members to analyze
#' @param ofac oversampling factor for lomb::lsp
#' @return a list of ensemble spectra results
#' \itemize{
#' \item freqs: vector of frequencies
#' \item power: vector of spectral powers
#' \item powerSyn: matrix of synthetic spectral power results
#' }
#' @import lomb
computeSpectraEns = function(time,values,max.ens=NA,method='lomb-scargle',gaussianize=FALSE){
  
  # process options
  
  if (method=='lomb-scargle'){ofac <- 1} 
  else if (method=='mtm'){padfac <- 2}   
  else if (method=='nuspectral'){sigma <- 0.02, wgtrad=0.2}    
  else  #unknown method error  
  
  #check to see if time and values are "column lists"
  if(is.list(time)){time=time$values}
  if(is.list(values)){values=values$values}
  
  #make them all matrices
  time = as.matrix(time)
  values = as.matrix(values)
  
  if(gaussianize==TRUE){vals_used = gaussianize(values)}else{vals_used=values}
  
  if(nrow(time) != nrow(values)){stop("time and values must have the same number of rows (observations)")}
  
  #how many ensemble members?
  nensPoss = ncol(time)*ncol(values)
  nens=nensPoss
  if(!is.na(max.ens)){
    if(max.ens<nensPoss){
      nens=max.ens
    }
  }
  
  
  #how big?  # method dependent
  noutrow = length(lsp(values[,1],times=time[,1],ofac=ofac,plot = F)$power)
  
  #preallocate
  fMat = matrix(NA,ncol=nens,nrow=noutrow)
  pMat = fMat
  pMatSyn = fMat
  
  pb = txtProgressBar(min=1,max = nens,style = 3)
  for(i in 1:nens){
    t = time[,sample.int(ncol(time),size = 1)]  # random sampling 
    v = values[,sample.int(ncol(values),size = 1)]  # should this use same indices as above?
    out = lsp(v,times=t,ofac=ofac,plot = F)
    syn = ar1Surrogates(t,v)#create synthetic timeseries  
    synOut = lsp(syn,times=t,ofac=ofac,plot = F) # Lomb-Scargle Periodogram
    pMatSyn[1:length(synOut$power),i] =  synOut$power
    fMat[,i]=out$scanned
    pMat[,i]=out$power
    if(i%%round(nens/50)==0){
      setTxtProgressBar(pb,i)
    }
  }
  close(pb)
  
  spec.ens = list(freqs = fMat,power = pMat, powerSyn = pMatSyn)
  return(spec.ens)
  
}