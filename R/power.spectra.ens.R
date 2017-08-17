#' @export
#' @family spectra
#' @family pca
#' @family correlations
#' @title Create a synthetic timeseries that emulates the characteristics of a variable
#' @description create synthetic timeseries based on a timeseries. Useful for null hypothesis testing
#' @param time LiPD "variable list" or vector of year/age values
#' @param vals LiPD "variable list" or vector of values
#' @param nens Number of ensemble members to simulate
#' @return a vector or matrix of AR(1) surrogates for series X
ar1Surrogates = function(time,vals,detrend_bool=FALSE,nens=1){
  
  #check to see if time and values are "column lists"
  if(is.list(time)){time=time$values}
  if(is.list(vals)){vals=vals$values}
  
  #make them all matrices
  time = as.matrix(time)
  vals = as.matrix(vals)
  
  #find the NAs...
  tnai = which(is.na(time))
  vnai = which(is.na(vals))
  
  # if detrend option is passed, use detrended vvalues; otherwise, unchanged.
  if(detrend_bool){
    #fit a linear trend
    trend=predict(lm(vals~time))
    #remove the linear trend
    vals_used=vals-trend
  } else {vals_used=vals}
    
  #get necessary metadata
  m=mean(vals_used,na.rm=TRUE)
  s=sd(vals_used,na.rm=TRUE)
  
  fit = arima(x = vals_used, order = c(1, 0, 0))
  #a=acf(vals_used,na.action=na.pass,plot=FALSE)
  #ar=max(0,as.numeric(unlist(a[1])[1]))
  
  vals_syn = matrix(NA,nrow=nrow(time),ncol=nens)
  #go through ensemble members
  for(jj in 1:nens){
    #generate a random series with ar=ar
    ar1=arima.sim(model=fit$model,n=length(vals_used))
    #remove any trend
    #rtrend=predict(lm(ar1~time))
    #ar1=ar1-rtrend
    
    #scale the same as data
    ar1=scale(ar1)*s+m
    ar1[vnai]=NA # put NA values in same place as original
    vals_syn[,jj]=ar1
  }
  # if trend was taken out, add it back in to each ensemble member
  if(detrend_bool){
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
powerSpectrumEns = function(time,values,spc_mthd = 'Lomb-Scargle',max.ens=NA,ofac=1){
  #check to see if time and values are "column lists"
  if(is.list(time)){time=time$values}
  if(is.list(values)){values=values$values}
  
  #make them all matrices
  time = as.matrix(time)
  values = as.matrix(values)
  
  
  if(nrow(time) != nrow(values)){stop("time and values must have the same number of rows (observations)")}
  
  
  #how many ensemble members?
  nensPoss = ncol(time)*ncol(values)
  nens=nensPoss
  if(!is.na(max.ens)){
    if(max.ens<nensPoss){
      nens=max.ens
    }
  }
  
  #how big?
  noutrow = length(lsp(values[,1],times=time[,1],ofac=ofac,plot = F)$power)
  
  #preallocate
  fMat = matrix(NA,ncol=nens,nrow=noutrow)
  pMat = fMat
  pMatSyn = fMat
  
  pb = txtProgressBar(min=1,max = nens,style = 3)
  for(i in 1:nens){
    t = time[,sample.int(ncol(time),size = 1)]
    v = values[,sample.int(ncol(values),size = 1)]
    out = lsp(v,times=t,ofac=ofac,plot = F)
    syn = ar1Surrogates(t,v)#create synthetic timeseries  #JEG: this should be done outside the loop
    synOut = lsp(syn,times=t,ofac=ofac,plot = F) 
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