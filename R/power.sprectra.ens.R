#' @export
#' @family spectra
#' @family pca
#' @title Create a synthetic timeseries that emulates the characteristics of a variable
#' @description create synthetic timeseries based on a timeseries. Useful for null hypothesis testing
#' @param time LiPD "variable list" or vector of year/age values
#' @param values LiPD "variable list" or vector of values
#' @param nens Number of ensemble members to simulate
#' @return a vector or matrix of synthetic values
createSyntheticTimeseries = function(time,values,nens=1){
  
  #check to see if time and values are "column lists"
  if(is.list(time)){time=time$values}
  if(is.list(values)){values=values$values}
  
  #make them all matrices
  time = as.matrix(time)
  values = as.matrix(values)
  
  #find the NAs...
  tnai = which(is.na(time))
  vnai = which(is.na(values))
  
  
  
  #measure long term trend
  trend=predict(lm(values~time))
  #remove the trend
  notrend=values-trend
  #get necessary metadata
  m=mean(notrend,na.rm=TRUE)
  s=sd(notrend,na.rm=TRUE)
  
  fit = arima(x = notrend, order = c(1, 0, 0))
  #a=acf(notrend,na.action=na.pass,plot=FALSE)
  #ar=max(0,as.numeric(unlist(a[1])[1]))
  
  synValues = matrix(NA,nrow=nrow(time),ncol=nens)
  #go through ensemble members
  for(jj in 1:nens){
    #generate a random series with ar=ar
    rdata=arima.sim(model=fit$model,n=length(notrend))
    #remove any trend
    rtrend=predict(lm(rdata~time))
    rdata=rdata-rtrend
    #scale the same as data
    rdata=scale(rdata)*s+m
    #add the data trend in
    withTrend=rdata+trend
    withTrend[vnai]=NA
    synValues[,jj]=withTrend
  }
  return(synValues)
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
powerSpectrumEns = function(time,values,max.ens=NA,ofac=1){
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
    syn = createSyntheticTimeseries(t,v)#create synthetic timeseries
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