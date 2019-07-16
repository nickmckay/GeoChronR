#' @export
#' @family spectra
#' @family pca
#' @title Create a synthetic timeseries that emulates the characteristics of a variable
#' @description create synthetic timeseries based on a timeseries. Useful for null hypothesis testing
#' @param time LiPD "variable list" or vector of year/age values
#' @param values LiPD "variable list" or vector of values
#' @param nens Number of ensemble members to simulate
#' @return a vector or matrix of synthetic values
createSyntheticTimeseries = function(time,values,nens=1,sameTrend=TRUE,index.to.model = NA){
  
  #check to see if time and values are "column lists"
  if(is.list(time)){time=time$values}
  if(is.list(values)){values=values$values}
  
  if(any(is.na(index.to.model))){
    index.to.model = seq_along(time)
  }
  orig.time = time
  
  #make them all matrices
  time = as.matrix(time[index.to.model])
  values = as.matrix(values[index.to.model])
  
  #find the NAs...
  tnai = which(is.na(time))
  vnai = which(is.na(values))
  
  
  
  #measure long term trend
  trend=predict(lm(values~time))
  trendmodel = lm(values~time)
  longtrend = orig.time*trendmodel$coefficients[2]+trendmodel$coefficients[1]
  #remove the trend
  notrend=values-trend
  #get necessary metadata
  m=mean(notrend,na.rm=TRUE)
  s=sd(notrend,na.rm=TRUE)
  a=acf(notrend,na.action=na.pass,plot=FALSE)
  ar=max(0,as.numeric(unlist(a[1])[1]))
  
  fit = arima(x = notrend, order = c(1, 0, 0)) #AR1 only

  synValues = matrix(NA,nrow=length(orig.time),ncol=nens)
  #go through ensemble members
  for(jj in 1:nens){
    #generate a random series with ar=ar
    rdata = arima.sim(model=list("ar"=ar),n=length(orig.time)) #More conservative
    #rdata=arima.sim(model=fit,n=length(notrend)) AR1 only 
    if(sameTrend){
    #remove any trend
    rtrend=predict(lm(rdata~orig.time))
    rdata=rdata-rtrend
    #scale the same as data
    rdata=scale(rdata)*s+m
    #add the data trend in
   
    
    withTrend=rdata+longtrend
    withTrend[vnai]=NA
    synValues[,jj]=withTrend
    }else{
      synValues[,jj] = rdata
    }
    
  }
  return(synValues)
}

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
#' @description Calculate ensemble power spectra using a variety of spectral methods: Lomb-Scargle, Multi-Taper, and Weighted Wavelet Z-transform
#' @param time LiPD "variable list" or vector of year/age values
#' @param values LiPD "variable list" or vector of values
#' @param max.ens Maximum number of ensemble members to analyze
#' @param ofac oversampling factor for lomb::lsp
#' @param tbw time-bandwidth product for astrochron::mtm___ functions
#' @param padfac padding factor for astrochron::mtm___ functions
#' @param wgtrad radius of the nuspectral::nuwaveletcoeff weight function (non-dimensional units)
#' @param sigma decay parameter of the nuspectral::nuwaveletcoeff wavelets
#' @return a list of ensemble spectra results
#' \itemize{
#' \item freqs: vector of frequencies
#' \item power: vector of spectral powers
#' \item powerSyn: matrix of synthetic spectral power results
#' }
#' @import lomb
#' @import astrochron
#' @suggest nuspectral
computeSpectraEns = function(time,values,max.ens=NA,method='mtm',gauss=FALSE,ofac=1,padfac=2,tbw=3,wgtrad=1,sigma=0.02,mtm_null='power_law'){
  
  #check to see if time and values are "column lists"
  if(is.list(time)){time=time$values}
  if(is.list(values)){values=values$values}
  
  #make them all matrices
  time = as.matrix(time)
  values = as.matrix(values)
  
  # apply mapping to standard Gaussian?
  if(gauss==TRUE){vals = gaussianize(values)}else{vals=values}
  
  if(nrow(time) != nrow(values)){stop("time and values must have the same number of rows (observations)")}
  
  #how many ensemble members?
  nensPoss = ncol(time)*ncol(vals)
  nens=nensPoss
  if(!is.na(max.ens)){
    if(max.ens<nensPoss){
      nens=max.ens
    }
  }
  
  # random sampling of columns in case of very large ensembles
  tind = sample.int(ncol(time),size = min(nens,ncol(time)))
  vind = sample.int(ncol(vals),size = min(nens,ncol(vals)))
  if (ncol(time) < nens){tind = rep_len(tind,nens)}
  if (ncol(vals) < nens){vind = rep_len(vind,nens)} 
  
  # Now apply various spectral methods
  if (method=='lomb-scargle') {
    # find first time axis without duplicates 
    noutrow = length(lsp(vals[,1],times=time[,1],ofac=ofac,plot = F)$power)
  
    #preallocate matrices
    fMat = matrix(NA,ncol=nens,nrow=noutrow)
    pMat = fMat
    pMatSyn = fMat

    pb = txtProgressBar(min=1,max = nens,style = 3)
    for(k in 1:nens){
      t = jitter(time[,tind[k]])   # add jitter
      #tu = unique(t) # avoid duplicates 
      v = vals[,vind[k]] 
      out = lsp(v,times=t,ofac=ofac,plot = F) # Lomb-Scargle Periodogram
      syn = ar1Surrogates(time=t,vals = v) #create matrix of synthetic timeseries  
      #NM: tu & vu doesn't seem to exist, perhaps t & v?
      synOut = lsp(syn,times=t,ofac=ofac,plot = F) # apply to AR(1) surrogate 
      pMatSyn[1:length(synOut$power),k] =  synOut$power # store for later  
      # JEG: the frequency axis will be subtly different for each iteration.  I think it would be better to interpolate to a common frequency axis if we are to compare them all. Or does quantile2d already take care of that?
      fMat[,k]=out$scanned
      pMat[,k]=out$power
      if(k%%round(nens/50)==0){
        setTxtProgressBar(pb,k)
      }
    }
    close(pb)
    
    # allocate output  
    spec.ens = list(freqs = fMat,power = pMat, powerSyn = pMatSyn)
  }
  else if ( method=='mtm') {    # need to make a choice on null : AR(1) or power law?
    if ( mtm_null=='AR(1)') {
      mtm.func = astrochron::mtm
    } else if ( mtm_null=='power_law') {
      mtm.func = astrochron::mtmPL
    } else if ( mtm_null=='ML96') {
      mtm.func = astrochron::mtmML96
    } else {
      stop("Valid choices are 'AR(1)', 'power_law', or 'ML96'")
    }
    
    #  apply workflow to first member
    t = time[,tind[1]]; v = vals[,vind[1]] # define data vectors
    dti =modeSelektor(diff(t))  # identify sensible interpolation interval (mode of distribution)
    dfi = linterp(data.frame(t,v),dt=dti,genplot=F,check=T,verbose=F)  # interpolate at that sampling rate
    ti = dfi$t  # interpolatedtimescale
    mtm.main    <- mtm.func(dfi,padfac=padfac,genplot = F,output=1, verbose = F, tbw = tbw)
    mtm.sigfreq <- mtm.func(dfi,padfac=padfac,genplot = F,output=2, verbose = F, tbw = tbw)
    # define output matrices
    ens.mtm.power   <-  matrix(NA,ncol=nens,nrow=length(mtm.main$Frequency))
    ens.mtm.sigfreq <-  matrix(0,ncol=nens,nrow=length(mtm.main$Frequency))
    # store output for first member
    ens.mtm.power[,1] <- mtm.main$Power
    ens.mtm.sigfreq[match(mtm.sigfreq$Frequency, mtm.main$Frequency, nomatch = 0),1] <- 1
    
    pb = txtProgressBar(min = 2, max = nens, style = 3)
    # rinse, repeat
    for (k in 2:nens){
      t = time[,tind[k]]; v = vals[,vind[k]] 
      dfl = data.frame(approx(t,v,ti,rule = 2)) 
      #dfe = linterp(data.frame(t,v),dt=dti,genplot=F,check=T,verbose=F)  # define local dataframe
      #dti =modeSelektor(diff(t))  # identify sensible interpolation interval (mode of distribution)
      #dfl = linterp(data.frame(t,v),dt=dti,genplot=F,check=T,verbose=F)  # interpolate at that sampling rate
      mtm.main    <- mtm.func(dfl,padfac=padfac,genplot = F,output=1, verbose = F, tbw = tbw)
      mtm.sigfreq <- mtm.func(dfl,padfac=padfac,genplot = F,output=2, verbose = F, tbw = tbw)
      ens.mtm.power[,k] <- mtm.main$Power
      ens.mtm.sigfreq[match(mtm.sigfreq$Frequency, mtm.main$Frequency, nomatch = 0),k] <- 1
      if(k%%round(nens/50)==0){
        setTxtProgressBar(pb,k)
      }
    }
    close(pb)
    freqs.prob = rowMeans(ens.mtm.sigfreq)
    f = mtm.main$Frequency
    # allocate output
    spec.ens = list(freqs = matrix(f,nrow=length(f),ncol=nens,byrow=F), power = ens.mtm.power, powerSyn = NA, prob = freqs.prob)
    }
  else if ( method=='nuspectral') {
    nt = length(time)
    tau = seq(min(time),max(time),length = max(nt %/% 5,10))
    
    # first iteration to define matrix sizes
    t = time[,tind[1]]; v = vals[,vind[1]] 
    nuspec <-  nuspectral:::nuwavelet_psd(t,v,sigma=sigma,taus = tau)
    noutrow <- length(nuspec$Frequency)
    
    #preallocate matrices
    fMat = matrix(NA,ncol=nens,nrow=noutrow)
    pMat = fMat
    pMatSyn = fMat
    
    # 1st, heard-earned ensemble member
    pMat[,1] <- nuspec$Power
    fMat[,1] <- nuspec$Frequency
    
    pb = txtProgressBar(min=1,max = nens,style = 3)  # run the loop with progress bar
    for (k in 2:nens){
      t = time[,tind[k]]; v = vals[,vind[k]] 
      nuspec   <-  nuspectral:::nuwavelet_psd(t,v,sigma=sigma,taus = tau)
      pMat[,k] <- nuspec$Power
      fMat[,k] <- nuspec$Frequency
  
      if(k%%round(nens/10)==0){
        setTxtProgressBar(pb,k)
      }
    }
    close(pb)
    
    # allocate output
    spec.ens = list(freqs = fMat, power = pMat, powerSyn = NA, prob = NA)
    
  } else {stop("Unknown method: Valid choices are: mtm, nuspectral, or lomb-scargle")}
  
  return(spec.ens)
}