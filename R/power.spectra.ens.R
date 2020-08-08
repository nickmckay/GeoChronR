#' @export
#' @name createSyntheticTimeseries
#' @family spectra
#' @family pca
#' @title Create a synthetic timeseries that emulates the characteristics of a timeseries
#' @description create synthetic timeseries based on a timeseries. Useful for null hypothesis testing
#' @param time LiPD "variable list" or vector of year/age values
#' @param values LiPD "variable list" or vector of values
#' @param n.ens Number of ensemble members to simulate
#' @return a vector or matrix of synthetic values
createSyntheticTimeseries <- function(time,values,n.ens=1,sameTrend=TRUE,index.to.model = NA){
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
  
  synValues = matrix(NA,nrow=length(orig.time),ncol=n.ens)
  #go through ensemble members
  for(jj in 1:n.ens){
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
#' @param detrend boolean variable, indicating whether one should remove a linear trend (default) or not. Note: does not affect the AR(1) fit, but does affect the generated timeseries
#' @param method Method used for estimation. Possible values are "even" or "redfit"
#' @param n.ens Number of ensemble members to simulate
#' @return a vector or matrix of AR(1) surrogates for series X
ar1Surrogates = function(time,vals,detrend=TRUE,method='redfit',n.ens=1){
  
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
  if(detrend){
    #remove the linear trend
    vals_used=vals-trend
  } else {vals_used=vals}
  
  #extract low-order moments
  m=mean(vals_used,na.rm=TRUE)
  s=sd(vals_used,na.rm=TRUE)
  if (method=='redfit') {
    redf.dat <- redfit(vals_used, time, nsim = 50) # 21 is minimum number you can get away with
    g = redf.dat$rho
  } else {
    fit = arima(x = vals_used, order = c(1, 0, 0)) # assumes evenly-spaced data
    g = as.numeric(fit$coef[1])
  }
  
  vals_syn = matrix(NA,nrow=nrow(time),ncol=n.ens)
  
  for(jj in 1:n.ens){  #go through ensemble members
    #generate a random series with fitted model.parameters
    ar1=arima.sim(model=list(g),n=length(vals_used))
    
    ar1=scale(ar1)*s+m   #scale the same as original values
    ar1[vnai]=NA # put NA values in same place as original
    vals_syn[,jj]=ar1 
  }
  # if trend needs to stay in, add it back in
  if(!detrend){
    vals_syn = vals_syn + replicate(n.ens,trend)
  }
  return(vals_syn)
}


#' Calculate ensemble power spectra
#' @title Calculate ensemble power spectra
#' @name computeSpectraEns
#' @description Calculate ensemble power spectra using 4 spectral methods: 
#' \enumerate{
#' \item Lomb-Scargle periodogram
#' \item REDFIT (Mudelsee et al, 2009) 
#' \item Multitaper Method (Thomson et al. 1982)
#' \item nuspectral (Mathias et al 2004)  (no confidence estimation)
#' }
#' For REDFIT and MTM, only the 95% confidence limit is computed. The estimation thereof is as described in Mudelsee et al 2009 for a single ensemble member.
#' For multiple ensemble members, the median of the ensemble of 95% CLs is taken.  
#' For Lomb-Scargle, the parameter `probs` determines the quantiles of the surrogate spectrum distribution extracted as confidence limits.
#' @param time LiPD "variable list" or vector of year/age values
#' @param values LiPD "variable list" or vector of values
#' @param max.ens Maximum number of ensemble members to analyze
#' @param ofac oversampling factor for lomb::lsp and dplR::redfit
#' @param tbw time-bandwidth product for astrochron::mtm___ functions
#' @param padfac padding factor for astrochron::mtm___ functions
#' @param wgtrad radius of the nuspectral::nuwaveletcoeff weight function (non-dimensional units)
#' @param sigma decay parameter of the nuspectral::nuwaveletcoeff wavelets
#' @param probs vector of probabilities for the siginificance levels. To avoid such computations, pass `probs = NA`.
#' @param gauss Boolean flag indicating whether the values should be mapped to a standard Gaussian prior to analysis.
#' @param mtm_mull null hypothesis for the detectiion of significant peaks in `astrochron::mtm()`. Possible choices: AR(1), power_law, or ML96.
#' @return a list of ensemble spectra results
#' \itemize{
#' \item freqs: vector of frequencies
#' \item power: vector of spectral powers
#' \item power.CL: matrix of confidence limits for spectral power
#' }
#' @import dplR
#' @importFrom astrochron mtm mtmPL mtmML96
#' @importFrom matrixStats rowMedians
#' @importFrom lomb lsp
#' @references Mudelsee, M., D. Scholz, R. Röthlisberger, D. Fleitmann, A. Mangini, and E. W. Wolff (2009), Climate spectrum estimation in the presence of timescale errors, Nonlinear Processes in Geophysics, 16(1), 43–56, doi:10.5194/npg-16-43-2009.
#' @references Mathias, A., F. Grond, R. Guardans, D. Seese, M. Canela, and H. Diebner (2004), Algorithms for spectral analysis of irregularly sampled time series, Journal of Statistical Software, Articles, 11(2), 1–27, doi:10.18637/jss.v011.i02.
#' @references Thomson, D. J. (1982), Spectrum estimation and harmonic analysis, Proc. IEEE, 70(9), 1055–1096.
#' @export
#' @family spectra
computeSpectraEns = function(time,values,max.ens=NA,method='mtm',probs=0.95,gaussianize=TRUE,ofac=4,padfac=2,tbw=3,wgtrad=1,sigma=0.02,mtm_null='power_law'){
  
  #check to see if time and values are "column lists"
  if(is.list(time)){time=time$values}
  if(is.list(values)){values=values$values}
  
  #make them all matrices
  time = as.matrix(time)
  values = as.matrix(values)
  
  # apply mapping to standard Gaussian?
  if(gaussianize){vals = gaussianize(values)}else{vals=values}
  
  if(nrow(time) != nrow(values)){stop("time and values must have the same number of rows (observations)")}
  
  #how many ensemble members?
  n.ensPoss = ncol(time)*ncol(vals)
  n.ens=n.ensPoss
  if(!is.na(max.ens)){
    if(max.ens<n.ensPoss){
      n.ens=max.ens
      nsim = max.ens
    }
  } else {nsim = 200}
  
  # random sampling of columns in case of very large ensembles
  tind = sample.int(ncol(time),size = min(n.ens,ncol(time)))
  vind = sample.int(ncol(vals),size = min(n.ens,ncol(vals)))
  if (ncol(time) < n.ens){tind = rep_len(tind,n.ens)}
  if (ncol(vals) < n.ens){vind = rep_len(vind,n.ens)} 
  
  # Now apply various spectral methods
  if (method=='lomb-scargle') {
    t = time[, 1]
    v = vals[, 1]
    lomb.out = lomb::lsp(v,
                   times = t,
                   ofac = ofac,
                   plot = F)
    noutrow = length(lomb.out$power)
    
    if (n.ens == 1) {
      fMat = lomb.out$scanned
      pMat = lomb.out$power
      #  Simulate AR(1) spectra: this part to be replaced by uAR1 as soon as it's available
      syn = ar1Surrogates(time = t,
                          vals = v,
                          n.ens = nsim) #create matrix of synthetic timeseries
      pMatSyn =  matrix(NA, ncol = nsim, nrow = length(pMat))
      #pb = txtProgressBar(min = 1, max = nsim, style = 3)
      for (k in 1:nsim) {
        # compute spectra
        pMatSyn[1:length(pMat), k] =  lsp(syn[, k],
                                          times = t,
                                          ofac = ofac,
                                          plot = F)$power
        #if (k %% round(nsim / 10) == 0) {setTxtProgressBar(pb, k)}
      }
      #close(pb)
    } else if (n.ens > 1){
      #preallocate matrices
      fMat = matrix(NA,ncol=n.ens,nrow=length(lomb.out$power))
      pMat = fMat
      pMatSyn = fMat
      
      #declare progress bar
      pb = txtProgressBar(min=1,max = n.ens,style = 3)
      
      for(k in 1:n.ens){  # loop over ensemble members
        t = jitter(time[,tind[k]])   # add jitter
        v = vals[,vind[k]] 
        out = lsp(v,times=t,ofac=ofac,plot = F) # Lomb-Scargle Periodogram
        if(!is.na(probs)) {
          syn = ar1Surrogates(time = t, vals = v) #create matrix of synthetic timeseries
          synOut = lsp(syn,
                       times = t,
                       ofac = ofac,
                       plot = F) # apply to AR(1) surrogate [NoTE: need to do more than 1...]
          pMatSyn[1:length(synOut$power), k] =  synOut$power # store for later
        }
        # JEG: the frequency axis will be subtly different for each iteration.  I think it would be better to interpolate to a common frequency axis if we are to compare them all. Or does quantile2d already take care of that?
        fMat[,k]=out$scanned
        pMat[,k]=out$power
        if(k%%round(n.ens/50)==0){
          setTxtProgressBar(pb,k)
        }
      }
      close(pb)
    } 
    
    # establish confidence limits
    pCL = matrix(NA, ncol = length(probs),nrow = noutrow)
    
    if(!is.na(probs)) {
      for(i in 1:noutrow){
        pCL[i,] = quantile(pMatSyn[i,],probs = probs,na.rm  = T)
      } 
      # allocate output  
      spec.ens = list(freqs = fMat,power = pMat, power.CL = pCL)
    } else {spec.ens = list(freqs = fMat,power = pMat, power.CL = NA)}
  }
  
  else if (method=='redfit') {
    # make sure the time axis is increasing
    # dt = diff(time[, 1])
    # if (median(dt) < 0) {
    #   time <- rev(time, 1)
    # }
    
    t = time[, 1]
    v = vals[, 1]
    
    mcflag = !is.na(probs) # only activate Monte Carlo test if probs is not NA
    
    # blank run on the first ensemble member to obtain matrix dimensions
    redfit.out <- redfit(v,
                         t,
                         tType = "time",
                         mctest = mcflag,
                         ofac = ofac)
    noutrow = length(redfit.out$freq)
    # frequency axis (in tests, ends up being very close between ensemble members, 1e-4 to 1e-6)
    freq <-  redfit.out$freq
    
    if (n.ens == 1) {
      pMat <-  redfit.out$gxx
      if(mcflag){pCL <- redfit.out$ci95}
      else {pCL = NA}
    }
    else if (n.ens > 1) {
      #preallocate matrices
      pMat = matrix(NA, ncol = n.ens, nrow = noutrow)
      pMatSyn = pMat
      
      pb = txtProgressBar(min = 1, max = n.ens, style = 3)
      for (k in 1:n.ens) {
        t <- jitter(time[, tind[k]])   # add jitter
        v <- vals[, vind[k]]
        redfit.out <- redfit(v,t,tType = "time", mctest = mcflag, ofac = ofac, nsim=200)
        pMatSyn[, k] <- redfit.out$ci95 # 95% limit
        pMat[, k] <- redfit.out$gxx
        if (k %% round(n.ens / 50) == 0) {
          setTxtProgressBar(pb, k)
        }
      }
      close(pb)
      # establish confidence limits
      if(mcflag) {
        pCL <- rowMedians(pMatSyn)
      } else {pCL = NA}
    } 
    
    # allocate output  
    spec.ens = list(freqs = freq, power = pMat, power.CL = pCL)
    
  } else if ( method=='mtm') {    # need to make a choice on null : AR(1) or power law?
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
    t  = time[,tind[1]]; v = vals[,vind[1]] # define data vectors
    dti = modeSelektor(diff(t))  # identify sensible interpolation interval (mode of distribution)
    dfi = linterp(data.frame(t,v),dt=dti,genplot=F,check=T,verbose=F)  # interpolate at that sampling rate
    ti = dfi$t  # interpolatedtimescale
    mtm.main    <- mtm.func(dfi,padfac=padfac,genplot = F,output=1, verbose = F, tbw = tbw)
    mtm.sigfreq <- mtm.func(dfi,padfac=padfac,genplot = F,output=2, verbose = F, tbw = tbw)
    # define output matrices
    f <- mtm.main$Frequency
    nout = length(f)
    ens.mtm.power  <-  matrix(NA,ncol=n.ens,nrow=nout)
    ens.mtm.cl     <-  matrix(NA,ncol=n.ens,nrow=nout)
    ens.mtm.sigfreq <-  matrix(0,ncol=n.ens,nrow=nout)
    
    pb = txtProgressBar(min = 0, max = n.ens, style = 3)
    # rinse, repeat
    for (k in 1:n.ens){
      t = time[,tind[k]]; v = vals[,vind[k]] 
      dfl = data.frame(approx(t,v,ti,rule = 2)) # interpolate onto new timescale
      #dfe = linterp(data.frame(t,v),dt=dti,genplot=F,check=T,verbose=F)  # define local dataframe
      #dti =modeSelektor(diff(t))  # identify sensible interpolation interval (mode of distribution)
      #dfl = linterp(data.frame(t,v),dt=dti,genplot=F,check=T,verbose=F)  # interpolate at that sampling rate
      mtm.main    <- mtm.func(dfl,padfac=padfac,genplot = F,output=1, verbose = F, tbw = tbw)
      mtm.sigfreq <- mtm.func(dfl,padfac=padfac,genplot = F,output=2, verbose = F, tbw = tbw)
      ens.mtm.power[,k] <- mtm.main$Power
      if (mtm_null=='AR(1)') {
        ens.mtm.cl[,k] <- mtm.main$AR1_95_power
      } else if ( mtm_null=='power_law') {
        ens.mtm.cl[,k] <- mtm.main$PowerLaw_95_power        
      } else if ( mtm_null=='ML96') {
        ens.mtm.cl[,k] <- astrochron::mtm.main$AR1_95_power
      } 
      ens.mtm.sigfreq[match(mtm.sigfreq$Frequency, mtm.main$Frequency, nomatch = 0),k] <- 1
      if(n.ens>1 & k%%round(n.ens/50)==0){
        setTxtProgressBar(pb,k)
      }
    }
    close(pb)
    
    # prepare significance assessment
    freqs.prob <- rowMeans(ens.mtm.sigfreq,na.rm = TRUE)
    pCL <- rowMedians(ens.mtm.cl,na.rm = TRUE)
    #fMat <- matrix(f,nrow=length(f),ncol=n.ens,byrow=F)
    
    # allocate output
    spec.ens = list(freqs = f, power = ens.mtm.power, power.CL=pCL, sig.freq=freqs.prob)
  }
  else if ( method=='nuspectral') {
    nt = length(time)
    tau = seq(min(time),max(time),length = max(nt %/% 5,10))
    
    # first iteration to define matrix sizes
    t = time[,tind[1]]; v = vals[,vind[1]] 
    nuspec <-  nuspectral:::nuwavelet_psd(t,v,sigma=sigma,taus = tau)
    noutrow <- length(nuspec$Frequency)
    
    #preallocate matrices
    #fMat = matrix(NA,ncol=n.ens,nrow=noutrow)
    #pMat = fMat
    #pMatSyn = fMat
    
    # 1st, heard-earned ensemble member
    pMat <- nuspec$Power
    freq <- nuspec$Frequency
    
    if (n.ens > 1){stop("Ensemble mode not supported with nuspectral")}
    # pb = txtProgressBar(min=1,max = n.ens,style = 3)  # run the loop with progress bar
    # for (k in 2:n.ens){
    #   t = time[,tind[k]]; v = vals[,vind[k]] 
    #   nuspec   <-  nuspectral:::nuwavelet_psd(t,v,sigma=sigma,taus = tau)
    #   pMat[,k] <- nuspec$Power
    #   fMat[,k] <- nuspec$Frequency
    # 
    #   if(k%%round(n.ens/10)==0){
    #     setTxtProgressBar(pb,k)
    #   }
    # }
    # close(pb)
    
    # allocate output
    spec.ens = list(freqs = freq, power = pMat, power.CL = NA)
    
  } else {stop("Unknown method: Valid choices are: 'mtm', 'redfit', 'nuspectral', or 'lomb-scargle' ")}
  
  return(spec.ens)
}