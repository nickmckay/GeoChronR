create.synthetic = function(time,values,nens=1){
  
  #check to see if time and values are "column lists"
  if(is.list(time)){time=time$values}
  if(is.list(values)){values=values$values}
  
  #make them all matrices
  time = as.matrix(time)
  values = as.matrix(values)
  
  #measure long term trend
  trend=predict(lm(values~time))
  #remove the trend
  notrend=values-trend
  #get necessary metadata
  m=mean(notrend,na.rm=TRUE)
  s=sd(notrend,na.rm=TRUE)
  a=acf(notrend,na.action=na.pass,plot=FALSE)
  ar=max(0,as.numeric(unlist(a[1])[1]))
  
  synValues = matrix(NA,nrow=nrow(time),ncol=nens)
  #go through ensemble members
  for(jj in 1:nens){
    #generate a random series with ar=ar
    rdata=arima.sim(model=list("ar"=ar),n=length(notrend))
    #remove any trend
    rtrend=predict(lm(rdata~time))
    rdata=rdata-rtrend
    #scale the same as data
    rdata=scale(rdata)*s+m
    #add the data trend in
    synValues[,jj]=rdata+trend
  }
  return(synValues)
}




powerSpectrum.ens = function(time,values,max.ens=NA,plot.opt=FALSE){
  library(lomb)#uses this library, for now...
  
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
  
  #preallocate
  fMat = matrix(NA,ncol=nens,nrow=nrow(time)-1)
  pMat = fMat
  pMatSyn = fMat
  
  pb = txtProgressBar(min=1,max = nens,style = 3)
  for(i in 1:nens){
    t = time[,sample.int(ncol(time),size = 1)]
    v = values[,sample.int(ncol(values),size = 1)]
    out = lsp(v,times=t,ofac=ofac,plot = F)
    syn = create.synthetic(t,v)#create synthetic timeseries
    pMatSyn[,i] =  lsp(syn,times=t,ofac=ofac,plot = F)$power
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