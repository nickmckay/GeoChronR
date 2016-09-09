#' @export
#' @param X a 1-column matrix or numeric dataset
#' @return ar coefficient estimate of AR1
AR1 = function(X){
  fit = arima(x = X, order = c(1, 0, 0))
  return(fit$coef[[1]])
}

#' @export
#' @param X a 1-column matrix or numeric dataset
#' @param Y a 1-column matrix or numeric dataset of the same length as X
#' @return effectiveN estimate of the effective sample size
effectiveN = function(X,Y){
  #from Bretherton 1999
  arX = AR1(X)
  n = sum(is.finite(X) & is.finite(Y))
  arY = AR1(Y)
  
  if(arX < 0 | arY < 0 ){#calculation is meaningless if either number is less than 0
    effN=n
  }else{
    effN = n *(1-arX*arY)/(1+arX*arY)
  }
  return(effN)
}

#' @export
#' @param r correlation coefficient
#' @param n sample size
#' @return p p-value
pvalPearson.serial.corrected = function(r,n){
  #r is the correlation coeffient
  #n is the number of pairwise observations
  Tval = r * sqrt((n-2)/(1-r^2))
  
  #two tailed test
  p = pt(-abs(Tval),df = n-2)*2
  return(p)
  
}
#' @export
#' @param M1 matrix of age-uncertain columns to correlate and calculate p-values
#' @param M2 matrix of age-uncertain columns to correlate and calculate p-values
#' @return out list of correlation coefficients (r) p-values (p) and autocorrelation corrected p-values (pAdj)

matrix.corr.and.pvalue = function(M1,M2){
  M1=as.matrix(M1)
  M2=as.matrix(M2)
  if(nrow(M1)!=nrow(M2)){stop("M1 and M2 must have the same number of rows")}
  
  p=matrix(NA,nrow = ncol(M1)*ncol(M2))
  pAdj=p;
  r=p
  nens=nrow(p) # number of ensemble members
  pb <- txtProgressBar(min=1,max=nens,style=3)
  print(paste("Calculating",nens,"correlations"))
  for(i in 1:ncol(M1)){
    for(j in 1:ncol(M2)){
      r[j+ncol(M1)*(i-1)] = cor(M1[,i],M2[,j],use="pairwise")
      effN = effectiveN(M1[,i],M2[,j])
      pAdj[j+ncol(M1)*(i-1)] = pvalPearson.serial.corrected(r[j+ncol(M1)*(i-1)],effN)
      p[j+ncol(M1)*(i-1)] = pvalPearson.serial.corrected(r[j+ncol(M1)*(i-1)],sum(!is.na(M1[,i])&!is.na(M2[,j])))
    
    setTxtProgressBar(pb, j+ncol(M1)*(i-1))
    }
  }
  # apply false discovery rate procedure to ADJUSTED p-values
  fdrOut = fdr(pAdj,qlevel=0.05,method="original",adjustment.method='mean') 
  sig_fdr = matrix(0,nens)
  sig_fdr[fdrOut] = 1 
 
    # Rmks:
    # 1) probably qlevel should be an optional parameter 
    # 2) could silence the FDR screen output
  # export to data frame
  out = data.frame("r"=r,"pSerial"=pAdj,"pRaw"=p,"sig_fdr"=sig_fdr)
  close(pb)
  return(out)
}

#' @export
#' @param X a matrix of predictor data
#' @param Y a vector of predictand data
#' @return B model coeffiecients
regress=function (X,Y){
  g=which(!apply(is.na(X),1,any) & !is.na(Y))
  X=X[g,]
  Y=Y[g]
  b=solve(t(X)%*%X)%*%(t(X)%*%Y)  # this is straight up OLS. Why not use lm? 
  return(b)
}


#' @export
regression.ens = function(timeX,valuesX,timeY,valuesY,binvec = NA,binstep = NA ,binfun=mean,max.ens=NA,percentiles=c(2.5,25,50,75,97.5),plot_reg=TRUE,plot_alpha=0.2,recon.binvec=NA,minObs=10){
  #check to see if time and values are "column lists"
  if(is.list(timeX)){
    otx=timeX
    timeX=timeX$values}
  if(is.list(timeY)){
    oty=timeY
  timeY=timeY$values}
  if(is.list(valuesX)){
    ovx=valuesX
    valuesX=valuesX$values}
  if(is.list(valuesY)){
    ovy=valuesY
    valuesY=valuesY$values}
  
  
  #make them all matrices
  timeX = as.matrix(timeX)
  timeY = as.matrix(timeY)
  valuesX = as.matrix(valuesX)
  valuesY = as.matrix(valuesY)
  
  if(nrow(timeX) != nrow(valuesX)){stop("timeX and valuesX must have the same number of rows (observations)")}
  if(nrow(timeY) != nrow(valuesY)){stop("timeY and valuesY must have the same number of rows (observations)")}
  
  if(all(is.na(binvec))){
    if(is.na(binstep)){
      stop("Either a binvec or binstep must be specified")
    }else{
      #look for common overlap
      binStart=max(c(min(timeX,na.rm=TRUE),min(timeY,na.rm=TRUE)))
      binStop=min(c(max(timeX,na.rm=TRUE),max(timeY,na.rm=TRUE)))
      binvec=seq(binStart,binStop,by=binstep)
    }
  }
  
  #create ensemble bins
  dum = bin.ens(time = timeX,values = valuesX,binvec = binvec,binfun=binfun,max.ens=max.ens)
  yearX = dum$time
  binX = dum$matrix
  binY = bin.ens(time = timeY,values = valuesY,binvec = binvec,binfun=binfun,max.ens=max.ens)$matrix
  
  #remove columns that have less than minObs datapoints
  good = which(apply(!is.na(binX),2,sum)>=minObs)
  if(length(good)==0){
    stop(paste("none of the columns have",minObs,"or more datapoints"))
  }
  binX = as.matrix(binX[,good])
  
  
  good = which(apply(!is.na(binY),2,sum)>=minObs)
  if(length(good)==0){
    stop(paste("none of the columns have",minObs,"or more datapoints"))
  }
  binY = as.matrix(binY[,good])

  
  if(is.na(binstep)){#if the binstep isn't specified
    binstep=abs(mean(diff(binvec,na.rm=TRUE)))
  }
  
  #check for a reconstruction binvec
  if(all(is.na(recon.binvec))){
    recon.binvec = seq(min(timeX,na.rm=TRUE),max(timeX,na.rm=TRUE),by=binstep)
  }
  
  #get full X for the reconstruction
  fullX = bin.ens(time = timeX,values = valuesX,binvec = recon.binvec,binfun=binfun,max.ens=max.ens)
  
  
  #how many ensemble members?
  nensPoss = NCOL(binX)*NCOL(binY)
  nens=nensPoss
  

  
  if(!is.na(max.ens)){
    if(max.ens<nensPoss){
      nens=max.ens
    }
  }
  
  randomize=FALSE
  if(nens<nensPoss){#if were examining only a subset of the possible permutations, randomize which ones we sample
    randomize=TRUE
  }
  
  #do the regression...
  m=matrix(NA,ncol = nens)
  b=m
  if(randomize){
    rX = sample.int(NCOL(binX),size = nens,replace = TRUE)
    rY = sample.int(NCOL(binY),size = nens,replace = TRUE)
  }else{
    rX = c(t(matrix(rep(seq(1,NCOL(binX)),times = NCOL(binY)),ncol = NCOL(binY))))
    rY = c(matrix(rep(seq(1,NCOL(binY)),times = NCOL(binX)),ncol = NCOL(binX)))
  }
  
  #ones columns
  ones=matrix(1,nrow = NROW(binX))
  
  #setup progress bar
  pb <- txtProgressBar(min=1,max=nens,style=3)
  print(paste("Calculating",nens,"regressions"))
  
  modeled.Y.mat = matrix(NA,ncol=nens,nrow=NROW(fullX$matrix))
  
  #do the regressions
  for(i in 1:nens){
    
    B=regress(X = cbind(binX[,rX[i]],ones),Y = binY[,rY[i]])
    m[i]=B[1]
    b[i]=B[2]
    
    #calculate reconstruction
    XC=cbind(as.matrix(fullX$matrix[,rX[i]]),matrix(1,nrow=length(as.matrix(fullX$matrix[,rX[i]]))))
    modeled.Y.mat[,i] = XC%*%B 
    
    if(i%%100==0){
      setTxtProgressBar(pb, i)
    }
  }
  close(pb)
  
  #calculate some default statistics
  if(!all(is.na(percentiles))){
    ms = sort(m)
    bs = sort(b)
    N=length(ms)
    regStats = data.frame(percentiles,"m" = ms[round(percentiles*N/100)],"b" = bs[round(percentiles*N/100)])
    row.names(regStats)=format(regStats$percentiles,digits = 2)
  }
  reg.ens.data=list("m"=m,"b"=b,"regStats"=regStats,"binX"=binX,"binY"=binY,"rX"=rX,"rY"=rY,"modeledY"=modeled.Y.mat)
  
  if(plot_reg){
    #scatter plot
    regPlot = plot_scatter.ens(binX,binY,alp=plot_alpha)
    #add trendlines
    regPlot = plot_trendlines.ens(mb.df = t(rbind(m,b)),xrange = range(binX,na.rm=TRUE), alp = plot_alpha/10,add.to.plot = regPlot$plot)
    reg.ens.data$scatterplot = regPlot
    if(exists("ovx")){
      pl=ovx
      reg.ens.data$scatterplot = reg.ens.data$scatterplot+xlab(paste0(pl$variableName, " (",pl$units,")"))
    }
    if(exists("ovy")){
      pl=ovy
      reg.ens.data$scatterplot = reg.ens.data$scatterplot+ylab(paste0(pl$variableName, " (",pl$units,")"))
    }
    
    
    
    #plot histograms of m and b
    mStats = regStats[,1:2]
    names(mStats)[2]="values"
    reg.ens.data$mHist = plot_hist.ens(m,ensStats = mStats)+xlab("Slope")
    bStats = regStats[,c(1,3)]
    names(bStats)[2]="values"
    reg.ens.data$bHist = plot_hist.ens(b,ensStats = bStats)+xlab("Intercept")
    
    
    binY[is.nan(binY)]=NA
    binX[is.nan(binX)]=NA

    #plot timeseries of regression and target over interval
    reg.ens.data$XPlot = plot_timeseries.ribbons(yearX,binX)+ggtitle("Calibration interval predictor")
    if(exists("otx")){
      pl=otx
      reg.ens.data$XPlot = reg.ens.data$XPlot+xlab(paste0(pl$variableName, " (",pl$units,")"))
    }
    if(exists("ovx")){
      pl=ovx
      reg.ens.data$XPlot = reg.ens.data$XPlot+ylab(paste0(pl$variableName, " (",pl$units,")"))
    }
    
    reg.ens.data$YPlot = plot_timeseries.ribbons(yearX,binY,colorHigh = "red")+ggtitle("Calibration interval predictand")
    if(exists("oty")){
      pl=oty
      reg.ens.data$YPlot = reg.ens.data$XPlot+xlab(paste0(pl$variableName, " (",pl$units,")"))
    }
    if(exists("ovy")){
      pl=ovy
      reg.ens.data$YPlot = reg.ens.data$XPlot+ylab(paste0(pl$variableName, " (",pl$units,")"))
    }
    
    
    
    #and plot reconstructions
    reg.ens.data$modeledYPlot = plot_timeseries.ribbons(X = fullX$time,Y=modeled.Y.mat)+ggtitle("Calibrated record using ensemble regression")
    if(exists("otx")){
      pl=otx
      reg.ens.data$modeledYPlot = reg.ens.data$modeledYPlot+xlab(paste0(pl$variableName, " (",pl$units,")"))
    }
    if(exists("ovy")){
      pl=ovy
      reg.ens.data$modeledYPlot = reg.ens.data$modeledYPlot+ylab(paste0(pl$variableName, " (",pl$units,")"))
    }
    
    
    library(gridExtra)
    lay = rbind(c(1,1,3,3,4,4),
                c(2,2,3,3,5,5),
                c(6,6,6,6,6,6),
                c(6,6,6,6,6,6))
    
    
    reg.ens.data$summaryPlot = grid.arrange(grobs = list(reg.ens.data$YPlot,reg.ens.data$XPlot,reg.ens.data$scatterplot,
                                                         reg.ens.data$mHist,reg.ens.data$bHist,reg.ens.data$modeledYPlot),
                                            layout_matrix=lay)  
    
    
    
    
  }
  
  
  return(reg.ens.data)
  
}


#' @export
cor.ens = function(time1,values1,time2,values2,binvec = NA,binstep = NA ,binfun=mean,max.ens=NA,percentiles=c(.025,.25,.5,.75,.975),plot_hist=TRUE,minObs=10){
  
  #check to see if time and values are "column lists"
  if(is.list(time1)){time1=time1$values}
  if(is.list(time2)){time2=time2$values}
  if(is.list(values1)){values1=values1$values}
  if(is.list(values2)){values2=values2$values}
  
  #make them all matrices
  time1 = as.matrix(time1)
  time2 = as.matrix(time2)
  values1 = as.matrix(values1)
  values2 = as.matrix(values2)
  
  if(nrow(time1) != nrow(values1)){stop("time1 and values1 must have the same number of rows (observations)")}
  if(nrow(time2) != nrow(values2)){stop("time2 and values2 must have the same number of rows (observations)")}
  
  if(all(is.na(binvec))){
    if(is.na(binstep)){
      stop("Either a binvec or binstep must be specified")
    }else{
      #look for common overlap
      binStart=floor(max(c(min(time1,na.rm=TRUE),min(time2,na.rm=TRUE))))
      binStop=ceiling(min(c(max(time1,na.rm=TRUE),max(time2,na.rm=TRUE))))
      print(paste("binning from",binStart,"to",binStop,"..."))
      binvec=seq(binStart,binStop,by=binstep)
    }
  }
  
  #create ensemble bins
  dum = bin.ens(time = time1,values = values1,binvec = binvec,binfun=binfun,max.ens=max.ens)
  year = dum$time
  bin1 = dum$matrix
  bin2 = bin.ens(time = time2,values = values2,binvec = binvec,binfun=binfun,max.ens=max.ens)$matrix
  
  #remove columns that have less than minObs datapoints
  good = which(apply(!is.na(bin1),2,sum)>=minObs)
  if(length(good)==0){
    stop(paste("none of the columns have",minObs,"or more datapoints"))
  }
  bin1 = as.matrix(bin1[,good])
  
  
  good = which(apply(!is.na(bin2),2,sum)>=minObs)
  if(length(good)==0){
    stop(paste("none of the columns have",minObs,"or more datapoints"))
  }
  bin2 = as.matrix(bin2[,good])
  
  
  
  #calculate the correlations
  #cormat=c(cor(bin1,bin2,use = "pairwise"))  #faster - but no significance...
  
  cor.df = matrix.corr.and.pvalue(bin1,bin2)

  #and the significance
  #pairwise observations
  
  
  #calculate some default statistics
  if(!all(is.na(percentiles))){
    pctl = quantile(cor.df$r,probs = percentiles/100)
    corStats = data.frame(percentiles,"values" = pctl)
    #row.names(corStats)=format(corStats$percentiles,digits = 2) # it appears that the rows are already well formatted
    cor.ens.data=list(cor.df = cor.df,corStats = corStats)
    
  }else{
    corStats=NA
    cor.ens.data=list(cor.df = cor.df)
  }
  
  if(plot_hist){
    library(ggplot2)
    cor.ens.data$plot_r = plot_corr.ens(cor.df,corStats)
    cor.ens.data$plot_p = plot_pvals.ens(cor.df)
  }
  return(cor.ens.data)
  
}


#' @export
bin.ens = function(time,values,binvec,binfun=mean,max.ens=NA){
  #takes ensembles in time and/or values and creates a matrix of data for future analysis
  time = as.matrix(time)
  values = as.matrix(values)
  
  #if it's an age ensemble only
  if(ncol(time)>1 & ncol(values)==1){
    if(!is.na(max.ens)){
      if(max.ens<ncol(time)){
        time=time[,1:max.ens]
      }
    }
    binMat = apply(time,MARGIN = 2,function(x) bin(time = x,values = values,binvec = binvec,binfun = binfun)$y)
    
    #if it's a value ensemble only
  }else if(ncol(time)==1 & ncol(values)>1){
    if(!is.na(max.ens)){
      if(max.ens<ncol(values)){
        values=values[,1:max.ens]
      }
    }
    binMat = apply(values,MARGIN = 2,function(x) bin(time = time,values = x,binvec = binvec,binfun = binfun)$y)
    
    #if it's a value AND age ensemble
  }else if(ncol(time)>1 & ncol(values)>1){
    nx = ncol(time)
    ny = ncol(values)
    if(!is.na(max.ens)){
      nens=min(max(nx,ny),max.ens)
    }else{
      nens = max(nx,ny)
    }
    if(nx>=ny){
      binMat = apply(time[,1:nens],MARGIN = 2,function(x) bin(time = x,values = values[,sample.int(ny,size=1)],binvec = binvec,binfun = binfun)$y)
    }else{
      binMat = apply(values[,1:nens],MARGIN = 2,function(x) bin(time = time[,sample.int(nx,size=1)],values = x,binvec = binvec,binfun = binfun)$y)
    }
    
    #both are single values
  }else{
    #just regular bin
    binMat = bin(time = time,values = values,binvec = binvec,binfun = binfun)$y
  }
  
  binMat = as.matrix(binMat)
  bin_x = apply(cbind(binvec[-1],binvec[-length(binvec)]),1,mean)
  binned=list("time"=bin_x,"matrix" = binMat)
  return(binned)
  
}
#' @export
bin = function(time,values,binvec,binfun = mean){
  #function that puts data into appropriate bins, based on the time and the binning vector
  #the bin vector describes the edges of the bins
  #binfun is the function to use for the binning, mean, sum, sd are all reasonable options
  bin_y = rep(NA,times = length(binvec)-1)
  bin_x = apply(cbind(binvec[-1],binvec[-length(binvec)]),1,mean)
  
  for(i in 1:length(bin_y)){
    q = which(time > binvec[i] & time <= binvec[i+1])
    bin_y[i] = binfun(values[q],na.rm=TRUE)
  }
  
  binned = data.frame(x=bin_x,y=bin_y)
  return(binned)
  
}
#' @export
bin.TS = function(TS,timeVar=c("ageEnsemble"),binvec,max.ens=1000,na.col.rm=TRUE){
  timeList = lapply(TS,"[[",timeVar)
  valueList = lapply(TS,"[[","paleoData_values")
  
  binMat = vector(mode="list",length = length(timeList))
  pb <- txtProgressBar(min=1,max=length(timeList),style=3)
  
  for(i in 1:length(timeList)){
    binMat[[i]]=bin.ens(time = timeList[[i]],values = valueList[[i]],binvec = binvec,max.ens = max.ens)
    if(na.col.rm){
      allNa=which(apply(is.na(binMat[[i]]$matrix),2,all))
      if(length(allNa)>0){
        binMat[[i]]$matrix = binMat[[i]]$matrix[,-allNa]
      }
    }
    
    setTxtProgressBar(pb,i)
  }
  close(pb)
  return(binMat)  
  
}

