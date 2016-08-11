AR1 = function(X){
  ar=cor(X[-1],X[-length(X)],use="pairwise")
  return(ar)
}

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
}

pvalPearson.serial.corrected = function(r,n){
  #r is the correlation coeffient
  #n is the number of pairwise observations
  Tval = r * sqrt((n-2)/(1-r^2))
  
  #two tailed test
  p = pt(-abs(Tval),df = n-2)*2
  return(p)
  
}

matrix.corr.and.pvalue = function(M1,M2){
  M1=as.matrix(M1)
  M2=as.matrix(M2)
  if(nrow(M1)!=nrow(M2)){stop("M1 and M2 must have the same number of rows")}
  
  p=matrix(NA,nrow = ncol(M1)*ncol(M2))
  pAdj=p;
  r=p
  nens=nrow(p)
  pb <- txtProgressBar(min=1,max=nens,style=3)
  print(paste("Calculating",nens,"correlations"))
  for(i in 1:ncol(M1)){
    for(j in 1:ncol(M2)){
      r[j+ncol(M1)*(i-1)] = cor(M1[,i],M2[,j],use="pairwise")
      effN = effectiveN(M1[,i],M2[,j])
      pAdj[j+ncol(M1)*(i-1)] = pvalPearson.serial.corrected(r[j+ncol(M1)*(i-1)],effN)
      p[j+ncol(M1)*(i-1)] = pvalPearson.serial.corrected(r[j+ncol(M1)*(i-1)],sum(!is.na(M1[,i])&!is.na(M2[,j])))
    }
    setTxtProgressBar(pb, j+ncol(M1)*(i-1))
  }
  out = data.frame("r"=r,"p"=p,"pAdj"=pAdj)
  close(pb)
  return(out)
}




regress=function (X,Y){
  g=which(!apply(is.na(X),1,any) & !is.na(Y))
  X=X[g,]
  Y=Y[g]
  b=solve(t(X)%*%X)%*%(t(X)%*%Y)
  return(b)
}

regression.ens = function(timeX,valuesX,timeY,valuesY,binvec = NA,binstep = NA ,binfun=mean,max.ens=NA,percentiles=c(pnorm(-2:2)),plot.reg=TRUE,plot.alpha=0.2){
  #check to see if time and values are "column lists"
  if(is.list(timeX)){timeX=timeX$values}
  if(is.list(timeY)){timeY=timeY$values}
  if(is.list(valuesX)){valuesX=valuesX$values}
  if(is.list(valuesY)){valuesY=valuesY$values}
  
  
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
  
  if(is.na(binstep)){#if the binstep isn't specified
    binstep=abs(mean(diff(binvec)))
  }
  
  #get full X for the reconstruction
  fullX = bin.ens(time = timeX,values = valuesX,binvec = seq(min(timeX,na.rm=TRUE),max(timeX,na.rm=TRUE),by=binstep),binfun=binfun,max.ens=max.ens)
  
  
  
  #how many ensemble members?
  nensPoss = ncol(binX)*ncol(binY)
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
    rX = sample.int(ncol(binX),size = nens,replace = TRUE)
    rY = sample.int(ncol(binY),size = nens,replace = TRUE)
  }else{
    rX = c(t(matrix(rep(seq(1,ncol(binX)),times = ncol(binY)),ncol = ncol(binY))))
    rY = c(matrix(rep(seq(1,ncol(binY)),times = ncol(binX)),ncol = ncol(binX)))
  }
  
  #ones columns
  ones=matrix(1,nrow = nrow(binX))
  
  #setup progress bar
  pb <- txtProgressBar(min=1,max=nens,style=3)
  print(paste("Calculating",nens,"regressions"))
  
  modeled.Y.mat = matrix(NA,ncol=nens,nrow=nrow(fullX$matrix))
  
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
    regStats = data.frame(percentiles,"m" = ms[round(percentiles*N)],"b" = bs[round(percentiles*N)])
    row.names(regStats)=format(regStats$percentiles,digits = 2)
  }
  reg.ens.data=list("m"=m,"b"=b,"regStats"=regStats,"binX"=binX,"binY"=binY,"rX"=rX,"rY"=rY,"modeledY"=modeled.Y.mat)
  
  if(plot.reg){
    #scatter plot
    regPlot = plot.scatter.ens(binX,binY,alp=plot.alpha)
    #add trendlines
    regPlot = plot.trendlines.ens(mb.df = t(rbind(m,b)),xrange = range(binX,na.rm=TRUE), alp = plot.alpha/10,add.to.plot = regPlot$plot)
    reg.ens.data$scatterplot = regPlot
    
    #plot histograms of m and b
    mStats = regStats[,1:2]
    names(mStats)[2]="values"
    reg.ens.data$mHist = plot.hist.ens(m,ensStats = mStats)+xlab("Slope")
    bStats = regStats[,c(1,3)]
    names(bStats)[2]="values"
    reg.ens.data$bHist = plot.hist.ens(b,ensStats = bStats)+xlab("Intercept")
    
    
    #plot timeseries of regression and target over interval
    reg.ens.data$XPlot = plot.timeseries.ribbons(yearX,binX)
    reg.ens.data$YPlot = plot.timeseries.ribbons(yearX,binY,colorHigh = "red")
    
    
    #and plot reconstructions
    reg.ens.data$modeledYPlot = plot.timeseries.ribbons(X = fullX$time,Y=modeled.Y.mat)
    
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

cor.ens = function(time1,values1,time2,values2,binvec = NA,binstep = NA ,binfun=mean,max.ens=NA,percentiles=c(pnorm(-2:2)),plot.hist=TRUE){
  
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
  
  if(is.na(binvec)){
    if(is.na(binstep)){
      stop("Either a binvec or binstep must be specified")
    }else{
      binStart=min(c(time1,time2),na.rm = TRUE)
      binStop=max(c(time1,time2),na.rm = TRUE)
      binvec=seq(binStart,binStop,by=binstep)
    }
  }
  
  #create ensemble bins
  dum = bin.ens(time = time1,values = values1,binvec = binvec,binfun=binfun,max.ens=max.ens)
  binYear = dum$time
  bin1 = dum$matrix
  bin2 = bin.ens(time = time2,values = values2,binvec = binvec,binfun=binfun,max.ens=max.ens)$matrix
  
  #calculate the correlations
  #cormat=c(cor(bin1,bin2,use = "pairwise"))  #faster - but no significance...
  
  cor.df = matrix.corr.and.pvalue(bin1,bin2)
  
  
  #and the significance
  #pairwise observations
  
  
  #calculate some default statistics
  if(!all(is.na(percentiles))){
    cormatS = sort(cor.df$r)
    N=length(cormatS)
    corStats = data.frame(percentiles,"values" = cormatS[round(percentiles*N)])
    row.names(corStats)=format(corStats$percentiles,digits = 2)
  }
  
  
  
  cor.ens.data=list(cor.df = cor.df,corStats = corStats)
  
  if(plot.hist){
    library(ggplot2)
    cor.ens.data$plot.r = plot.hist.ens(cor.df$r,ensStats = corStats)
    cor.ens.data$plot.p = plot.hist.ens(cor.df$p)
    perc = data.frame("values"=0.05)
    row.names(perc)= "α = 0.05"
    cor.ens.data$plot.p=plot.hist.ens(cor.df$pAdj,fill="red",alp=.5,add.to.plot = cor.ens.data$plot.p,ensStats = perc)
  #need to add legend...
  }
  return(cor.ens.data)
  
}



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

