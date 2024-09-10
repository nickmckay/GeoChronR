
#' Use the write function from rEDM depending on the version
#'
#' @param ... Arguments to pass to surrogate
#' @importFrom rEDM SurrogateData
#' @return Surrogate output
#' @family utility
#' @export
surrogateDataFun <- function(...){
    out <- rEDM::SurrogateData(...)
  return(out)
}


#' @export
#' @title Estimate Auto-Regressive coefficient at 1-timesetep
#' @description estimates ar1 using the arima() function
#' @author Julien Emile-Geay
#' @param x a 1-column matrix or numeric dataset
#' @return ar coefficient estimate of ar1
#' @family correlation
ar1 = function(x){
  fit = suppressWarnings(arima(x = x, order = c(1, 0, 0)))
  return(fit$coef[[1]])
}

#' @export
#' @title Correlations and their significance according to AR(1) benchmarks
#' @description Generate parametric or non-parametric surrogates of two series X & Y 
#' @author Julien Emile-Geay
#' @author Nick McKay
#' @param X a 1-column vector
#' @param Y a 1-column vector of the same 
#' @param n.sim number of simulations
#' @param cor.method correlation method to pass to cor() "pearson" (default), "kendall", or "spearman"
#' @param method the method applies. Possible choices are "isospectral" (default) and "isopersistent"
#' @return output
#' @family correlation
pvalMonteCarlo = function(X,Y,n.sim=100,method = "isospectral",cor.method = "pearson"){
  if(is.matrix(X)){
    if(min(dim(X)) != 1){
      stop("the input to this function should be a vector, not a matrix")
    }
  }
  if(is.matrix(Y)){
    if(min(dim(Y)) != 1){
      stop("the input to this function should be a vector, not a matrix")
    }
  }
  
  
  nx = length(X)
  ny = length(Y)
  
  if(nx != ny){
    stop("X and Y must be the same length")
  }  
  
  rhoXY = cor(X,Y,use="pairwise.complete.obs",method = cor.method)
  
  if(grepl(method,pattern = "persis",ignore.case = T)){
    tdum = 1:nx  # dummy time axis
    # generate AR(1) surrogates
    X.surr <- try(ar1Surrogates(tdum,X,detrend=TRUE,method='redfit',n.ens=n.sim),silent = TRUE) # replace with 
    Y.surr <- try(ar1Surrogates(tdum,Y,detrend=TRUE,method='redfit',n.ens=n.sim),silent = TRUE)
  }else if(grepl(method,pattern = "spectra",ignore.case = T)){
    
    ix.good <- which(is.finite(X))
    X.surr <- matrix(NA,nrow = length(X),ncol = n.sim)
    X.surr[ix.good,] <- try(surrogateDataFun(X[ix.good],method = 'ebisuzaki',num_surr = n.sim),silent = TRUE)
    
    iy.good <- which(is.finite(Y))
    Y.surr <- matrix(NA,nrow = length(Y),ncol = n.sim)
    Y.surr[iy.good,] <- try(surrogateDataFun(Y[iy.good],method = 'ebisuzaki',num_surr = n.sim),silent = TRUE)
  }else{
    stop("method must be 'isopersistent' or 'isospectral'")
  }
  
  if(!is.matrix(X.surr) | !is.matrix(Y.surr)){
    #surrogates failed, return NAs
    return(matrix(NA,nrow = length(X)))
  }
  
  
  cor.mat1 = cor(X,Y.surr,use="pairwise.complete.obs",method = cor.method) # X vs Y-like noise
  cor.mat2 = cor(Y,X.surr,use="pairwise.complete.obs",method = cor.method) # Y vs X-like noise
  cor.mat = cbind(cor.mat1,cor.mat2)  # bind together
  rho = abs(cor.mat[1,])  # convert to vector
  #  compute sampling distribution 
  if(length(rho) < 1000){
    rho_cdf  <- cdfDensity(rho) # turn into CDF
  }else{
    rho_cdf <- ecdf(rho)  # this is the empirical way; OK if large ensemble
  }
  # estimate test p-value
  pval = 1-rho_cdf(abs(rhoXY))
  
  return(pval)
}

cdfDensity <- function(x){
  f <- stats::density(x,from=0,to=1,na.rm = TRUE) # estimate density
#modified from spatstat.univar::CDF.density  
  xx <- f$x
  yy <- f$y
  nn <- length(xx)
  Fx <- cumsum(yy * c(0, diff(xx)))
  Fx <- Fx/Fx[nn]
  FF <- approxfun(xx, Fx, method = "linear", rule = 2)
  return(FF)
}

#' @export
#' @title Estimate effective sample size accounting for autocorrelation
#' @description Bretherton et al., 1999 estimate of effective sample size.
#' @author Nick McKay
#' @param X a 1-column matrix or numeric dataset
#' @param Y a 1-column matrix or numeric dataset of the same length as X
#' @return estimate of the effective sample size
#' @family correlation
effectiveN = function(X,Y){
  #from Bretherton 1999
  arX = ar1(X)
  n = sum(is.finite(X) & is.finite(Y))
  arY = ar1(Y)
  
  if(arX < 0 | arY < 0 ){#calculation is meaningless if either number is less than 0
    effN=n
  }else{
    effN = n *(1-arX*arY)/(1+arX*arY)
  }
  return(effN)
}

#' @export
#' @title Calculate correlation p-value given sample size. 
#' @description Calculate Pearson p-values accounting for effective sample size
#' @author Nick McKay
#' @param r correlation coefficient
#' @param n sample size
#' @return p-value based on two-tailed t-test
#' @family correlation
pvalPearsonSerialCorrected = function(r,n){
  #r is the correlation coeffient
  #n is the number of pairwise observations
  Tval = r * sqrt((n-2)/(1-r^2))
  
  #two tailed test
  p = pt(-abs(Tval),df = n-2)*2
  return(p)
}
#' @export
#' @family correlation
#' @title Matrix correlation
#' @description Calculates correlations and associated p-values for two ensemble matrices (or vectors) 
#' @author Nick McKay
#' @author Julien Emile-Geay
#'
#' @param ens.1 matrix of age-uncertain columns to correlate and calculate p-values
#' @param ens.2 matrix of age-uncertain columns to correlate and calculate p-values
#' @param max.ens optionally limit the number of ensembles calculated (default = NA)
#' @param isospectral estimate significance using the Ebisuzaki method (default = TRUE)
#' @param isopersistent estimate significance using the isopersistence method (default = FALSE)
#' @param p.ens number of ensemble members to use for isospectral and/or isopersistent methods (default = 100)
#' @param gaussianize Boolean flag indicating whether the values should be mapped to a standard Gaussian prior to analysis.
#' @param cor.method correlation method to pass to cor() "pearson" (default), "kendall", or "spearman". Note that because the standard Student's T-test for significance is inappropriate for Kendall's Tau correlations, the raw and effective-N significance estimates will be NA when using "kendall"
#'
#' @return out list of correlation coefficients (r) p-values (p) and autocorrelation corrected p-values (pAdj)
corMatrix = function(ens.1,
                     ens.2,
                     max.ens = NA,
                     isospectral = TRUE, 
                     isopersistent = FALSE,
                     p.ens = 100,
                     gaussianize = TRUE,
                     cor.method = "pearson"
){
  
  ens.1=as.matrix(ens.1)
  ens.2=as.matrix(ens.2)
  if(nrow(ens.1)!=nrow(ens.2)){stop("ens.1 and ens.2 must have the same number of rows")}
  
  if(gaussianize){
    ens.1 <- gaussianize(ens.1)
    ens.2 <- gaussianize(ens.2)
  }
  
  p=matrix(NA,nrow = ncol(ens.1)*ncol(ens.2))
  pIsospectral <- pIsopersistent <- pAdj <- p
  r=p
  n.ens=nrow(p) # number of ensemble members
  ncor <- ifelse(is.na(max.ens),n.ens,max.ens)
  pb <- txtProgressBar(min=0,max=n.ens,style=3)
  print(paste("Calculating",ncor,"correlations"))
  
  for(i in 1:ncol(ens.1)){
    for(j in 1:ncol(ens.2)){
      #test for singularity
      effN = try(effectiveN(ens.1[,i],ens.2[,j]),silent = TRUE)
      if(is.numeric(effN)){
        r[j+ncol(ens.2)*(i-1)] <- cor(ens.1[,i],ens.2[,j],use="pairwise",method = cor.method)
        #calculate raw
        if(cor.method == "kendall"){#just NAs here
          p[j+ncol(ens.2)*(i-1)] <- NA
          #calculate adjust p-value (Bretherton 1999)
          pAdj[j+ncol(ens.2)*(i-1)] <- NA
        }else{
          p[j+ncol(ens.2)*(i-1)] <- pvalPearsonSerialCorrected(r[j+ncol(ens.2)*(i-1)],sum(!is.na(ens.1[,i])&!is.na(ens.2[,j])))
          #calculate adjust p-value (Bretherton 1999)
          pAdj[j+ncol(ens.2)*(i-1)] <- pvalPearsonSerialCorrected(r[j+ncol(ens.2)*(i-1)],effN)
        }
        #calculate isopersist
        if(isopersistent){
          pIsopersistent[j+ncol(ens.2)*(i-1)] <- pvalMonteCarlo(ens.1[,i],
                                                                ens.2[,j],
                                                                n.sim = p.ens,
                                                                method = "isopersistent",
                                                                cor.method = cor.method)
        }
        #calculate isospectral
        if(isospectral){
          pIsospectral[j+ncol(ens.2)*(i-1)] <- pvalMonteCarlo(ens.1[,i],ens.2[,j],n.sim = p.ens,method = "isospectral",cor.method = cor.method)
        }
        
        
      }
      setTxtProgressBar(pb, j+ncol(ens.2)*(i-1))
    }
  }
  
  pAdj[!is.finite(pAdj)]=1#This is for instances whenn NEff <=2. I guess this is a reasonable solution?
  
  
  out <- data.frame("r"=r,
                    "pSerial"=pAdj,
                    "pRaw"=p,
                    "pIsopersistent" = pIsopersistent,
                    "pIsospectral" = pIsospectral)
  
  if(!isospectral){
    out <- dplyr::select(out,-"pIsospectral")
  }
  if(!isopersistent){
    out <- dplyr::select(out,-"pIsopersistent")
  }
  
  if(!is.na(max.ens)){
    out <- out[seq_len(max.ens),]
  }
  close(pb)
  return(out)
}

#' @export
#' @title Simple ordinary least squeares regression
#' @description Simple regression function. Faster than lm()
#' @author Nick McKay
#' @param X a matrix of predictor data
#' @param Y a vector of predictand data
#' @return model coefficients
#' @family regress
regress=function (X,Y){
  g=which(!apply(is.na(X),1,any) & !is.na(Y))
  X=X[g,]
  Y=Y[g]
  b=solve(t(X)%*%X)%*%(t(X)%*%Y)  # this is straight up OLS. Why not use lm? - Answer: this is a bit faster computationally. NM.
  return(b)
}


#' @export
#' @title Ensemble regression
#' @description This is the primary function for ensemble regression. It will take ensemble values in time and/or values in the predictor (X), and regress them on ensemble values in time and/or values in Y (the predictand). The function will then apply the ensemble linear model to the full length of X to create a modeled Y. Will also optionally create plots. 
#' @param time.x matrix of age/time ensembles, or single column
#' @param values.x matrix of values ensembles, or single column
#' @param time.y matrix of age/time ensembles, or single column
#' @param values.y matrix of values ensembles, or single column
#' @param bin.vec vector of bin edges for binning step
#' @param bin.step spacing of bins, used to build bin step
#' @param bin.fun function to use during binning (mean, sd, and sum all work)
#' @param max.ens maximum number of ensemble members to regress
#' @param percentiles quantiles to calculate for regression parameters
#' @param recon.bin.vec bin vector to use for the modeled regression.
#' @param min.obs minimum number of points required to calculate regression
#' @param gaussianize Boolean flag indicating whether the values should be mapped to a standard Gaussian prior to analysis.
#' @return list of ensemble output
#' @author Nick McKay
#' @family regress
#' @section Long-form example:
#' \href{http://nickmckay.github.io/GeoChronR/articles/regression.html}{View a full-fledged example of how to use this function.}

regressEns = function(time.x,
                      values.x,
                      time.y,
                      values.y,
                      bin.vec = NA,
                      bin.step = NA,
                      bin.fun=mean,
                      max.ens=NA,
                      percentiles=c(.025,.25,.50,.75,0.975),
                      recon.bin.vec=NA,
                      min.obs=10,
                      gaussianize = TRUE){
  #time and values must be "column lists"
  if(!is.list(time.x) | !is.list(time.y) | !is.list(values.x) | !is.list(values.y)){
    stop("time.x, time.y, values.x and values.y must all be ``variable lists'' (output of selectData)")
  }
  
  otx=time.x
  oty=time.y
  ovx=values.x
  ovy=values.y
  
  if(gaussianize){
    values.x$values <- gaussianize(values.x$values)
    values.y$values <- gaussianize(values.y$values)
  }
  
  aligned = alignTimeseriesBin(time.x,values.x,time.y,values.y,bin.vec = bin.vec,bin.step = bin.step ,bin.fun=bin.fun,max.ens=max.ens,min.obs=min.obs)
  
  
  yearX = aligned$yearBins
  binX = aligned$binX
  binY = aligned$binY
  
  #check for a reconstruction bin.vec
  if(all(is.na(recon.bin.vec))){
    recon.bin.vec = seq(min(time.x,na.rm=TRUE),max(time.x,na.rm=TRUE),by=abs(aligned$bin.step))
  }
  
  #get full X for the reconstruction
  fullX = binEns(time = as.matrix(time.x$values),values = as.matrix(values.x$values),bin.vec = recon.bin.vec,bin.fun=bin.fun,max.ens=max.ens)
  
  
  #how many ensemble members?
  n.ensPoss = NCOL(binX)*NCOL(binY)
  n.ens=n.ensPoss
  
  
  
  if(!is.na(max.ens)){
    if(max.ens<n.ensPoss){
      n.ens=max.ens
    }
  }
  
  randomize=FALSE
  if(n.ens<n.ensPoss){#if were examining only a subset of the possible permutations, randomize which ones we sample
    randomize=TRUE
  }
  
  #do the regression...
  m=matrix(NA,ncol = n.ens)
  b=m
  if(randomize){
    rX = sample.int(NCOL(binX),size = n.ens,replace = TRUE)
    rY = sample.int(NCOL(binY),size = n.ens,replace = TRUE)
  }else{
    rX = c(t(matrix(rep(seq(1,NCOL(binX)),times = NCOL(binY)),ncol = NCOL(binY))))
    rY = c(matrix(rep(seq(1,NCOL(binY)),times = NCOL(binX)),ncol = NCOL(binX)))
  }
  
  #ones columns
  ones=matrix(1,nrow = NROW(binX))
  
  #setup progress bar
  pb <- txtProgressBar(min=1,max=n.ens,style=3)
  print(paste("Calculating",n.ens,"regressions"))
  
  modeled.Y.mat = matrix(NA,ncol=n.ens,nrow=NROW(fullX$matrix))
  
  #do the regressions
  for(i in 1:n.ens){
    
    B=regress(X = cbind(binX[,rX[i]],ones),Y = binY[,rY[i]])
    m[i]=B[1]
    b[i]=B[2]
    
    #calculate reconstruction
    XC=cbind(as.matrix(fullX$matrix[,rX[i]]),matrix(1,nrow=length(as.matrix(fullX$matrix[,rX[i]]))))
    modeled.Y.mat[,i] = XC%*%B 
    
    modeled = list(values = modeled.Y.mat,units = ovy$units, variableName = ovy$variableName, variableType= "inferredVariable")
    
    
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
  reg.ens.data=list("m"=m,"b"=b,"regStats"=regStats,"binX"=binX,"binY"=binY,"rX"=rX,"rY"=rY,"modeledY"=modeled.Y.mat,time.x = otx,values.x= ovx,time.y=oty,values.y=ovy,modeled = modeled,yearX = yearX,modeledYear = fullX$time)
  
  
  return(reg.ens.data)
  
}


#' @export
#' @title Ensemble correlation
#' 
#' @description Primary function for calculating correlation ensembles
#' @author Nick McKay
#' @importFrom dplyr select bind_cols
#' @importFrom magrittr %>% 
#' @importFrom purrr map_dfc map_dfr
#'
#' @param time.1 matrix of age/time ensembles, or single column
#' @param values.1 matrix of values ensembles, or single column
#' @param time.2 matrix of age/time ensembles, or single column
#' @param values.2 matrix of values ensembles, or single column
#' @param bin.vec vector of bin edges for binning step
#' @param bin.step spacing of bins, used to build bin step
#' @param bin.fun function to use during binning (mean, sd, and sum all work)
#' @param percentiles quantiles to calculate for regression parameters
#' @param min.obs minimum number of points required to calculate regression
#' @param max.ens Maximum number of ensembles to use
#' @param gaussianize Convert data to Gaussian distribution before correlating?
#' @param fdr.qlevel target false discovery rate (most users won't want to change this)
#'
#' @inheritDotParams corMatrix
#' @return list of ensemble output and percentile information
#' @section Long-form example:
#' \href{http://nickmckay.github.io/GeoChronR/articles/correlation.html}{View a full-fledged example of how to use this function.}
corEns = function(time.1,
                  values.1,
                  time.2,
                  values.2,
                  bin.vec = NA,
                  bin.step = NA,
                  bin.fun=mean,
                  max.ens=NA,
                  percentiles=c(.025,.25,.5,.75,.975),
                  min.obs=10,
                  fdr.qlevel = 0.05,
                  gaussianize = TRUE,
                  ...){
  
  #check to see if time and values are "column lists"
  if(is.list(time.1)){time.1=time.1$values}
  if(is.list(time.2)){time.2=time.2$values}
  if(is.list(values.1)){values.1=values.1$values}
  if(is.list(values.2)){values.2=values.2$values}
  
  #make them all matrices
  time.1 = as.matrix(time.1)
  time.2 = as.matrix(time.2)
  values.1 = as.matrix(values.1)
  values.2 = as.matrix(values.2)
  
  if(nrow(time.1) != nrow(values.1)){stop("time.1 and values.1 must have the same number of rows (observations)")}
  if(nrow(time.2) != nrow(values.2)){stop("time.2 and values.2 must have the same number of rows (observations)")}
  
  if(all(is.na(bin.vec))){
    if(any(is.na(bin.step))){
      stop("Either a bin.vec or bin.step must be specified")
    }else{
      #look for common overlap
      binStart=floor(max(c(min(time.1,na.rm=TRUE),min(time.2,na.rm=TRUE))))
      binStop=ceiling(min(c(max(time.1,na.rm=TRUE),max(time.2,na.rm=TRUE))))
      print(paste("binning from",binStart,"to",binStop,"..."))
      bin.vec=seq(binStart,binStop,by=bin.step)
    }
  }
  
  #create ensemble bins
  dum = binEns(time = time.1,values = values.1,bin.vec = bin.vec,bin.fun=bin.fun,max.ens=ceiling(sqrt(max.ens)))
  year = dum$time
  bin1 = dum$matrix
  bin2 = binEns(time = time.2,values = values.2,bin.vec = bin.vec,bin.fun=bin.fun,max.ens= ceiling(sqrt(max.ens)))$matrix
  
  #remove columns that have less than min.obs datapoints
  good = which(apply(!is.na(bin1),2,sum)>=min.obs)
  if(length(good)==0){
    stop(paste("none of the columns have",min.obs,"or more datapoints"))
  }
  bin1 = as.matrix(bin1[,good])
  
  
  good = which(apply(!is.na(bin2),2,sum)>=min.obs)
  if(length(good)==0){
    stop(paste("none of the columns have",min.obs,"or more datapoints"))
  }
  bin2 = as.matrix(bin2[,good])
  
  
  # apply mapping to standard Gaussian [optional]
  if(gaussianize==TRUE){
    bin1 = gaussianize(bin1)
    bin2 = gaussianize(bin2)
  }
  
  #calculate the correlations
  #cormat=c(cor(bin1,bin2,use = "pairwise"))  #faster - but no significance...
  
  cor.df = corMatrix(bin1,bin2,max.ens = max.ens,...)
  
  #calculate the FDR adjusted values
  for(co in 2:ncol(cor.df)){
    cn <- names(cor.df)[co]
    ncn <- paste0(cn,"FDR")
    fdrOut <- fdr(cor.df[,co],qlevel=fdr.qlevel,method="original",adjustment.method='mean')
    sig_fdr = matrix(0,nrow(cor.df))
    sig_fdr[fdrOut] = 1
    cor.df[ncn] <- sig_fdr
  }
  
  #calculate some default statistics
  if(!all(is.na(percentiles))){
    stats <- cor.df %>% 
      dplyr::select(-ends_with("FDR")) %>% 
      purrr::map_dfc(quantile,probs = percentiles,na.rm = T)
    
    fdrStats <-  cor.df %>% 
      dplyr::select(ends_with("FDR")) %>%
      purrr::map_dfr(mean,na.rm = T)
    
    cor.stats <- dplyr::bind_cols(percentiles = percentiles,stats)
    #row.names(cor.stats)=format(cor.stats$percentiles,digits = 2) # it appears that the rows are already well formatted
    corEns.data <- list(cor.df = cor.df,cor.stats = cor.stats,cor.fdr.stats = fdrStats)
    
  }else{
    cor.stats=NA
    corEns.data=list(cor.df = cor.df)
  }
  
  return(corEns.data)
  
}


#' @export
#' @title Bin ensemble data
#' @description takes ensembles in time and/or values and creates a matrix of data for future analysis
#' @param time single column vector of time
#' @param values single column vector of values to bin
#' @param bin.vec vector of bin edges for binning step
#' @param bin.fun function to use during binning (mean, sd, and sum all work)
#' @param max.ens maximum number of ensemble members to regress
#' @return list that includes matrix of binned data and binned time
#' @family bin

binEns = function(time,values,bin.vec,bin.fun=mean,max.ens=NA){
  
  time = as.matrix(time)
  values = as.matrix(values)
  
  #if it's an age ensemble only
  if(ncol(time)>1 & ncol(values)==1){
    if(!is.na(max.ens)){
      if(max.ens<ncol(time)){
        time=time[,1:max.ens]
      }
    }
    binMat = apply(time,MARGIN = 2,function(x) bin(time = x,values = values,bin.vec = bin.vec,bin.fun = bin.fun)$y)
    
    #if it's a value ensemble only
  }else if(ncol(time)==1 & ncol(values)>1){
    if(!is.na(max.ens)){
      if(max.ens<ncol(values)){
        values=values[,1:max.ens]
      }
    }
    binMat = apply(values,MARGIN = 2,function(x) bin(time = time,values = x,bin.vec = bin.vec,bin.fun = bin.fun)$y)
    
    #if it's a value AND age ensemble
  }else if(ncol(time)>1 & ncol(values)>1){
    nx = ncol(time)
    ny = ncol(values)
    if(!is.na(max.ens)){
      n.ens=min(max(nx,ny),max.ens)
    }else{
      n.ens = max(nx,ny)
    }
    if(nx>=ny){
      binMat = apply(time[,1:n.ens],MARGIN = 2,function(x) bin(time = x,values = values[,sample.int(ny,size=1)],bin.vec = bin.vec,bin.fun = bin.fun)$y)
    }else{
      binMat = apply(values[,1:n.ens],MARGIN = 2,function(x) bin(time = time[,sample.int(nx,size=1)],values = x,bin.vec = bin.vec,bin.fun = bin.fun)$y)
    }
    
    #both are single values
  }else{
    #just regular bin
    binMat = bin(time = time,values = values,bin.vec = bin.vec,bin.fun = bin.fun)$y
  }
  
  binMat = as.matrix(binMat)
  bin_x = apply(cbind(bin.vec[-1],bin.vec[-length(bin.vec)]),1,mean)
  binned=list("time"=bin_x,"matrix" = binMat)
  return(binned)
  
}

#' @export
#' @title Bin Data
#' @description function that puts data into appropriate bins, based on the time and the binning vector the bin vector describes the edges of the bins
#' @param time vector of time
#' @param values vector of values to bin
#' @param bin.vec vector of bin edges for describing where to bin
#' @param bin.fun function to use during binning (mean, sd, and sum all work)
#' @author Nick McKay
#' @family bin
#' @return A data.frame of (x) binned time, and (y) binned values
bin = function(time,values,bin.vec,bin.fun = mean){
  #function that puts data into appropriate bins, based on the time and the binning vector
  #the bin vector describes the edges of the bins
  #bin.fun is the function to use for the binning, mean, sum, sd are all reasonable options
  
  
  bin_y = rep(NA,times = length(bin.vec)-1)
  bin_x = apply(cbind(bin.vec[-1],bin.vec[-length(bin.vec)]),1,mean)
  
  for(i in 1:length(bin_y)){
    be <- sort(bin.vec[i:(i+1)])
    q = which(time > be[1] & time <= be[2])
    bin_y[i] = bin.fun(values[q],na.rm=TRUE)
  }
  
  binned = data.frame(x=bin_x,y=bin_y)
  return(binned)
}


#' @export
#' @family bin
#' @title Bin every entry in a Timeseries object
#' @description Aggregate data from a timeseries object into the same timeline through binning. 
#'
#' @param TS LiPD timeseries object See \url{http://nickmckay.github.io/LiPD-utilities/r/index.html#what-is-a-time-series}
#' @param bin.vec vector of bin edges for describing where to bin
#' @param bin.fun function to use during binning (mean, sd, and sum all work)
#' @param max.ens Maximum number of ensemble members.
#' @param time.var specify the time variable to bin (default = "ageEnsemble")
#' @param na.col.rm Remove columns that are all NAs? (TRUE or FALSE)
#'
#' @author Nick McKay
#' @return A list of binned years and values.
binTs = function(TS,
                 time.var="ageEnsemble",
                 bin.vec,
                 bin.fun = mean,
                 max.ens=1000,
                 na.col.rm=TRUE){
  
  #check to see if TS is a tibble
  if(tibble::is_tibble(TS)){#convert back to TS
    TS <- lipdR::untidyTs(TS)
  }
  
  
  #check for time.var
  
  
  
  
  timeList = lapply(TS,"[[",time.var)
  valueList = lapply(TS,"[[","paleoData_values")
  
  
  intl <- sapply(timeList,is.null)
  if(any(intl)){
    dataSetNames <- pullTsVariable(TS,variable = "dataSetName")
    stop(glue::glue("{time.var} is missing from {paste(dataSetNames[intl],collapse = ', ')}"))
  }
  
  binMat = vector(mode="list",length = length(timeList))
  pb <- txtProgressBar(min=1,max=length(timeList),style=3)
  
  for(i in 1:length(timeList)){
    binMat[[i]]=binEns(time = timeList[[i]],
                       values = valueList[[i]],
                       bin.vec = bin.vec,max.ens = max.ens,bin.fun = bin.fun)
    if(na.col.rm){
      allNa=which(apply(is.na(binMat[[i]]$matrix),2,all) | 
                    apply(is.nan(binMat[[i]]$matrix),2,all) | 
                    apply(binMat[[i]]$matrix=="nan",2,all))
      if(length(allNa)>0){
        binMat[[i]]$matrix = binMat[[i]]$matrix[,-allNa]
      }
    }
    if(ncol(binMat[[i]]$matrix) == 0){
      warning(paste("index",i,"has no values within binvec"))
    }
    
    
    
    setTxtProgressBar(pb,i)
  }
  close(pb)
  return(binMat)  
  
}

