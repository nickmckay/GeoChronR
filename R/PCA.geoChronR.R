#' @export
#' @family pca
#' @title Perform principle components analysis (PCA) across an ensemble
#' @description Ensemble PCA, or Monte Carlo Empirical Orthogonal Functions
#' @param bin.list A list of binned data, the output of binTs()
#' @param method What method to use for PCA? pcaMethods::listPcaMethods() for options. "ppca" is default. Other options may not work in GeoChronR.
#' @param weights Vector of weights to apply to timeseries in the bin.list
#' @param pca.type Correlation ("corr" - default) or Covariance ("cov"), matrix
#' @param gaussianize Map input data to a standard Gaussian distribution? This is only relevant for correlation matrices, covariance matrices will not be gaussianized. (default = TRUE)
#' @param n.ens how many ensemble members to calculate
#' @param simulateTrendInNull Should the null include the trend?
#' @param n.pcs number of PCs/EOFs to calculate
#' @section Long-form example:
#' \href{http://nickmckay.github.io/GeoChronR/articles/PCA.html}{View a full-fledged example of how to use this function.}

pcaEns <-  function(bin.list,
                    method='ppca',
                    weights=NA,
                    pca.type="corr",
                    gaussianize = TRUE,
                    n.pcs=8,
                    n.ens=1000,
                    simulateTrendInNull = FALSE){
  
  #this may not be needed anymore now that we're using biocview in the description
  #check for the pcaMethods package
  if (!requireNamespace("pcaMethods", quietly = TRUE)){
    
      stop("You'll need the pcaMethods package to run pcaEns(), which is a bioconductor package. You can install it with these commands:\n\n     install.packages('BiocManager'); BiocManager::install('pcaMethods')")
  }
  
  #the option type controls whether the analysis is done on a correlation (default) or covariance matrix ("cov")
  #the function uses the pcaMethods library to do the heavy lifting
  time = bin.list[[1]]$time
  
  nD = length(bin.list) #how many sites?
  n.ensSite = sapply(bin.list,function(x) ncol(x$matrix)) #how many ensembles at each site
  
  
  #dat.mat[[1]]$record.density <- rowSums(!is.na(dat.mat[[1]]$matrix))/NCOL(dat.mat[[1]]$matrix)
  #dat.mat <- tmat2
  
  #setup output
  nullloads <- loads <-  array(data = NA,dim = c(nD,n.pcs,n.ens))
  nullPCs <- PCs <-  array(data = NA,dim = c(length(time),n.pcs,n.ens))
  nullvarExp <- varExp <- array(data = NA,dim = c(n.pcs,n.ens))
  dataDensity = matrix(data = NA, nrow = length(time),ncol=n.ens)
  pb=txtProgressBar(min=1,max=n.ens,style = 3)
  failed <- c()
  for(n in 1:n.ens){#for each ensemble member
    #build a matrix from the list
    NULLMAT <- PCAMAT <- matrix(NA,nrow=length(time),ncol = nD)
    for(j in 1:nD){
      rn <- sample.int(n.ensSite[j],size=1)
      PCAMAT[,j] = bin.list[[j]]$matrix[,rn]
      synSeries <- try(ar1Surrogates(time,bin.list[[j]]$matrix[,rn],n.ens = 1,detrend = !simulateTrendInNull),silent = TRUE)
      if(any(class(synSeries) == "try-error")){
        NULLMAT[,j] <- sample(PCAMAT[,j])
      }else{
        NULLMAT[,j] <- synSeries
      }
      if(all(!is.finite(NULLMAT[,j]))){
        NULLMAT[,j] <- sample(PCAMAT[,j])
      }
    }
    
    PCAMAT[is.nan(PCAMAT)]=NA
    NULLMAT[is.nan(NULLMAT)]=NA
    
    dataDensity[,n]=apply(!is.na(PCAMAT),1,sum)/nD
    
    if(any(!is.na(weights))){
      normmat=scale(PCAMAT)
      wmat=scale(normmat,scale=1/weights)
      PCAMAT=wmat
      
      #repeat for NULL
      normmat=scale(NULLMAT)
      wmat=scale(normmat,scale=1/weights)
      NULLMAT=wmat
    }
    
    #remove any rows that are all NAs
    goodRowsP = which(!apply(is.na(PCAMAT),1,all))
    goodRowsN = which(!apply(is.na(NULLMAT),1,all))
    goodRows <- intersect(goodRowsP,goodRowsN)
    
    PCAMAT = PCAMAT[goodRows,]
    NULLMAT = NULLMAT[goodRows,]
    

    
#check for columns of all NAs
    allNaPca = apply(is.na(PCAMAT),2,all)
    allNaNull = apply(is.na(NULLMAT),2,all)
 
    if(any(allNaPca) | any(allNaNull)){
      failed <- c(failed,n)
      next
    }

    #remove means, and scale if correlation matrix
    if(pca.type=="corr"){
      if(gaussianize){
        PCAMAT <- gaussianize(PCAMAT)
        NULLMAT <- gaussianize(NULLMAT)
      }
      pca.out=pcaMethods::pca(PCAMAT,method,center=TRUE,scale="vector",nPcs=n.pcs)
      null.out=pcaMethods::pca(NULLMAT,method,center=TRUE,scale="vector",nPcs=n.pcs)
      
    }else if(pca.type=="cov"){
      pca.out=pcaMethods::pca(PCAMAT,method,center=TRUE,scale="none",nPcs=n.pcs)
      null.out=pcaMethods::pca(NULLMAT,method,center=TRUE,scale="none",nPcs=n.pcs)
      
    }else{
      stop(glue::glue("pca.type not recognized. You entered {pca.type}, please choose 'corr' or 'cov'"))
    }
    
    loads[,,n]=pcaMethods::loadings(pca.out)
    nullloads[,,n]=pcaMethods::loadings(null.out)
    
    PCs[goodRows,,n]=pcaMethods::scores(pca.out)
    nullPCs[goodRows,,n]=pcaMethods::scores(null.out)
    
    
    #reorient PCs such that the mean loadings are positive
    meanloadings <- colMeans(loads[,,n],na.rm = T)
    for (npc in 1:NCOL(PCs)){
      if(meanloadings[npc] < 0){
        loads[,npc,n]=-1*loads[,npc,n]
        PCs[,npc,n]=-1*PCs[,npc,n]
      }
    }
    
    nullmeanloadings <- colMeans(nullloads[,,n],na.rm = T)
    for (npc in 1:NCOL(nullPCs)){
      if(nullmeanloadings[npc] < 0){
        nullloads[,npc,n]=-1*nullloads[,npc,n]
        nullPCs[,npc,n]=-1*nullPCs[,npc,n]
      }
    }
    
    #variance explained  
    cumvariance=pcaMethods::R2cum(pca.out)
    
    variance=cumvariance
    for (v in 2:length(variance)){
      variance[v]=cumvariance[v]-cumvariance[v-1]
    }
    varExp[,n]=variance
    
    #repeat for null
    nullcumvariance=pcaMethods::R2cum(null.out)
    
    nullvariance=nullcumvariance
    for (v in 2:length(nullvariance)){
      nullvariance[v]=nullcumvariance[v]-nullcumvariance[v-1]
    }
    nullvarExp[,n]=nullvariance
    
    setTxtProgressBar(pb,n)
  }
  close(pb)
  
  #remove results from failed iterations
  if(length(failed) > 0){
    if(length(failed)/n.ens > 0.9){
      stop("More than 90% of the ensemble members didn't have a full matrix for PCA. Check your settings and rerun")
    }
    if(length(failed)/n.ens > 0.5){
      warning("More than 50% of the ensemble members didn't have a full matrix for PCA. You may want to check your settings and rerun")
    }
    loads <- loads[,,-failed]
    nullloads <- nullloads[,,-failed]
    PCs <- PCs[,,-failed]
    nullPCs <- nullPCs[,,-failed]
    varExp <- varExp[,-failed]
    nullvarExp <- nullvarExp[,-failed]
  }
  
  
  
  #reorient any PCs that have a negative correlation with PC mean
  for (p in 1:n.pcs){
    meanPC=apply(PCs[,p,],c(1),median)
    ct = cor(meanPC,PCs[,p,])
    toflip = which(ct<0)
    if(length(toflip)>0){
      PCs[,p,toflip]= -PCs[,p,toflip]
      loads[,p,toflip] = -loads[,p,toflip]  
    }
  }
  
  if(any(!is.na(weights))){
    ens.pc.out <- list(loads/weights,PCs,varExp,time,apply(dataDensity,1,mean),nullloads/weights,nullPCs,nullvarExp)
    ens.pc.out$weightedPCmat=PCAMAT
    
  }else{
    ens.pc.out <- list(loads,PCs,varExp,time,apply(dataDensity,1,mean),nullloads/weights,nullPCs,nullvarExp)
    
    
  }
  



names(ens.pc.out) <- c("loadings","PCs","variance","age","meanDataDensity","nullLoadings","nullPCs","nullVariance")

return(ens.pc.out)

}




