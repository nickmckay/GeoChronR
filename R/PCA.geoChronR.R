#' @export
#' @family pca
#' @title Perform principle components analysis (PCA) across an ensemble
#' @description Ensemble PCA, or Monte Carlo Empirical Orthogonal Functions as described in Anchukaitis and Tierney 2012.
#' @param bin.list A list of binned data, the output of binTs()
#' @param method What method to use for PCA? pcaMethods::listPcaMethods() for options. "ppca" is default. Other options may not work in GeoChronR.
#' @param weights Vector of weights to apply to timeseries in the bin.list
#' @param PCAtype Correlation ("corr" - default) or Covariance ("cov"), matrix
#' @param nPCs number of PCs/EOFs to calculate
#' @param nens how many ensemble members?

pcaEns <-  function(bin.list,method='ppca',weights=NA,PCAtype="corr",nPCs=4,nEns=1000){
 
  #the option type controls whether the analysis is done on a correlation (default) or covariance matrix ("cov")
#the function uses the pcaMethods library to do the heavy lifting
library("matrixStats")
library("pcaMethods")
  time = bin.list[[1]]$time

nD = length(bin.list) #how many sites?
nEnsSite = sapply(bin.list,function(x) ncol(x$matrix)) #how many ensembles at each site


#dat.mat[[1]]$record.density <- rowSums(!is.na(dat.mat[[1]]$matrix))/NCOL(dat.mat[[1]]$matrix)
#dat.mat <- tmat2

#setup output
loads = array(data = NA,dim = c(nD,nPCs,nEns))
PCs = array(data = NA,dim = c(length(time),nPCs,nEns))
varExp = array(data = NA,dim = c(nPCs,nEns))
dataDensity = matrix(data = NA, nrow = length(time),ncol=nEns)
pb=txtProgressBar(min=1,max=nEns,style = 3)
for(n in 1:nEns){#for each ensemble member
  #build a matrix from the list
  PCAMAT=matrix(NA,nrow=length(time),ncol = nD)
  for(j in 1:nD){
    PCAMAT[,j] = bin.list[[j]]$matrix[,sample.int(nEnsSite[j],size=1)]
  }
  PCAMAT[is.nan(PCAMAT)]=NA
  
  dataDensity[,n]=apply(!is.na(PCAMAT),1,sum)/nD
  
  if(any(!is.na(weights))){
    normmat=scale(PCAMAT,na.rm=T)
    wmat=scale(normmat,scale=1/weights,na.rm=T)
    PCAMAT=wmat
  }
  
  #remove any rows that are all NAs
  goodRows = which(!apply(is.na(PCAMAT),1,all))
  PCAMAT = PCAMAT[goodRows,]
  #remove means, and scale if correlation matrix
  if(PCAtype=="corr"){
    pca.out=pca(PCAMAT,method,center=TRUE,scale="vector",nPcs=nPCs)
  }else if(PCAtype=="cov"){
    pca.out=pca(PCAMAT,method,center=TRUE,scale="none",nPcs=nPCs)
  }
  
  loads[,,n]=loadings(pca.out)
  PCs[goodRows,,n]=scores(pca.out)
  
  
  #reorient PCs such that the mean loadings are positive
  meanloadings <- colMeans(loads[,,n],na.rm = T)
  for (npc in 1:NCOL(PCs)){
    if(meanloadings[npc] < 0){
      loads[,npc,n]=-1*loads[,npc,n]
      PCs[,npc,n]=-1*PCs[,npc,n]
    }
    }
  
  
  
  #variance explained  
  cumvariance=R2cum(pca.out)
  variance=cumvariance
  for (v in 2:length(variance)){
    variance[v]=cumvariance[v]-cumvariance[v-1]
  }
  varExp[,n]=variance
  setTxtProgressBar(pb,n)
}
close(pb)

#reorient any PCs that have a negative correlation with PC mean
for (p in 1:nPCs){
  meanPC=apply(PCs[,p,],c(1),median)
  ct = cor(meanPC,PCs[,p,])
  toflip = which(ct<0)
  if(length(toflip)>0){
    PCs[,p,toflip]= -PCs[,p,toflip]
    loads[,p,toflip] = -loads[,p,toflip]  
  }
}

if(any(!is.na(weights))){
  ens.PC.out <- list(loads/weights,PCs,varExp,time,apply(dataDensity,1,mean))
  ens.PC.out$weightedPCmat=PCAMAT
}else{
  ens.PC.out <- list(loads,PCs,varExp,time,apply(dataDensity,1,mean))
}
names(ens.PC.out) <- c("loadings","PCs","variance","age","meanDataDensity")
return(ens.PC.out)

}




