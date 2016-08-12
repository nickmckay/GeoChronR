ageEnsemble.to.paleoData = function(L,which.paleo=NA,which.pmt=NA,which.chron=NA,which.model=NA,max.ensemble.members=NA){
  #check on the chronModel first
  if(is.null(L$chronData)){
    stop("There's no chronData in this file")
  }
  
  #initialize which.chron
  if(is.na(which.chron)){
    if(length(L$chronData)==1){
      which.chron=1
    }else{
      which.chron=as.integer(readline(prompt = "Which chronData do you want to pull this ensemble from? "))
    }
  }
  
  #initialize model number
  if(length(L$chronData[[which.chron]]$chronModel)==0){
    stop("No model in this chronData")
  }
  if(is.na(which.model)){
    if(length(L$chronData[[which.chron]]$chronModel)==1){
      #only one model
      which.model=1
    }else{
      print(paste("ChronData", which.chron, "has", length(L$chronData[[which.chron]]$chronModel), "models"))
      which.model=as.integer(readline(prompt = "Which chron model do you want to get the ensemble from? Enter an integer "))
    }
  }
  
  
  #initialize which.paleo
  if(is.na(which.paleo)){
    if(length(L$paleoData)==1){
      which.paleo=1
    }else{
      which.paleo=as.integer(readline(prompt = "Which paleoData do you want to put this age ensemble in? "))
    }
  }
  
  #initialize measurement table number
  if(is.na(which.pmt)){
    if(length(L$paleoData[[which.paleo]]$paleoMeasurementTable)==1){
      #only one pmt
      which.pmt=1
    }else{
      print(paste("PaleoData", which.paleo, "has", length(L$paleoData[[which.paleo]]$paleoMeasurementTable), "measurement tables"))
      which.pmt=as.integer(readline(prompt = "Which measurement table do you want to put the ensemble in? Enter an integer "))
    }
  }
  
  #make sure the ensemble is there, with data
  print("Looking for age ensemble....")
aei=getVariableIndex(L$chronData[[which.chron]]$chronModel[[which.model]]$ensembleTable,varName = "ageEnsemble",altNames = c("age","year"))
    if(is.na(aei)){
    stop("There doesn't seem to be in values in the ensemble table for this model (there may not be an ensembleTable, or even a model)")
  }
  #grab the ensemble
  ens=L$chronData[[which.chron]]$chronModel[[which.model]]$ensembleTable[[aei]]$values
  ensDepth = L$chronData[[which.chron]]$chronModel[[which.model]]$ensembleTable$depth$values
  #get the depth from the paleo measurement table
  print("getting depth from the paleodata table...")
  di = getVariableIndex(L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]],"depth",altNames = "position",always.choose = FALSE)

  #depthTarget
  depth = L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]][[di]]$values
  
  #interpolate
  library(Hmisc)
  aei=apply(X=ens,MARGIN = 2,FUN = function(y) approxExtrap(ensDepth,y,xout=depth,na.rm=TRUE)$y)
  
  if(!is.na(max.ensemble.members)){
    if(ncol(aei)>max.ensemble.members){
      #randomly select the appropriate number of ensemble members
      aei = aei[,sample.int(ncol(aei),size = max.ensemble.members)]
    }
  }
  
  
  #assign into paleoMeasurementTable
  L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]]$ageEnsemble$values = aei
  L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]]$ageEnsemble$units = L$chronData[[which.chron]]$chronModel[[which.model]]$ensembleTable$ageEnsemble$units
  L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]]$ageEnsemble$fromChronData = which.chron
  L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]]$ageEnsemble$fromChronModel = which.model
  L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]]$ageEnsemble$description = paste("age ensemble pulled from chronData", which.chron,"model",which.model,"- fit to paleoData depth with linear interpolation")
  
  
  return(L)
  
  
}
  
  