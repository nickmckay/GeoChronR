run.BAM.LiPD = function(L,which.paleo=NA,which.pmt=NA,which.chron=1,which.model=NA,makeNew=NA,nens = 1000){
  
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
  
  #Which age/year vector do you want to perturb?
  yearData = select.data(L,varName = "year",altNames = "age", which.data = which.paleo, which.mt=which.pmt,always.choose = TRUE)
  
  
  
  #does this LiPD have a chronData?
  if(is.null(L$chronData)){
    L$chronData=list()
  }
  
  C=L$chronData[[which.chron]]
  
  #initialize model
  if(is.na(which.model)){
    if(is.null(L$chronData[[which.chron]]$chronModel[[1]])){
      #no models, this is first
      which.model=1
    }else{
      print(paste("You already have", length(L$chronData[[which.chron]]$chronModel), "chron model(s) in chronData" ,which.chron))
      which.model=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  
  if(is.na(makeNew)){
    makeNew = FALSE
  }
  
  if(length(L$chronData[[which.chron]]$chronModel)<which.model){
    if(makeNew){
      L$chronData[[which.chron]]$chronModel[[which.model]]=NA
    }else{
      nm=readline(prompt = paste("model",which.model,"doesn't exist. Create it? y or n "))
      if(grepl(pattern = "y",x = tolower(nm))){
        L$chronData[[which.chron]]$chronModel[[which.model]]=NA
      }else{
        stop("Stopping, since you didn't want to create a new model")
      }
    }
  }
  
  
  
  CM=L$chronData[[which.chron]]$chronModel[[which.model]]
  #get BAM parameters
  if(is.na(CM)){
    CM=list()
  }
if(is.null(CM$methods)){
  CM$methods=list()
}
    CM$methods$algorithm = "BAM"
  
  #specify model type
  if(is.null( CM$methods$parameters$modelType)){
    print("Which type of model do you want to use for BAM?")
    print("1 - Poisson (default)")
    print("2 - Bernoulli")
    mi = as.integer(readline(prompt = "Pick a number: "))
    if(mi!=2){
      CM$methods$parameters$modelType="poisson"
    }else{
      CM$methods$parameters$modelType="bernoulli"
    }
  }
  
  #specify undercounting rate
  if(is.null( CM$methods$parameters$undercountingProbability)){
    print("What's the probability of undercounting")
    CM$methods$parameters$undercountingProbability = as.numeric(readline(prompt = "Enter a number between 0 and 1: "))
  }
  #specify overcounting rate
  if(is.null( CM$methods$parameters$overcountingProbability)){
    print("What's the probability of overcounting")
    CM$methods$parameters$overcountingProbability = as.numeric(readline(prompt = "Enter a number between 0 and 1: "))
  }
  
  #ensemble members
  if(is.null( CM$methods$parameters$nEns)){
    if(!is.na(nens)){
      CM$methods$parameters$nEns=nens
    }else{
      CM$methods$parameters$nEns = as.integer(readline(prompt = "How many ensemble members?"))
    }
  }
  
  #this shouldn't change I think
  CM$methods$parameters$resize = 0
  
  #create model
  model <- list(name= CM$methods$parameters$modelType,param=c(CM$methods$parameters$undercountingProbability,CM$methods$parameters$overcountingProbability)
                ,ns=CM$methods$parameters$nEns,resize=CM$methods$parameters$resize)	
    
    
  
  #run BAM
  bamOut=BAM_simul(as.matrix(yearData$values),as.matrix(yearData$values),ageEnsOut=TRUE,model = model)
  
  #store output appropriately in chronModel
  CM$ensembleTable = list()
  CM$ensembleTable$ageEnsemble$values = bamOut$ageEns
  CM$ensembleTable$ageEnsemble$units = yearData$units
  CM$ensembleTable$timeCorrectionMatrix$values = bamOut$tmc
  CM$ensembleTable$timeCorrectionMatrix$units = NA
  CM$ensembleTable$timeCorrectionMatrix$description = "corresponding ensemble of time-correction matrices (tn*p*ns) to map realizations in Xp back to the original data X (2=insert nan, 0=remove double band)"
  
  #place into paleoData appropriately.
  #assign into paleoMeasurementTable
  L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]]$ageEnsemble$values = bamOut$ageEns
  L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]]$ageEnsemble$units = yearData$units
  L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]]$ageEnsemble$fromChronData = which.chron
  L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]]$ageEnsemble$fromChronModel = which.model
  L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]]$ageEnsemble$description = paste("age ensemble pulled from chronData", which.chron,"model",which.model,"- fit to paleoData depth with linear interpolation")
  
  return(L)
  
  
}
  
  
  