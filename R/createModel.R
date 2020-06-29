createSummaryTableFromEnsembleTable <- function(L,
                                                chronOrPaleo = "chronData",
                                                which.chronOrPaleo = NA,
                                                which.model = NA,
                                                which.ens.table = NA,
                                                ensVarName = "ageEnsemble",
                                                probs = c(0.025,0.25,.5,.75,.975),
                                                probVarNames = c("unc2.5","unc25","ageMedian","unc75","unc97.5"),
                                                depthVarName = "depth"){
  
  
  #initialize which.chron
  if(is.na(which.chronOrPaleo)){
    if(length(L[[chronOrPaleo]])==1){
      which.chronOrPaleo=1
    }else{
      which.chronOrPaleo=as.integer(readline(prompt = paste0("Which ",chronOrPaleo," do you want to get data for?")))
    }
  }
  
  #initialize model number
  if(is.na(which.model)){
    if(is.null(L[[chronOrPaleo]][[which.chronOrPaleo]]$model[[1]])){
      #no models, this is first
      which.model=1
    }else{
      print(paste("You already have", length(L[[chronOrPaleo]][[which.chronOrPaleo]]$model), "model(s) in",chronOrPaleo,which.chronOrPaleo))
      which.model=as.integer(readline(prompt = "Enter the number for this model you want to use"))
    }
  }
  
  #initialize ensemble table number
  if(is.na(which.ens.table)){
    if(length(model$ensembleTable) == 1){
      #no models, this is first
      which.ens.table=1
    }else{
      print(paste("You have", length(L[[chronOrPaleo]][[which.chronOrPaleo]]$model[[which.model]]$ensembleTable), "ensemble table(s) in model" ,which.model))
      which.ens.table=as.integer(readline(prompt = "Enter the number for this ensembleTable you want to create the summaryTable from"))
    }
  }
  
  #pull the ensemble data
  ensVals <- selectData(L,
                        varName = ensVarName,
                        where = chronOrPaleo,
                        which.data = which.chronOrPaleo,
                        tableType = "ens",
                        which.ens = which.ens.table,
                        model.num = which.model)
  
  #calculate the quantiles
  quants <- t(apply(ensVals$values,1,quantile,probs = probs))
  
  
  #pull the depth data
  depthVals <- selectData(L,
                          varName = depthVarName,
                          where = chronOrPaleo,
                          which.data = which.chronOrPaleo,
                          tableType = "ens",
                          which.ens = which.ens.table,
                          model.num = which.model)
  
  
  
  summaryTable = list()
  for(n in 1:length(probVarNames)){
    summaryTable[[probVarNames[n]]]$variableName = probVarNames[n]
    summaryTable[[probVarNames[n]]]$values = quants[,n]
    summaryTable[[probVarNames[n]]]$units = ensVals$units
    if(requireNamespace("lipdR",quietly = TRUE)){
      summaryTable[[probVarNames[n]]]$TSid = lipdR::createTSid()
    }
  }
  
  
  if(is.null(depthVals)){
    con <- askUser("It looks like there are no independent variable (typically depth) values. Should we continue without them?")
    if(!grepl(con,pattern = "y",ignore.case = )){stop("you chose to stop")}  
  }else{
    summaryTable[[depthVals$variableName]] <- depthVals
  }
  
  L[[chronOrPaleo]][[which.chronOrPaleo]]$model[[which.model]]$summaryTable[[1]] <- summaryTable
  
  return(L)
}





createModel <- function(L,
                        depthOrAgeVector,
                        ensembleData, 
                        chronOrPaleo = "chronData",
                        which.chronOrPaleo = NA,
                        which.model = NA,
                        which.ens.table = 1,
                        depthOrAgeName= "depth",
                        depthOrAgeUnits = "cm",
                        ensembleVariableName = "ageEnsemble",
                        ensembleUnits = "yr BP",
                        makeNew = TRUE,
                        methods = NA,
                        createSummaryTable = TRUE){
  
  
  #initialize which.chron
  if(is.na(which.chronOrPaleo)){
    if(length(L[[chronOrPaleo]])==1){
      which.chronOrPaleo=1
    }else{
      which.chronOrPaleo=as.integer(readline(prompt = paste0("Which ",chronOrPaleo," do you want to get data for?")))
    }
  }
  
  #initialize if needed
  if(is.null(L[[chronOrPaleo]][[which.chronOrPaleo]])){
    L[[chronOrPaleo]][[which.chronOrPaleo]] <- list()
  }
  
  #initialize model number
  if(is.na(which.model)){
    if(is.null(L[[chronOrPaleo]][[which.chronOrPaleo]]$model[[1]])){
      #no models, this is first
      which.model=1
    }else{
      print(paste("You already have", length(L[[chronOrPaleo]][[which.chronOrPaleo]]$model), "model(s) in",chronOrPaleo,which.chronOrPaleo))
      print(paste("If you want to create a new model, enter", length(L[[chronOrPaleo]][[which.chronOrPaleo]]$model)+1))
      which.model=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  #initialize if needed
  if(length(L[[chronOrPaleo]][[which.chronOrPaleo]]$model)<which.model){
    if(makeNew){
      L[[chronOrPaleo]][[which.chronOrPaleo]]$model[[which.model]] <- list()
    }else{
      nm=readline(prompt = paste("model",which.model,"doesn't exist. Create it? y or n "))
      if(grepl(pattern = "y",x = tolower(nm))){
        L[[chronOrPaleo]][[which.chronOrPaleo]]$model[[which.model]] <- list()
      }else{
        stop("Stopping, since you didn't want to create a new model")
      }
    }
  }
  
  if(!is.na(methods)){#if methods are supplied, add them in
    L[[chronOrPaleo]][[which.chronOrPaleo]]$model[[which.model]]$methods=methods
  }
  
  
  #initialize ensemble table number
  if(is.na(which.ens.table)){
    if(is.null(L[[chronOrPaleo]][[which.chronOrPaleo]]$model[[which.model]]$ensembleTable[[1]])){
      #no models, this is first
      which.ens.table=1
    }else{
      print(paste("You already have", length(L[[chronOrPaleo]][[which.chronOrPaleo]]$model[[which.model]]$ensembleTable), "ensemble table(s) in  model" ,which.model))
      print(paste("If you want to create a new ensembleTable, enter", length(L[[chronOrPaleo]][[which.chronOrPaleo]]$model)+1))
      which.ens.table=as.integer(readline(prompt = "Enter the number for this ensembleTable - will overwrite if necessary "))
    }
  }
  
  
  #now add in the ensemble data.
  valEns <- list(values = as.matrix(ensembleData),
                 variableName = ensembleVariableName,
                 units = ensembleUnits)
  
  if(requireNamespace("lipdR",quietly = TRUE)){
    valEns$TSid <- lipdR::createTSid()
  }
  
  colEns <- list(values = as.matrix(depthOrAgeVector),
                 variableName = depthOrAgeName,
                 units = depthOrAgeUnits)
  
  if(requireNamespace("lipdR",quietly = TRUE)){
    colEns$TSid <- lipdR::createTSid()
  }
  
  et <- list(colEns,valEns)
  names(et) <- c(colEns$variableName,valEns$variableName)
  
  #assign in ensembleTable object object
  L[[chronOrPaleo]][[which.chronOrPaleo]]$model[[which.model]]$ensembleTable[[which.ens.table]] <-  et
  
  #create a summary table from the ensemble data?
  if(createSummaryTable){
    L <- createSummaryTableFromEnsembleTable(L,
                                             chronOrPaleo = chronOrPaleo,
                                             which.chronOrPaleo = which.chronOrPaleo,
                                             which.model = which.model,
                                             which.ens.table = which.ens.table,
                                             ensVarName = valEns$variableName,
                                             depthVarName = colEns$variableName)
  }
  
  #to do- create distribution tables?
 
  return(L) 
}

