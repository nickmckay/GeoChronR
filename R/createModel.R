#' Create a summary table from an ensemble table
#' @inheritParams selectData
#' @param ens.var The name of the ensemble variable (default = "ageEnsemble")
#' @param probs A vector of probabilites (quantiles) should be computed for the summary? (default = c(0.025,0.25,.5,.75,.975))
#' @param prob.vars A vector of variableNames for the probabilites in probs (default =  c("unc2.5","unc25","ageMedian","unc75","unc97.5"))
#' @param depth.var The name of the depth (or position) variable in the ensemble table (default = "depth")
#' @family LiPD manipulation
#' @return A LiPD object with an added ensemble table
#' @export
createSummaryTableFromEnsembleTable <- function(L,
                                                paleo.or.chron = "chronData",
                                                paleo.or.chron.num = NA,
                                                model.num = NA,
                                                ens.table.num = NA,
                                                ens.var = "ageEnsemble",
                                                probs = c(0.025,0.25,.5,.75,.975),
                                                prob.vars = c("unc2.5","unc25","ageMedian","unc75","unc97.5"),
                                                depth.var = "depth"){
  
  
  #initialize chron.num
  if(is.na(paleo.or.chron.num)){
    if(length(L[[paleo.or.chron]])==1){
      paleo.or.chron.num=1
    }else{
      paleo.or.chron.num=as.integer(readline(prompt = paste0("Which ",paleo.or.chron," do you want to get data for?")))
    }
  }
  
  #initialize model number
  if(is.na(model.num)){
    if(is.null(L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[1]])){
      #no models, this is first
      model.num=1
    }else{
      print(paste("You already have", length(L[[paleo.or.chron]][[paleo.or.chron.num]]$model), "model(s) in",paleo.or.chron,paleo.or.chron.num))
      model.num=as.integer(readline(prompt = "Enter the number for this model you want to use"))
    }
  }
  
  #initialize ensemble table number
  if(is.na(ens.table.num)){
    if(length(model$ensembleTable) == 1){
      #no models, this is first
      ens.table.num=1
    }else{
      print(paste("You have", length(L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[model.num]]$ensembleTable), "ensemble table(s) in model" ,model.num))
      ens.table.num=as.integer(readline(prompt = "Enter the number for this ensembleTable you want to create the summaryTable from"))
    }
  }
  
  #pull the ensemble data
  ensVals <- selectData(L,
                        var.name = ens.var,
                        paleo.or.chron = paleo.or.chron,
                        paleo.or.chron.num = paleo.or.chron.num,
                        table.type = "ens",
                        ens.table.num = ens.table.num,
                        model.num = model.num)
  
  #calculate the quantiles
  quants <- t(apply(ensVals$values,1,quantile,probs = probs,na.rm = TRUE))
  
  
  #pull the depth data
  depthVals <- selectData(L,
                          var.name = depth.var,
                          paleo.or.chron = paleo.or.chron,
                          paleo.or.chron.num = paleo.or.chron.num,
                          table.type = "ens",
                          ens.table.num = ens.table.num,
                          model.num = model.num)
  
  
  
  summaryTable = list()
  for(n in 1:length(prob.vars)){
    summaryTable[[prob.vars[n]]]$variableName = prob.vars[n]
    summaryTable[[prob.vars[n]]]$values = quants[,n]
    summaryTable[[prob.vars[n]]]$units = ensVals$units
    summaryTable[[prob.vars[n]]]$TSid = createTSid()
    
  }
  
  
  if(is.null(depthVals)){
    con <- askUser("It looks like there are no independent variable (typically depth) values. Should we continue without them?")
    if(!grepl(con,pattern = "y",ignore.case = )){stop("you chose to stop")}  
  }else{
    summaryTable[[depthVals$variableName]] <- depthVals
  }
  
  L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[model.num]]$summaryTable[[1]] <- summaryTable
  
  return(L)
}





#' Create a chron or paleo model including adding ensemble data
#' @description This function will create a model in a LiPD object, adding ensemble data
#' @inheritParams selectData
#' @family LiPD manipulation
#' @param depth.or.age.vector A vector of data for the ensemble data that records the position
#' @param ensemble.data A matrix or data.frame of ensemble data, with ensemble members in separate columns
#' @param depth.or.age.var A string describing the name of the depth or age vector (default = "depth")
#' @param depth.or.age.units Units for depth.or.age.var (default = "cm")
#' @param ens.var A string describing the name of the ensemble data (default = "age")
#' @param ens.units Units for the ensemble data (default = "yr BP")
#' @param make.new Create a new model? (default = TRUE)
#' @param methods Optionally add in a list describing the methods used to make the model (default = NA)
#' @param create.summary.table Optionally create a summary table derived from the ensemble data (default = TRUE)
#'
#' @return
#' @export
createModel <- function(L,
                        depth.or.age.vector,
                        ensemble.data, 
                        paleo.or.chron = "chronData",
                        paleo.or.chron.num = NA,
                        model.num = NA,
                        ens.table.num = 1,
                        depth.or.age.var= "depth",
                        depth.or.age.units = "cm",
                        ens.var = "ageEnsemble",
                        ens.units = "yr BP",
                        make.new = TRUE,
                        methods = NA,
                        create.summary.table = TRUE){
  
  
  #initialize chron.num
  if(is.na(paleo.or.chron.num)){
    if(length(L[[paleo.or.chron]])==1){
      paleo.or.chron.num=1
    }else{
      paleo.or.chron.num=as.integer(readline(prompt = paste0("Which ",paleo.or.chron," do you want to get data for?")))
    }
  }
  
  #initialize if needed
  if(is.null(L[[paleo.or.chron]][[paleo.or.chron.num]])){
    L[[paleo.or.chron]][[paleo.or.chron.num]] <- list()
  }
  
  #initialize model number
  if(is.na(model.num)){
    if(is.null(L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[1]])){
      #no models, this is first
      model.num=1
    }else{
      print(paste("You already have", length(L[[paleo.or.chron]][[paleo.or.chron.num]]$model), "model(s) in",paleo.or.chron,paleo.or.chron.num))
      print(paste("If you want to create a new model, enter", length(L[[paleo.or.chron]][[paleo.or.chron.num]]$model)+1))
      model.num=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  #initialize if needed
  if(length(L[[paleo.or.chron]][[paleo.or.chron.num]]$model)<model.num){
    if(make.new){
      L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[model.num]] <- list()
    }else{
      nm=readline(prompt = paste("model",model.num,"doesn't exist. Create it? y or n "))
      if(grepl(pattern = "y",x = tolower(nm))){
        L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[model.num]] <- list()
      }else{
        stop("Stopping, since you didn't want to create a new model")
      }
    }
  }
  
  if(!is.na(methods)){#if methods are supplied, add them in
    L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[model.num]]$methods=methods
  }
  
  
  #initialize ensemble table number
  if(is.na(ens.table.num)){
    if(is.null(L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[model.num]]$ensembleTable[[1]])){
      #no models, this is first
      ens.table.num=1
    }else{
      print(paste("You already have", length(L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[model.num]]$ensembleTable), "ensemble table(s) in  model" ,model.num))
      print(paste("If you want to create a new ensembleTable, enter", length(L[[paleo.or.chron]][[paleo.or.chron.num]]$model)+1))
      ens.table.num=as.integer(readline(prompt = "Enter the number for this ensembleTable - will overwrite if necessary "))
    }
  }
  
  
  #now add in the ensemble data.
  valEns <- list(values = as.matrix(ensemble.data),
                 variableName = ens.var,
                 units = ens.units,
                 TSid = createTSid())
  
  colEns <- list(values = as.matrix(depth.or.age.vector),
                 variableName = depth.or.age.var,
                 units = depth.or.age.units,
                 TSid = createTSid())

  et <- list(colEns,valEns)
  names(et) <- c(colEns$variableName,valEns$variableName)
  
  #assign in ensembleTable object object
  L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[model.num]]$ensembleTable[[ens.table.num]] <-  et
  
  #create a summary table from the ensemble data?
  if(create.summary.table){
    L <- createSummaryTableFromEnsembleTable(L,
                                             paleo.or.chron = paleo.or.chron,
                                             paleo.or.chron.num = paleo.or.chron.num,
                                             model.num = model.num,
                                             ens.table.num = ens.table.num,
                                             ens.var = valEns$variableName,
                                             depth.var = colEns$variableName)
  }
  
  #to do- create distribution tables?
 
  return(L) 
}

