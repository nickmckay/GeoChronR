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
  if(any(is.na(paleo.or.chron.num))){
    if(length(L[[paleo.or.chron]])==1){
      paleo.or.chron.num=1
    }else{
      paleo.or.chron.num=as.integer(readline(prompt = paste0("Which ",paleo.or.chron," do you want to get data for?")))
    }
  }
  
  #initialize model number
  if(any(is.na(model.num))){
    if(is.null(L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[1]])){
      stop("There aren't any models in this object")
    }else if(length(L[[paleo.or.chron]][[paleo.or.chron.num]]$model) == 1){
      model.num <- 1
    }else{
      print(paste("You already have", length(L[[paleo.or.chron]][[paleo.or.chron.num]]$model), "model(s) in",paleo.or.chron,paleo.or.chron.num))
      model.num=as.integer(readline(prompt = "Enter the number for this model you want to use"))
    }
  }
  
  #initialize ensemble table number
  if(any(is.na(ens.table.num))){
    if(length(L[[paleo.or.chron]][[paleo.or.chron.num]]$model$ensembleTable) == 1){
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
  if(!any(is.na(depth.var))){
  depthVals <- selectData(L,
                          var.name = depth.var,
                          paleo.or.chron = paleo.or.chron,
                          paleo.or.chron.num = paleo.or.chron.num,
                          table.type = "ens",
                          ens.table.num = ens.table.num,
                          model.num = model.num)
  }else{
    depthVals <- NULL
  }
  
  
  summaryTable = list()
  for(n in 1:length(prob.vars)){
    summaryTable[[prob.vars[n]]]$variableName = prob.vars[n]
    summaryTable[[prob.vars[n]]]$values = quants[,n]
    summaryTable[[prob.vars[n]]]$units = ensVals$units
    summaryTable[[prob.vars[n]]]$TSid = createTSid()
    
  }
  
  if(!is.null(depthVals)){
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
#' @return a model object
#' @export
createModel <- function(L,
                        depth.or.age.vector = NA,
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
  
  if(!all(is.na(methods))){#if methods are supplied, add them in
    L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[model.num]]$methods=methods
  }
  
  
  if(all(is.na(depth.or.age.vector))){
    noPositionVector <- TRUE
  }else{
    noPositionVector <- FALSE
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
  
  if(!noPositionVector){
    colEns <- list(values = as.matrix(depth.or.age.vector),
                   variableName = depth.or.age.var,
                   units = depth.or.age.units,
                   TSid = createTSid())
    et <- list(colEns,valEns)
    names(et) <- c(colEns$variableName,valEns$variableName)

  }else{
    et <- list(valEns)
    names(et) <- c(valEns$variableName)
    
  }

  
  #assign in ensembleTable object object
  L[[paleo.or.chron]][[paleo.or.chron.num]]$model[[model.num]]$ensembleTable[[ens.table.num]] <-  et
  
  #create a summary table from the ensemble data?
  if(create.summary.table){
    if(noPositionVector){
      L <- createSummaryTableFromEnsembleTable(L,
                                               paleo.or.chron = paleo.or.chron,
                                               paleo.or.chron.num = paleo.or.chron.num,
                                               model.num = model.num,
                                               ens.table.num = ens.table.num,
                                               ens.var = valEns$variableName,
                                               depth.var = NA)
    }else{
    L <- createSummaryTableFromEnsembleTable(L,
                                             paleo.or.chron = paleo.or.chron,
                                             paleo.or.chron.num = paleo.or.chron.num,
                                             model.num = model.num,
                                             ens.table.num = ens.table.num,
                                             ens.var = valEns$variableName,
                                             depth.var = colEns$variableName)
    }
  }
  
  print(glue::glue("Created a model in {paleo.or.chron}-{paleo.or.chron.num}, model-{model.num}, ensembleTable {ens.table.num} with variables: {paste(names(et),collapse = ', ')}"))
  #to do- create distribution tables?
  if(create.summary.table){
    cat("\n")
    print(glue::glue("Also created a summary table in {paleo.or.chron}-{paleo.or.chron.num}, model-{model.num}"))
  }
  return(L) 
}


#' Create a multi-model ensemble
#'
#' @param L A lipd object containing the ensembles you want to combine
#' @param depth.var What is the variable name of the ensemble depth vector? Must be the same for all models. 
#' @param age.var What is the variable name of the chronEnsemble? Must be the same for all models. 
#' @param models.to.combine 2 or more integers that correspond to the chronData model objects you want to combine into an ensemble
#' @param chron.num an integer that corresponds to number of the chronData object (L$chron[[?]])
#' @param depth.interval The depth spacing that you want the new ensemble to be interpolated to.
#' @param depth.seq Optionally specify a depth sequence overwhich to create the new ensemble
#' @param ens.table.number What ensemble table number should we pull the data from (default = 1)
#' @param n.ens How many ensembles should be included in the final model? The amount from each contributing model will be equal (or very nearly so if it's not evenly divisible)
#' @family LiPD manipulation
#'
#' @return A lipd object with a new chron model that contains your multimodel ensemble. 
#' @export 
createMultiModelEnsemble <- function(L,
                                     depth.var = "depth",
                                     age.var = "ageEnsemble",
                                     models.to.combine,
                                     chron.num = 1, 
                                     depth.interval =10 ,
                                     depth.seq = NA,
                                     ens.table.number = 1,
                                     n.ens = 1000){
  
  if(any(is.na(depth.seq))){
    #figure out the depth sequence
    depthrange <- function(mod,depth.var,ens.table.number){
      return(mod$ensembleTable[[ens.table.number]][[depth.var]]$values %>% 
               range())
    }
    
    ranges <- suppressMessages(purrr::map_dfc(L$chronData[[chron.num]]$model[models.to.combine],depthrange,depth.var,ens.table.number) )
    
    ds <- max(ranges[1,],na.rm = TRUE)
    de <- min(ranges[2,],na.rm = TRUE)
    
    depth.seq <- seq(ds,de,by = depth.interval)
  }
  
  ##interpolate ensemble tables
  interpolateEnsembles <- function(mod,
                                   depth.seq,
                                   depth.var,
                                   age.var,
                                   ens.table.number,
                                   n.ens.out){
    
    od <- mod$ensembleTable[[ens.table.number]][[depth.var]]$values
    oa <- mod$ensembleTable[[ens.table.number]][[age.var]]$values %>% purrr::array_branch(margin = 2)
    ia <- purrr::map_dfc(oa,~ approx(od,.x,depth.seq)$y)
    neo <- nrow(ia)
    if(neo >= n.ens.out){
      replace <- FALSE
    }else{
      replace <- TRUE
    }
    return(ia[ , sample.int(neo,size = n.ens.out,replace = replace)])
  }
  
  interpolatedEnsembles <- suppressMessages(purrr::map_dfc(L$chronData[[chron.num]]$model[models.to.combine],
                                                           interpolateEnsembles,
                                                           depth.seq,
                                                           depth.var,
                                                           age.var,
                                                           ens.table.number,
                                                           ceiling(n.ens/length(models.to.combine)))) %>% 
    as.matrix()
  
  to.output <- interpolatedEnsembles[,sample.int(ncol(interpolatedEnsembles),n.ens,replace = FALSE)]
  
  #make some methods.
  
  allMethods <- purrr::map(L$chronData[[chron.num]]$model[models.to.combine],purrr::pluck,"methods")
  methods <- list(algorithm = "grand model ensemble by geoChronR::createMultiModelEnsemble()", originalMethods = allMethods)
  
  L <- createModel(L,
                   depth.or.age.vector = depth.seq,
                   ensemble.data = to.output,
                   model.num = length(L$chronData[[chron.num]]$model) +1,
                   paleo.or.chron = "chronData",
                   paleo.or.chron.num = chron.num,
                   depth.or.age.var = depth.var,
                   depth.or.age.units = L$chronData[[chron.num]]$model[[models.to.combine[1]]]$ensembleTable[[ens.table.number]][[depth.var]]$units,
                   ens.var = age.var,
                   L$chronData[[chron.num]]$model[[models.to.combine[1]]]$ensembleTable[[ens.table.number]][[age.var]]$units,
                   make.new = TRUE,
                   create.summary.table = TRUE,
                   methods = methods)
  
  
  
  return(L)
  
  
  
}


