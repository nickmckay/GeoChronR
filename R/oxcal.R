
#' write oxcal expression for each date.
#'
#' @param ... 
#' @param defaultOutlierProb 
#'
#' @return
#' @export
oxCalDateExpression <- function(...,
                                defaultOutlierProb = 0.05,
                                calCurve = "intcal13",
                                unknownDeltaR = FALSE
){
  
  
  #create Curve expression
  if(tolower(calCurve) == "intcal13" | grepl(calCurve,pattern = "atmo")){
    curveExp <- 'Curve("Atmospheric","IntCal13.14c");\n'
    cal <- "atmo"
  }else if(grepl(calCurve,pattern = "marine")){
    curveExp <- 'Curve("Oceanic","Marine13.14c");\n'
    cal <- "marine"
  }else if(grepl(calCurve,pattern = "south")){
    curveExp <- 'Curve("SH","SHCal.14c");\n'
    cal <- "SH"
  }else{
    stop(paste0("unrecognized calibration curve: ",calCurve))
  } 
  
  
  adf <- tibble::tibble(...)
  

if("outlierProb" %in% names(adf)){
  if(is.na(adf$outlierProb)){#assign default
    adf$outlierProb <- defaultOutlierProb
  }
}else{
  adf$outlierProb <- defaultOutlierProb
}

#figure out date type
if(!is.na(adf$age14C)){#radiocarbon
  #check labID
  if(is.na(adf$labID)){#throw an error - this is required
    stop("no lab ID, all dated layers must have a unique lab ID")
  }
  if(is.na(adf$age14CUnc)){
    stop("seems like a 14C date, but no uncertainty included (age14CUnc)")
  }  
  
  #check for reservoir corrections
  drExp <- NA #initialize
  if(cal == "marine"){#only do this for marine curves
    if(!is.na(adf$reservoirAge)){
      if(is.na(adf$reservoirAgeUnc)){
        adf$reservoirAgeUnc <- adf$reservoirAge/2
        print("no reservoirAgeUnc found...\n using half of the reservoirAge value")
      }
      drExp <- paste0('Delta_R("Local Marine",',adf$reservoirAge,',',adf$reservoirAgeUnc,');\n')
    }else if(unknownDeltaR){#use a uniform distribution here instead
      drExp <- 'Delta_R("uniform",U(0,800))\n'
    }
  }
  
  #create expression
  dateLine <- paste0('R_Date("',adf$labID,'",',adf$age14C,',',adf$age14CUnc,'){ z=',adf$depth,'; Outlier(',adf$outlierProb,'); };\n')
  
  #build full expresion
  if(!is.na(drExp)){
    dateExp <- paste0(curveExp,drExp,dateLine)
  }else{
    dateExp <- paste0(curveExp,dateLine)
  }
  
}else if(!is.na(adf$age)){#normally distributed cal date
  if(is.na(adf$ageUnc)){
    stop("seems like a calibrated date, but no uncertainty included (ageUnc)")
  } 
  
  if(is.na(adf$labID)){#throw an error - this is required
    stop("no lab ID, all dated layers must have a unique lab ID")
  }
  
  
  
  #create expression
  dateExp <- paste0('C_Date("',adf$labID,'",',convertBP2AD(adf$age),',',adf$ageUnc,'){ z=',adf$depth,'; Outlier(',adf$outlierProb,'); };\n')
  
}else{#undated layer
  dateExp <- paste0('Date("D"){ z=',adf$depth,'; };\n')
  
}

return(dateExp)

}


#' Create the oxcal model script
#'
#' @param cdf 
#' @param depthsToModel 
#' @param depthInterval 
#' @param thin 
#' @param nIt 
#' @param eventsPerUnitLength 
#' @param eventsPerUnitLengthUncertainty In orders of magnitude 
#' @param seqType 
#' @param topBoundary 
#' @param bottomBoundary 
#' @import dplyr tidyr
#' @importFrom magrittr %>% 
#' @importFrom purrr pmap_chr
#' @return
#' @export
createOxcalModel <- function(cdf,
                             depthsToModel = NA,
                             depthInterval = 10,
                             thin = 50,
                             nIt = 10000,
                             eventsPerUnitLength = 1,
                             eventsPerUnitLengthUncertainty = 2,
                             seqType = "P_Sequence",
                             topBoundary = "Boundary",
                             bottomBoundary = "Boundary",
                             outlierProb = 0.05,
                             calCurve = "intcal13"){
  
  
  
  
  
  if(is.na(depthsToModel)){
    depthsToModel <- data.frame(depth = seq(0,max(cdf$depth),by = depthInterval))
  }
  
  if(!is.data.frame(depthsToModel)){
    depthsToModel <- data.frame(depth = depthsToModel)
    
  }
  
  depthsToModel <- dplyr::filter(depthsToModel,!depth %in% cdf$depth)
  
  #create oxcal code
  #pull in relevant data
  #fold in depths to date
  
  age2m <- cdf %>%
    bind_rows(depthsToModel) %>%
    arrange(desc(depth))
  
  #create the guts
  modGuts <- purrr::pmap_chr(age2m,
                             oxCalDateExpression,outlierProb = outlierProb,
                             calCurve = calCurve)
  
  
  if(is.na(eventsPerUnitLengthUncertainty) | eventsPerUnitLengthUncertainty <= 0){#no uncertainty on K
    eventsPerUnitLengthExp <- eventsPerUnitLength
  }else{
    eventsPerUnitLengthExp <- paste0('"variable",',
                                     eventsPerUnitLength,
                                     ',',
                                     eventsPerUnitLengthUncertainty,
                                     ',U(-',
                                     eventsPerUnitLengthUncertainty,
                                     ',',
                                     eventsPerUnitLengthUncertainty,
                                     ')')
  }
  
  
  #model start text
  modStart <- paste0(seqType,"(",eventsPerUnitLengthExp,")\n",
                     "{\n",
                     topBoundary,"();\n"
  )
  
  #model end text
  modEnd <- paste0(bottomBoundary,"();\n",
                   "MCMC_Sample(",thin,",",nIt,");\n",
                   "};")
  
  modText <- paste(c(modStart,modGuts,modEnd),collapse = "")
  
  #write out parameters
  parameters <- list( thin = thin,
                      nIt = nIt,
                      eventsPerUnitLength = eventsPerUnitLength,
                      seqType = seqType,
                      topBoundary = topBoundary,
                      bottomBoundary = bottomBoundary)
  
  return(list(modelText = modText,
              parameters= parameters,
              inputData = age2m))
  
}



#' Load oxcal output
#'
#' @param L 
#' @param oxcalResultFilePath 
#' @param modelParameters 
#' @param depthUnits 
#' @param which.chron 
#' @param modelNum 
#' @param makeNew 
#' @param maxEns 
#'
#' @return
#' @export
loadOxcalOutput <- function(L,
                            oxcalResultFilePath,
                            modelParameters,
                            depthUnits = "cm",
                            which.chron=NA,
                            modelNum=NA,
                            makeNew=NA,
                            maxEns = 1000){
  
  
  #see if the path has MCMC results
  if(!file.exists(dirname(oxcalResultFilePath),"MCMC_Sample.csv")){
    stop("It doesn't look like the model ran properly. Typically this is because of parameter choices. If all else seems right, consider increasing the depthInterval, or increasing eventsPerUnitLength")
  }
  
  
  # setup storage -----------------------------------------------------------
  
  
  #setup storage in LiPD file
  
  #initialize which.chron
  if(is.na(which.chron)){
    if(length(L$chronData)==1){
      which.chron=1
    }else{
      which.chron=as.integer(readline(prompt = "Which chronData do you want to grab bacon data for? "))
    }
  }
  
  
  
  #initialize model number
  if(is.na(modelNum)){
    if(is.null(L$chronData[[which.chron]]$model[[1]])){
      #no models, this is first
      modelNum=1
    }else{
      print(paste("You already have", length(L$chronData[[which.chron]]$model), "chron model(s) in chronData" ,which.chron))
      print(paste("If you want to create a new model, enter", length(L$chronData[[which.chron]]$model)+1))
      modelNum=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  if(is.na(makeNew)){
    makeNew = FALSE
  }
  
  if(length(L$chronData[[which.chron]]$model)<modelNum){
    if(makeNew){
      L$chronData[[which.chron]]$model[[modelNum]]=NA
    }else{
      nm=readline(prompt = paste("model",modelNum,"doesn't exist. Create it? y or n "))
      if(grepl(pattern = "y",x = tolower(nm))){
        L$chronData[[which.chron]]$model[[modelNum]]=NA
      }else{
        stop("Stopping, since you didn't want to create a new model")
      }
    }
  }
  
  
  
  # load in methods ---------------------------------------------------------
  
  oxText <- oxcAAR::readOxcalOutput(oxcalResultFilePath)
  
  methods = list("parameters"= modelParameters$parameters)
  methods$algorithm = "oxcal"
  
  #get version
  vers <- try(stringr::str_split(oxText[5],pattern = "OxCal v")[[1]][2])
  if(class(vers) == "character"){
    methods$version <- vers
  }
  
  
  if(is.na(L$chronData[[which.chron]]$model[[modelNum]])){
    L$chronData[[which.chron]]$model[[modelNum]]=list("methods"=methods)
    
  }else{
    L$chronData[[which.chron]]$model[[modelNum]]$methods=methods
  }
  
  # get probability distributions -------------------------------------------
  
  oxData <- oxcAAR::parseOxcalOutput(oxText)
  L$chronData[[which.chron]]$model[[modelNum]]$distributionTable <- vector(mode = "list",length = length(oxData))
  for(dd in 1:length(oxData)){
    dTable = list()
    dTable$age = list(values = convertAD2BP(oxData[[dd]]$raw_probabilities$dates),
                      units =  "BP", 
                      variableName = "age")
    dTable$probabilityDensity = list(values = oxData[[dd]]$raw_probabilities$probabilities , 
                                     variableName = "probabilityDensity")
    dTable$labId <- oxData[[dd]]$name
    dTable$depth  <- modelParameters$inputData$depth[which(modelParameters$inputData$labID == oxData[[dd]]$name)]
    dTable$depthUnits = depthUnits
    L$chronData[[which.chron]]$model[[modelNum]]$distributionTable[[dd]] = dTable
  }
  
  
  # get ensemble data -------------------------------------------------------
  
  MCMCfile <- file.path(dirname(oxcalResultFilePath),"MCMC_Sample.csv")
  
  oxEns <- read.csv(MCMCfile)
  
  
  ensembleTable=list()
  ensembleTable$depth$values <- modelParameters$inputData$depth
  ensembleTable$depth$variableName  <-  "depth"
  ensembleTable$depth$units <- depthUnits
  
  names(oxEns)
  goodColumns <- seq(3,ncol(oxEns)-2)
  
  if(length(goodColumns) != length( ensembleTable$depth$values)){
    stop("depths and age ensemble levels don't match!")
  }
  
  #only take the end of the posterior
  goodRows <- seq(nrow(oxEns)-maxEns+1,nrow(oxEns))
  goodRows <- goodRows[goodRows >0]
  
  ensembleTable$ageEnsemble$values <-  convertAD2BP(t(as.matrix(oxEns[goodRows,goodColumns])))
  ensembleTable$ageEnsemble$variableName = "ageEnsemble"
  ensembleTable$ageEnsemble$units <- "BP"
  
  L$chronData[[which.chron]]$model[[modelNum]]$ensembleTable[[1]] <-  ensembleTable
  
  
  
  
  return(L)
  
}



#' @export
#' @author Nick McKay
#' @family oxcal
#' @import oxcAAR
#' @importFrom crayon bold yellow cyan red green blue 
#' @title Generate an oxcal Age Model and add it into a LiPD object
#' @description This is a high-level function that uses oxcal to simulate an age model, and stores this as an age-ensemble in a model in chronData. If needed input variables are not entered, and cannot be deduced, it will run in interactive mode. See Bronk Ramsey et al. 2008 doi:10.1016/j.quascirev.2007.01.019
#' @inheritParams writeBacon
#' @param maxEns the maximum number of ensembles to load in (default = 1000)
#' @param staticReservoirAge optionally assign a deltaR to all radiocarbon dates
#' @param staticReservoirAgeUnc optionally assign a deltaR uncertainty to all radiocarbon dates
#' @param ... arguments to pass to createChronMeasInputDf
#' @return L The single LiPD object that was entered, with methods, ensembleTable, summaryTable and distributionTable added to the chronData model.
#' @examples 
#' Run in interactive mode:
#' L = runOxcal(L)
#' 
#' Run in noninteractive mode, describing everything:
#' L = runOxcal(L,which.chron = 1, which.mt = 1, modelNum = 3, baconDir = "~/Bacon/",site.name = "MSB2K", cc = 1)
runOxcal <-  function(L,
                      which.chron=NA,
                      which.mt = NA,
                      oxcalPath="~/OxCal/",
                      modelNum=NA,
                      remove.rejected=TRUE,
                      overwrite=TRUE,
                      maxEns = 1000,
                      staticReservoirAge = NA,
                      staticReservoirAgeUnc = NA,
                      surfaceAge = NA,
                      surfaceAgeUnc = NA,
                      surfaceAgeDepth = 0,
                      depthInterval = NA,
                      eventsPerUnitLength = NA,
                      eventsPerUnitLengthUncertainty = 0,
                      outlierProb = .05,
                      calCurve = "IntCal13",
                      
                      
                      ...){
  
  
  #get oxcal directory
  oxcAAR::quickSetupOxcal(path = "oxcalPath")
  
  #initialize which.chron
  if(is.na(which.chron)){
    if(length(L$chronData)==1){
      which.chron=1
    }else{
      which.chron=as.integer(readline(prompt = "Which chronData do you want to run oxcal for? "))
    }
  }
  
  
  #initialize model number
  if(is.na(modelNum)){
    if(is.null(L$chronData[[which.chron]]$model[[1]])){
      #no models, this is first
      modelNum=1
    }else{
      print(paste("You already have", length(L$chronData[[which.chron]]$model), "chron model(s) in chronData" ,which.chron))
      print(paste("If you want to create a new model, enter", length(L$chronData[[which.chron]]$model)+1))
      
      modelNum=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  
  #create the chron data.frame
  cdf <- createChronMeasInputDf(L,...)
  
  #assign in reservoir ages?
  if(!is.na(staticReservoirAge)){
    cdf$reservoirAge <- staticReservoirAge
  }
  if(!is.na(staticReservoirAgeUnc)){
    cdf$reservoirAgeUnc <- staticReservoirAgeUnc
  }
  
  #add surface age estimate?
  if(!is.na(surfaceAge)){
    
    if(is.na(surfaceAgeUnc)){
      stop("you cannot add a surface age without an uncertainty")
    }
    
    sage <- data.frame(labID = "surface",
                       age = surfaceAge,
                       ageUnc = surfaceAgeUnc,
                       depth = surfaceAgeDepth)
    cdf <- dplyr::bind_rows(sage,cdf)
  }
  
  
  #remove rejected
  which.rejected <- which(!is.na(cdf$rejected))
  if(remove.rejected & length(which.rejected)>1){
    cdf <- cdf[-which.rejected,]
  }
  
  #prepare for model input
  if(is.na(depthInterval)){
    #take a stab at it
    depthInterval <- abs(diff(range(cdf$depth)))/50
    cat(crayon::bold(paste("No depthInterval entered, trying",crayon::red(depthInterval),"\n Specify your own depthInterval if preferred\n")))
  }
  
  if(is.na(eventsPerUnitLength)){
    #take a stab at it
    eventsPerUnitLength <- 1/depthInterval
    cat(crayon::bold(paste("No eventsPerUnitLength entered, trying",crayon::red(eventsPerUnitLength),"\n Specify your own eventsPerUnitLength if preferred\n")))
  }
  
  cat(crayon::blue("Oxcal is now running, depending on your settings and your computer, this may take a few minutes to several hours. The model is complete when a table of model diagnostics appears.\n"))
  
  #create the model input
  oxMod <- createOxcalModel(cdf,
                            depthInterval = depthInterval,
                            eventsPerUnitLength = eventsPerUnitLength,
                            eventsPerUnitLengthUncertainty = eventsPerUnitLengthUncertainty,
                            outlierProb = outlierProb,
                            calCurve = calCurve)
  
  
 #run the file!
  oxcalResultFilePath <- oxcAAR::executeOxcalScript(oxMod$modelText)
  
  
  L <- loadOxcalOutput(L,
                       oxcalResultFilePath,
                       oxMod,
                       which.chron = which.chron,
                       maxEns = maxEns,
                       modelNum = modelNum,
                       makeNew = overwrite)
  return(L)
}

