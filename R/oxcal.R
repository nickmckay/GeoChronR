
#' write oxcal expression for each date.
#' @family oxcal
#' @param ... data to pass into a tibble
#' @param default.outlier.prob Outlier Probability
#' @param cal.curve Calibration curve
#' @param unknown.delta.r Use an unknown delta R (T/f)
#'
#' @return an oxcal date expression
#' @export
oxCalDateExpression <- function(...,
                                default.outlier.prob = 0.05,
                                cal.curve = "intcal20",
                                unknown.delta.r = FALSE){
  
  
  #create Curve expression
  if(grepl(cal.curve,pattern = "intcal20",ignore.case = TRUE) | grepl(cal.curve,pattern = "atmo",ignore.case = TRUE) ){
    curveExp <- 'Curve("Atmospheric","IntCal20.14c");\n'
    cal <- "intcal20"
  }else if(grepl(cal.curve,pattern = "marine20",ignore.case = TRUE)){
    curveExp <- 'Curve("Oceanic","Marine20.14c");\n'
    cal <- "marine20"
  }else if(grepl(cal.curve,pattern = "shcal20",ignore.case = TRUE) | grepl(cal.curve,pattern = "sh20",ignore.case = TRUE)){
    curveExp <- 'Curve("SH","shcal20.14c");\n'
    cal <- "SH20"
  }else if(tolower(cal.curve) == "intcal13"){
    curveExp <- 'Curve("Atmospheric","IntCal13.14c");\n'
    cal <- "intcal13"
  }else if(grepl(cal.curve,pattern = "marine13",ignore.case = TRUE)){
    curveExp <- 'Curve("Oceanic","Marine13.14c");\n'
    cal <- "marine13"
  }else if(grepl(cal.curve,pattern = "shcal13",ignore.case = TRUE) | grepl(cal.curve,pattern = "sh13")){
    curveExp <- 'Curve("SH","shcal13.14c");\n'
    cal <- "SH13"
  }else{
    stop(paste0("unrecognized calibration curve: ",cal.curve))
  } 
  
  
  adf <- tibble::tibble(...)
  
  
  if("outlier.prob" %in% names(adf)){
    if(any(is.na(adf$outlier.prob))){#assign default
      adf$outlier.prob <- default.outlier.prob
    }
  }else{
    adf$outlier.prob <- default.outlier.prob
  }
  
  #figure out date type
  if(!any(is.na(adf$age14C))){#radiocarbon
    #check labID
    if(any(is.na(adf$labID))){#throw an error - this is required
      stop("no lab ID, all dated layers must have a unique lab ID")
    }
    if(any(is.na(adf$age14CUnc))){
      stop("seems like a 14C date, but no uncertainty included (age14CUnc)")
    }  
    
    #check for reservoir corrections
    drExp <- NA #initialize
    if(grepl(cal,pattern = "marine",ignore.case = TRUE)){#only do this for marine curves
      if(!any(is.na(adf$reservoirAge))){
        if(any(is.na(adf$reservoirAgeUnc))){
          adf$reservoirAgeUnc <- adf$reservoirAge/2
          print("no reservoirAgeUnc found...\n using half of the reservoirAge value")
        }
        drExp <- paste0('Delta_R("Local Marine",',adf$reservoirAge,',',adf$reservoirAgeUnc,');\n')
      }else if(unknown.delta.r){#use a uniform distribution here instead
        drExp <- 'Delta_R("uniform",U(0,800))\n'
      }
    }
    
    #create expression
    dateLine <- paste0('R_Date("',adf$labID,'",',adf$age14C,',',adf$age14CUnc,'){ z=',adf$depth,'; Outlier(',adf$outlier.prob,'); };\n')
    
    #build full expresion
    if(!any(is.na(drExp))){
      dateExp <- paste0(curveExp,drExp,dateLine)
    }else{
      dateExp <- paste0(curveExp,dateLine)
    }
    
  }else if(!any(is.na(adf$age))){#normally distributed cal date
    if(any(is.na(adf$ageUnc))){
      stop("seems like a calibrated date, but no uncertainty included (ageUnc)")
    } 
    
    if(any(is.na(adf$labID))){#throw an error - this is required
      stop("no lab ID, all dated layers must have a unique lab ID")
    }
    
    
    
    #create expression
    dateExp <- paste0('C_Date("',adf$labID,'",',convertBP2AD(adf$age),',',adf$ageUnc,'){ z=',adf$depth,'; Outlier(',adf$outlier.prob,'); };\n')
    
  }else{#undated layer
    dateExp <- paste0('Date("D"){ z=',adf$depth,'; };\n')
    
  }
  
  return(dateExp)
  
}


#' Create the oxcal model script
#'
#' @param cdf A data.frame of chron measurement data (output from createChronMeasInputDf())
#' @param depths.to.model A vector of depths to model (default = create a sequence using depth.interval)
#' @param depth.interval Depth interval to use to create depths.to.model (default = 10)
#' @param thin Thinning parameter for OxCal (default = 50)
#' @param n.it Number of iteration in model (default = 10000)
#' @param events.per.unit.length How many events per unit length for the Poisson model? This controls the flexibility on the model, the lower the number the more flexibile. 
#' @param events.per.unit.length.uncertainty Uncertainty on events.per.unit.length in orders of magnitude. This makes for better models but *greatly* increases the run time. If NA or < 0 it runs without uncertainty (default = NA)
#' @param seq.type What type of Oxcal sequence to use? (default = "P_Sequence")
#' @param top.boundary What type of Oxcal boundary to use for the top (default = "Boundary")
#' @param bottom.boundary What type of Oxcal boundary to use for the bottom (default = "Boundary")
#' @param outlier.prob What's the probability of a date being an outlier if not otherwise specified (default= 0.05)
#' @param cal.curve What calibration curve to use? Current options are "intcal20" (default), "marine20", "shcal20", "intcal13", "marine13", or "shcal13"
#' @import dplyr tidyr
#' @importFrom magrittr %>% 
#' @importFrom purrr pmap_chr
#' @family oxcal
#' @return a list of an oxcal model
#' @export
createOxcalModel <- function(cdf,
                             depths.to.model = NA,
                             depth.interval = 10,
                             thin = 50,
                             n.it = 10000,
                             events.per.unit.length = 1,
                             events.per.unit.length.uncertainty = NA,
                             seq.type = "P_Sequence",
                             top.boundary = "Boundary",
                             bottom.boundary = "Boundary",
                             outlier.prob = 0.05,
                             cal.curve = "intcal20"){
  
  
  
  
  
  if(any(is.na(depths.to.model))){
    depths.to.model <- data.frame(depth = seq(min(cdf$depth),max(cdf$depth),by = depth.interval))
  }
  
  if(!is.data.frame(depths.to.model)){
    depths.to.model <- data.frame(depth = depths.to.model)
    
  }
  
  depths.to.model <- dplyr::filter(depths.to.model,!depth %in% cdf$depth)
  
  #create oxcal code
  #pull in relevant data
  #fold in depths to date
  
  age2m <- cdf %>%
    bind_rows(depths.to.model) %>%
    arrange(desc(depth))
  
  #create the guts
  modGuts <- purrr::pmap_chr(age2m,
                             oxCalDateExpression,outlier.prob = outlier.prob,
                             cal.curve = cal.curve)
  
  
  if(any(is.na(events.per.unit.length.uncertainty)) | 
     events.per.unit.length.uncertainty <= 0){#no uncertainty on K
    events.per.unit.lengthExp <- events.per.unit.length
  }else{
    events.per.unit.lengthExp <- paste0('"variable",',
                                        events.per.unit.length,
                                        ',',
                                        events.per.unit.length.uncertainty,
                                        ',U(-',
                                        events.per.unit.length.uncertainty,
                                        ',',
                                        events.per.unit.length.uncertainty,
                                        ')')
  }
  
  
  #model start text
  modStart <- paste0(seq.type,"(",events.per.unit.lengthExp,")\n",
                     "{\n",
                     top.boundary,"();\n"
  )
  
  #model end text
  modEnd <- paste0(bottom.boundary,"();\n",
                   "MCMC_Sample(",thin,",",n.it,");\n",
                   "};")
  
  modText <- paste(c(modStart,modGuts,modEnd),collapse = "")
  
  #write out parameters
  parameters <- list( thin = thin,
                      n.it = n.it,
                      events.per.unit.length = events.per.unit.length,
                      seq.type = seq.type,
                      top.boundary = top.boundary,
                      bottom.boundary = bottom.boundary)
  
  return(list(modelText = modText,
              parameters= parameters,
              inputData = age2m))
  
}


#' Pull model parameters from OxCal Code
#'
#' @param modText The model text
#'
#' @return The list of model parameters
#' @export
getModelParametersFromOxcalText <- function(modText){
  
  #get thin and n.it
  #remove lines that start with //
    modText <- gsub("//.*?\\\n", "", modText)
  
  matches <- stringr::str_match(modText, "MCMC_Sample\\((\\d+),(\\d+)\\);")
  
  if(is.na(matches[1])){
    stop("To extract an ensemble from OxCal you must include 'MCMC_Sample(...)' in the model text")
  }
  
  thin <- as.numeric(matches[2])
  n.it <- as.numeric(matches[3])
  
  events.per.unit.length <- as.numeric(stringr::str_match(modText,"_Sequence\\((\\d+\\.?\\d*)\\)")[2])
  
  
  #sequence type:
  seq.type <- stringr::str_match(modText,"^([^\\(]+)\\(")[2]
  
  #Boundarys:
  
  pattern_start <- "^([^\\n;]+Boundary\\(\\));"
  pattern_end <- "\\n([^\\n;]+Boundary\\(\\))\\n\\};"
  
  bounds <- stringr::str_extract_all(modText, "\\w*Boundary\\(\\)")[[1]]
  
  top.boundary <-  bounds[1]
  bottom.boundary <- bounds[length(bounds)]
  
  
  parameters <- list( thin = thin,
                      n.it = n.it,
                      events.per.unit.length = events.per.unit.length,
                      seq.type = seq.type,
                      top.boundary = top.boundary,
                      bottom.boundary = bottom.boundary)
  
  #now get all the depths:
  
  age2m <- stringr::str_extract_all(modText, "(?<=z=)\\d+\\.?\\d*") |> 
     unlist() |> 
    as.numeric() |> 
    as.data.frame() |> 
    setNames("depth")
  
  # now get all the lab IDs
  labIds <- stringr::str_extract_all(modText, '(?<=Date\\(|Prior\\(|Boundary\\(")(.+?)(?=\\))') |> 
    unlist() |> 
    stringr::str_remove_all('"') |> 
    stringr::str_remove_all('\\\\')
    
  
  if(length(labIds) == nrow(age2m)){#it seems right!
    bad <- which(!stringr::str_detect(labIds,",")) #these are not dates
    
    if(length(bad) > 0){
      labIds[bad] <- NA
    }
    labIds <- stringr::str_split(labIds,",") |> 
      purrr::map_chr(purrr::pluck,1)
    age2m$labID <- labIds
  }else{
    stop("Couldn't extract lab IDs properly")
  }

  return(list(modelText = modText,
              parameters= parameters,
              inputData = age2m))
  
}


#' Load oxcal output
#' @inheritParams selectData
#' @param chron.num Which chronData object to select?
#' @param oxcal.result.file.path Path to oxcal output data (from oxcAAR::executeOxcalScript())
#' @param model.parameters model parameters to write into the lipd file
#' @param depth.units Depth units to assign in (default = "cm")
#' @param make.new Create a new model?
#' @param max.ens Maximum number of ensemble members to import (default = 1000)
#' @family oxcal
#' @importFrom stringr str_split
#' @return A lipd object
#' @export 
loadOxcalOutput <- function(L,
                            oxcal.result.file.path,
                            model.parameters,
                            depth.units = "cm",
                            chron.num=NA,
                            model.num=NA,
                            make.new=NA,
                            max.ens = 1000){
  
  
  #see if the path has MCMC results
  if(!file.exists(file.path(dirname(oxcal.result.file.path),"MCMC_Sample.csv"))){
    stop("It doesn't look like the model ran properly. Typically this is because of parameter choices. If all else seems right, consider increasing the depth.interval, or increasing events.per.unit.length")
  }
  
  
  #see if model.parameters is a list (exported from createOxcalModel) or is oxCal model code itself
  if(!is.list(model.parameters)){
    model.parameters <- getModelParametersFromOxcalText(model.parameters)
  }
    
    
  
  
  # setup storage -----------------------------------------------------------
  
  
  #setup storage in LiPD file
  
  #initialize chron.num
  if(any(is.na(chron.num))){
    if(length(L$chronData)==1){
      chron.num=1
    }else{
      chron.num=as.integer(readline(prompt = "Which chronData do you want to grab bacon data for? "))
    }
  }
  
  
  
  #initialize model number
  if(any(is.na(model.num))){
    if(is.null(L$chronData[[chron.num]]$model[[1]])){
      #no models, this is first
      model.num=1
    }else{
      print(paste("You already have", length(L$chronData[[chron.num]]$model), "chron model(s) in chronData" ,chron.num))
      print(paste("If you want to create a new model, enter", length(L$chronData[[chron.num]]$model)+1))
      model.num=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  if(any(is.na(make.new))){
    make.new = FALSE
  }
  
  if(length(L$chronData[[chron.num]]$model)<model.num){
    if(make.new){
      L$chronData[[chron.num]]$model[[model.num]]=list()
    }else{
      nm=readline(prompt = paste("model",model.num,"doesn't exist. Create it? y or n "))
      if(grepl(pattern = "y",x = tolower(nm))){
        L$chronData[[chron.num]]$model[[model.num]]=list()
      }else{
        stop("Stopping, since you didn't want to create a new model")
      }
    }
  }
  
  
  
  # load in methods ---------------------------------------------------------
  
  oxText <- oxcAAR::readOxcalOutput(oxcal.result.file.path)
  
  methods = list("parameters"= model.parameters$parameters)
  methods$algorithm = "oxcal"
  
  #get version
  vers <- try(stringr::str_split(oxText[5],pattern = "OxCal v")[[1]][2])
  if(class(vers) == "character"){
    methods$version <- vers
  }
  
  
  if(is.null(L$chronData[[chron.num]]$model[[model.num]]$methods)){
    L$chronData[[chron.num]]$model[[model.num]]=list("methods"=methods)
    
  }else{
    L$chronData[[chron.num]]$model[[model.num]]$methods=methods
  }
  
  # get probability distributions -------------------------------------------
  
  oxData <- oxcAAR::parseOxcalOutput(oxText)
  L$chronData[[chron.num]]$model[[model.num]]$distributionTable <- vector(mode = "list",length = length(oxData))
  for(dd in 1:length(oxData)){
    dTable = list()
    dTable$age = list(values = convertAD2BP(oxData[[dd]]$raw_probabilities$dates),
                      units =  "BP", 
                      variableName = "age")
    dTable$probabilityDensity = list(values = oxData[[dd]]$raw_probabilities$probabilities , 
                                     variableName = "probabilityDensity")
    dTable$labId <- oxData[[dd]]$name
    dTable$depth  <- model.parameters$inputData$depth[which(model.parameters$inputData$labID == oxData[[dd]]$name)]
    dTable$depth.units = depth.units
    L$chronData[[chron.num]]$model[[model.num]]$distributionTable[[dd]] = dTable
  }
  
  
  # get ensemble data -------------------------------------------------------
  
  MCMCfile <- file.path(dirname(oxcal.result.file.path),"MCMC_Sample.csv")
  
  oxEns <- read.csv(MCMCfile)
  
  
  ensembleTable=list()
  ensembleTable$depth$values <- model.parameters$inputData$depth
  ensembleTable$depth$variableName  <-  "depth"
  ensembleTable$depth$units <- depth.units
  
  names(oxEns)
  goodColumns <- seq(3,ncol(oxEns)-2)
  
  if(length(goodColumns) != length( ensembleTable$depth$values)){
    goodColumns <- seq(2,ncol(oxEns)-2)
    if(length(goodColumns) != length( ensembleTable$depth$values)){
      stop("depths and age ensemble levels don't match!")
    }
  }
  
  #only take the end of the posterior
  goodRows <- seq(nrow(oxEns)-max.ens+1,nrow(oxEns))
  goodRows <- goodRows[goodRows >0]
  
  ensembleTable$ageEnsemble$values <-  convertAD2BP(t(as.matrix(oxEns[goodRows,goodColumns])))
  ensembleTable$ageEnsemble$variableName = "ageEnsemble"
  ensembleTable$ageEnsemble$units <- "BP"
  
  L$chronData[[chron.num]]$model[[model.num]]$ensembleTable[[1]] <-  ensembleTable
  
  
  
  
  return(L)
  
}

#' @export
#' @author Nick McKay
#' @family oxcal
#' @import oxcAAR
#' @importFrom crayon bold yellow cyan red green blue 
#' @title Generate an oxcal Age Model and add it into a LiPD object
#' @description This is a high-level function that uses oxcal to simulate an age model, and stores this as an age-ensemble in a model in chronData. If needed input variables are not entered, and cannot be deduced, it will run in interactive mode. See Bronk Ramsey et al. 2008 doi:10.1016/j.quascirev.2007.01.019
#' @inheritParams createOxcalModel
#' @param max.ens the maximum number of ensembles to load in (default = 1000)
#' @param static.reservoir.age optionally assign a deltaR to all radiocarbon dates
#' @param static.reservoir.age.unc optionally assign a deltaR uncertainty to all radiocarbon dates
#' @param oxcal.path path to oxcal binaries
#' @param surface.age specify the age of the surface
#' @param surface.age.unc surface age uncertainty
#' @param surface.age.depth depth of the surface age
#' @param oxcal.code.export.path optionally output the oxcal code by specifying a path (default = NA)
#' @inheritParams createChronMeasInputDf
#' @inheritParams writeBacon
#' @inheritDotParams createChronMeasInputDf
#' @return L The single LiPD object that was entered, with methods, ensembleTable, summaryTable and distributionTable added to the chronData model.
#' @examples 
#' \dontrun{
#' #Run in interactive mode:
#' L = runOxcal(L)
#' 
#' #Run in noninteractive mode, describing everything:
#' L = runOxcal(L,chron.num = 1, meas.table.num = 1, model.num = 3, bacon.dir = "~/Bacon/",site.name = "MSB2K", cc = 1)
#' }
#' @section Long-form example:
#' \href{http://nickmckay.github.io/GeoChronR/articles/oxCal.html}{View a full-fledged example of how to use this function.}
runOxcal <-  function(L,
                      chron.num=NA,
                      meas.table.num = NA,
                      oxcal.path="~/OxCal/",
                      model.num=NA,
                      remove.rejected=TRUE,
                      overwrite=TRUE,
                      max.ens = 1000,
                      static.reservoir.age = NA,
                      static.reservoir.age.unc = NA,
                      surface.age = NA,
                      surface.age.unc = NA,
                      surface.age.depth = 0,
                      depth.interval = NA,
                      events.per.unit.length = NA,
                      events.per.unit.length.uncertainty = 0,
                      outlier.prob = .05,
                      cal.curve = "IntCal20",
                      oxcal.code.export.path = NA,
                      ...){
  
  
  #get oxcal directory
  oxcAAR::quickSetupOxcal(path = oxcal.path)
  
  #initialize chron.num
  if(any(is.na(chron.num))){
    if(length(L$chronData)==1){
      chron.num=1
    }else{
      chron.num=as.integer(readline(prompt = "Which chronData do you want to run oxcal for? "))
    }
  }
  
  
  #initialize model number
  if(any(is.na(model.num))){
    if(is.null(L$chronData[[chron.num]]$model[[1]])){
      #no models, this is first
      model.num=1
    }else{
      print(paste("You already have", length(L$chronData[[chron.num]]$model), "chron model(s) in chronData" ,chron.num))
      print(paste("If you want to create a new model, enter", length(L$chronData[[chron.num]]$model)+1))
      
      model.num=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  
  #create the chron data.frame
  cdf <- createChronMeasInputDf(L,...)
  
  #assign in reservoir ages?
  if(!any(is.na(static.reservoir.age))){
    cdf$reservoirAge <- static.reservoir.age
  }
  if(!any(is.na(static.reservoir.age.unc))){
    cdf$reservoirAgeUnc <- static.reservoir.age.unc
  }
  
  #add surface age estimate?
  if(!any(is.na(surface.age))){
    
    if(any(is.na(surface.age.unc))){
      stop("you cannot add a surface age without an uncertainty")
    }
    
    sage <- data.frame(labID = "surface",
                       age = surface.age,
                       ageUnc = surface.age.unc,
                       depth = surface.age.depth)
    cdf <- dplyr::bind_rows(sage,cdf)
  }
  
  
  #remove rejected
  which.rejected <- which(!is.na(cdf$rejected))
  if(remove.rejected & length(which.rejected)>1){
    cdf <- cdf[-which.rejected,]
  }
  
  #prepare for model input
  if(any(is.na(depth.interval))){
    #take a stab at it
    depth.interval <- abs(diff(range(cdf$depth)))/50
    cat(crayon::bold(paste("No depth.interval entered, trying",crayon::red(depth.interval),"\n Specify your own depth.interval if preferred\n")))
  }
  
  if(any(is.na(events.per.unit.length))){
    #take a stab at it
    events.per.unit.length <- 1/depth.interval
    cat(crayon::bold(paste("No events.per.unit.length entered, trying",crayon::red(events.per.unit.length),"\n Specify your own events.per.unit.length if preferred\n")))
  }
  
  cat(crayon::blue("Oxcal is now running, depending on your settings and your computer, this may take a few minutes to several hours. The model is complete when a table of model diagnostics appears.\n"))
  
  #create the model input
  oxMod <- createOxcalModel(cdf,
                            depth.interval = depth.interval,
                            events.per.unit.length = events.per.unit.length,
                            events.per.unit.length.uncertainty = events.per.unit.length.uncertainty,
                            outlier.prob = outlier.prob,
                            cal.curve = cal.curve)
  
  #optionally export the model code
  if(!is.na(oxcal.code.export.path)){
    saveRDS(oxMod,file = stringr::str_c(oxcal.code.export.path,".RDS"))
    readr::write_file(oxMod$modelText,file = oxcal.code.export.path)
  }
  
  
  #remove old MCMC results
  unlink(file.path(tempdir(),"MCMC_Sample.csv"))
  
  #run the file!
  oxcal.result.file.path <- oxcAAR::executeOxcalScript(oxMod$modelText)
  
  
  L <- loadOxcalOutput(L,
                       oxcal.result.file.path,
                       oxMod,
                       chron.num = chron.num,
                       max.ens = max.ens,
                       model.num = model.num,
                       make.new = overwrite)
  return(L)
}

#' Title
#'
#' @param model.text a character vector of the model input
#' @param oxcal.path path to Oxcal executable
#' @param overwrite overwrite the previous run
#' @param max.ens the maximum number of ensembles to load in (default = 1000)
#' @param oxcal.path path to oxcal binaries
#' @inheritParams writeBacon
#'
#' @return a LiPD object
runOxcalModel <- function(L,
                          model.text,
                          model.num=NA,
                          chron.num=NA,
                          oxcal.path="~/OxCal/",
                          overwrite=TRUE,
                          max.ens = 1000){
  
  #get oxcal directory
  oxcAAR::quickSetupOxcal(path = oxcal.path)
  
  #initialize chron.num
  if(any(is.na(chron.num))){
    if(length(L$chronData)==1){
      chron.num=1
    }else{
      chron.num=as.integer(readline(prompt = "Which chronData do you want to run oxcal for? "))
    }
  }
  
  
  #initialize model number
  if(any(is.na(model.num))){
    if(is.null(L$chronData[[chron.num]]$model[[1]])){
      #no models, this is first
      model.num=1
    }else{
      print(paste("You already have", length(L$chronData[[chron.num]]$model), "chron model(s) in chronData" ,chron.num))
      print(paste("If you want to create a new model, enter", length(L$chronData[[chron.num]]$model)+1))
      
      model.num=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  
  #remove old MCMC results
  unlink(file.path(tempdir(),"MCMC_Sample.csv"))
  
  #run the file!
  oxcal.result.file.path <- oxcAAR::executeOxcalScript(model.text)
  
  
  L <- loadOxcalOutput(L,
                       oxcal.result.file.path,
                       model.parameters = model.text,
                       chron.num = chron.num,
                       max.ens = max.ens,
                       model.num = model.num,
                       make.new = overwrite)
  return(L)
}

