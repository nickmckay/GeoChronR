
#write oxcal expression for each date.
oxCalDateExpression <- function(...,defaultOutlierProb = 0.05){
  
  adf <- tibble::tibble(...)
  #setup labID
  if(is.na(adf$labID)){#grab a new name
    adf$labID <- paste0("randomLabId_",paste(sample(c(letters,LETTERS,0:9),10),collapse = ""))
  }
  if("outlierProb" %in% names(adf)){
    if(is.na(adf$outlierProb)){#assign default
      adf$outlierProb <- defaultOutlierProb
    }
  }else{
    adf$outlierProb <- defaultOutlierProb
  }
  
  #figure out date type
  if(!is.na(adf$age14C)){#radiocarbon
    if(is.na(adf$age14CUnc)){stop("seems like a 14C date, but no uncertainty included (age14CUnc)")}  
    
    #create expression
    dateExp <- paste0('R_Date("',adf$labID,'",',adf$age14C,',',adf$age14CUnc,'){ z=',adf$depth,'; Outlier(',adf$outlierProb,'); };\n')
    
  }else if(!is.na(adf$age)){#normally distributed cal date
    if(is.na(adf$ageUnc)){stop("seems like a calibrated date, but no uncertainty included (ageUnc)")}  
    
    
    #create expression
    dateExp <- paste0('C_Date("',adf$labID,'",',convertBP2AD(adf$age),',',adf$ageUnc,'){ z=',adf$depth,'; Outlier(',adf$outlierProb,'); };\n')
    
  }else{#undated layer
    dateExp <- paste0('Date("D"){ z=',adf$depth,'; };\n')
    
  }
  
  return(dateExp)
  
}


#' Title
#'
#' @param cdf 
#' @param depthsToModel 
#' @param depthInterval 
#' @param thin 
#' @param nIt 
#' @param eventsPerUnitLength 
#' @param seqType 
#' @param topBoundary 
#' @param bottomBoundary 
#' @import magrittr dplyr tidyr
#'
#' @return
#' @export
#'
#' @examples
createOxcalModel <- function(cdf,
                             depthsToModel = NA,
                             depthInterval = 10,
                             thin = 50,
                             nIt = 10000,
                             eventsPerUnitLength = 1,
                             seqType = "P_Sequence",
                             topBoundary = "Boundary",
                             bottomBoundary = "Boundary"){


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
modGuts <- purrr::pmap_chr(age2m,oxCalDateExpression,outlierProb = .05)

#model start text
modStart <- paste0(seqType,"(",eventsPerUnitLength,")\n",
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



#load oxcal output
loadOxcalOutput <- function(L,
                            oxcalResultFilePath,
                            modelParameters,
                            depthUnits = "cm",
                            which.chron=NA,
                            modelNum=NA,
                            makeNew=NA,
                            maxEns = 1000){

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
  
  oxText <- readOxcalOutput(oxcalResultFilePath)
  
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

    oxData <- parseOxcalOutput(oxText)
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



  