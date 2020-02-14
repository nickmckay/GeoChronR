#' Convert variable names to strings
#'
#' @param varName 
#'
#' @return
#' @export
#'
#' @examples
stringifyVariables <- function(varName,usedValue){
  if(is.null(usedValue)){
    so <- paste0(varName," = NULL")
  }else if(isTRUE(usedValue == "NULL")){
    so <- paste0(varName," = ", usedValue)
  }else{
    so <- paste0(varName," = '", usedValue, "'")
  }
  return(so)
}


#' askUser
#' @description Helper function to consistently get user input
#' @param query 
#'
#' @return user input
#' @export
askUser <- function(query){
  return(readline(prompt = cat(crayon::bold(query))))
}


#' Title
#'
#' @param L 
#' @param which.chron 
#' @param which.mt 
#' @param overwrite 
#' @param site.name 
#' @param modelNum 
#' @param useMarine 
#' @param labIDVar 
#' @param age14CVar 
#' @param age14CuncertaintyVar 
#' @param ageVar 
#' @param ageUncertaintyVar 
#' @param depthVar 
#' @param reservoirAge14CVar 
#' @param reservoirAge14CUncertaintyVar 
#' @param rejectedAgesVar 
#'
#' @import purrr crayon 
#' @return a standardized dataframe of chron measurements
#' @export
createChronMeasInputDf <- function(L,
                                   which.chron=NA,
                                   which.mt = NA,
                                   labIDVar="labID",
                                   age14CVar = "age14C", 
                                   age14CuncertaintyVar = "age14CUnc", 
                                   ageVar = "age", 
                                   ageUncertaintyVar = "ageUnc", 
                                   depthVar = "depth", 
                                   reservoirAge14CVar = "reservoirAge",
                                   reservoirAge14CUncertaintyVar = "reservoirAge14C",
                                   rejectedAgesVar="rejected"){
  
  
  #initialize which.chron
  if(is.na(which.chron)){
    if(length(L$chronData)==1){
      which.chron=1
    }else{
      which.chron=as.integer()
    }
  }
  
  #We no longer get this
  # if(is.na(modelNum)){
  #   if(is.null(L$chronData[[which.chron]]$model[[1]])){
  #     #no models, this is first
  #     modelNum=1
  #   }else{
  #     print(cat(crayon::yellow(paste("You already have", length(L$chronData[[which.chron]]$model), "chron model(s) in chronData" ,which.chron))))
  #     modelNum=as.integer(askUser("Enter the number for this model- will overwrite if necessary "))
  #   }
  # }
  # 
  # 
  #pull out chronology
  C=L$chronData[[which.chron]]
  
  #check for measurementTables
  if(is.na(which.mt)){
    if(length(C$measurementTable)==1){
      which.mt = 1  
    }else{
      print(paste("There are", length(L$chronData[[which.chron]]$measurementTable), "measurement tables in chronData " ,which.chron))
      which.mt=as.integer(askUser("Which do you want to use here? (Enter an integer)"))
    }
  }
  
  MT=C$measurementTable[[which.mt]]
  
  #NM: move this to google speadsheet import?
  
  #go through required fields for bacon
  v2go <- c("labID",
            "age14C",
            "age14CUnc",
            "age",
            "ageUnc",
            "depth",
            "reservoirAge",
            "reservoirAgeUnc",
            "rejected")
  
  #user input variable names
  v2gu <- c(
    ifelse(is.null(labIDVar),"NULL",labIDVar),
    ifelse(is.null(age14CVar),"NULL",age14CVar),
    ifelse(is.null(age14CuncertaintyVar),"NULL",age14CuncertaintyVar),
    ifelse(is.null(ageVar),"NULL",ageVar),
    ifelse(is.null(ageUncertaintyVar),"NULL",ageUncertaintyVar),
    ifelse(is.null(depthVar),"NULL",depthVar),
    ifelse(is.null(reservoirAge14CVar),"NULL",reservoirAge14CVar),
    ifelse(is.null(reservoirAge14CUncertaintyVar),"NULL",reservoirAge14CUncertaintyVar),
    ifelse(is.null(rejectedAgesVar),"NULL",rejectedAgesVar)
  )
  
  #input variable names
  v2gus <-  c("labIDVar",
              "age14CVar",
              "age14CuncertaintyVar",
              "ageVar",
              "ageUncertaintyVar",
              "depthVar",
              "reservoirAge14CVar",
              "reservoirAge14CUncertaintyVar",
              "rejectedAgesVar")
  
  #alt names
  v2ga <- c("id",#labIDvar,
            "age",#age14CVar,
            "unc",#age14CuncertaintyVar,
            "age",#ageVar,
            "unc",#ageUncertaintyVar,
            "depth",#depthVar,
            "reservoir",#reservoirAge14CVar,
            reservoirAge14CUncertaintyVar,#reservoirAge14CUncertaintyVar no good alt name here
            "reject")#rejectedAgesVar)
  
  #verbose names
  v2gv <-   c("laboratory ID",  #labIDvar,
              "radiocarbon ages",   #age14CVar,
              "1-sigma radiocarbon age uncertainty (+/-)",  #age14CuncertaintyVar,
              "calibrated/calendar ages",  #ageVar
              "2-sigma calibrated age uncertainty (+/-)", #ageUncertaintyVar,
              "depth or position",   #depthVar,
              "radiocarbon reservoir age offsets (deltaR)",  #reservoirAge14CVar,
              "radiocarbon reservoir age offsets (deltaR) uncertainties", #reservoirAge14CUncertaintyVar,
              "rejected ages")  #rejectedAgesVar))
  
  #set up used string
  v2gUsed <- v2gu
  
  nObsTable <- max(purrr::map_dbl(MT,function(x){
    if(is.list(x)){
      l <- length(x$values)
    }else{
      l <- 0
    }
    return(l)
  }
  ))
  
  chronDf <- as.data.frame(matrix(NA,nrow = nObsTable,ncol = length(v2go)))
  names(chronDf) <- v2go
  for(tv in 1:length(v2go)){#loop through the variables
    cat(crayon::cyan(paste("Looking for",crayon::bold(v2gv[tv]),"\n")))
    ci <- getVariableIndex(MT,v2gu[tv],altNames = v2ga[tv])
    if(!is.na(ci)){
      chronDf[,tv] <- MT[[ci]]$values
      v2gUsed[tv] <- MT[[ci]]$variableName
    }else{
      cat(crayon::red(paste(crayon::bold(v2gv[tv]),"does not seem to exist, moving on.\n")))
      v2gUsed[tv] <- "NULL"
    }
  }       
  
  #print results...
  print("Variable choices for reuse...")
  varUsedStr <- ""
  for(tv in 1:length(v2gu)){
    varUsedStr <- paste0(varUsedStr,stringifyVariables(v2gus[tv],v2gUsed[tv]),", ") 
  }
  
  
  #store this for later use
  assign("chron_varUsedStr",value = varUsedStr,envir = geoChronREnv)
  cat(crayon::green(crayon::bold("For future reference: here are the options you chose:\n Find later with getLastVarString()\n")))
  cat(crayon::green(paste0(varUsedStr,"\n")))
  
  #calculate a few more columns
  #create combined age column.  Assign calibrated ages when 14C ages are empty
  chronDf$allAge <- chronDf$age14C
  no14Ci <- which(is.na(chronDf$age14C))
  chronDf$allAge[no14Ci] <- chronDf$age[no14Ci]
  
  #Create combined age uncertainty column. Assign calibrated uncertainties when 14C uncertainty is empty
  chronDf$allUnc <- chronDf$age14CUnc
  chronDf$allUnc[no14Ci] <- chronDf$ageUnc[no14Ci]
  
  #Createa an age type column
  chronDf$ageType <- "14C"
  chronDf$ageType[no14Ci] <- "cal"
  
  return(chronDf)
  
}

#' Get the last set of parameters you used in createChronMeasInputDf()
#'
#' @return
#' @export
getLastVarString <- function(){
  if(exists("chron_varUsedStr",envir = geoChronREnv)){
    
    return(get("chron_varUsedStr",envir = geoChronREnv))
  }else{
    return("createChronMeasInputDf() has not yet been successfully run this session")
  }
}

