#@export
pull.data.from.TS = function(TS,var2get){
  
#query the TS get only the data you want
civar = lapply(TS,"[[",var2get)
newvar=c()
newvar[sapply(civar,is.null)]=NA
newvar[!sapply(civar,is.null)]=sapply(TS[!sapply(civar,is.null)],"[[",var2get)

return(newvar)
}



#'  Create an ageEnsemble variable in a paleoMeasurement Table
#'
#' @param L a lipd object
#' @param which.paleo an integer that corresponds to which paleoData object (L$paleoData[[?]]) has the measurementTable you want to modify
#'
#' @return L a lipd object
#' @export
ageEnsemble.to.paleoData = function(L,which.paleo=NA,which.pmt=NA,which.chron=NA,which.model=NA,max.ensemble.members=NA,strictSearch=FALSE){
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
  if(strictSearch){
    aei=getVariableIndex(L$chronData[[which.chron]]$chronModel[[which.model]]$ensembleTable,varName = "ageEnsemble",always.choose = FALSE,strictSearch = strictSearch)
  }else{
aei=getVariableIndex(L$chronData[[which.chron]]$chronModel[[which.model]]$ensembleTable,varName = "ageEnsemble",altNames = c("age","year"))
}
    if(is.na(aei)){
    stop("There doesn't seem to be in values in the ensemble table for this model (there may not be an ensembleTable, or even a model)")
  }
  #grab the ensemble
  ens=L$chronData[[which.chron]]$chronModel[[which.model]]$ensembleTable[[aei]]$values
  ensDepth = L$chronData[[which.chron]]$chronModel[[which.model]]$ensembleTable$depth$values
  
  
  #get the depth from the paleo measurement table
  print("getting depth from the paleodata table...")
  di = getVariableIndex(L$paleoData[[which.paleo]]$paleoMeasurementTable[[which.pmt]],"depth",altNames = "position",always.choose = FALSE,strictSearch = strictSearch)

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
  
#' @export
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)){
      os <- "osx"
    }else if(grepl("linux-gnu", R.version$os)){
      os <- "linux"
    }else{
      os <- "windows"
    }
  }
  return(tolower(os))
  
}

#' @export
select.data = function(L,varName=NA,where="paleoData",which.data=NA, which.mt=NA,always.choose=TRUE,altNames=NA){
  #paleo or chron
  P = L[[where]]
  
  #which <where>Data
  
  if(is.na(which.data)){
    if(length(P)==1){
      which.data=1
    }else{
      print(names(P))
      which.data=as.integer(readline(prompt = "Which paleoData do you want to put this age ensemble in? Select a number "))
    }
  }
  
  #initialize measurement table number
  mti=which(grepl(names(P[[which.data]]),pattern = "MeasurementTable"))
  MT = P[[which.data]][[mti]]
  if(is.na(which.mt)){
    if(length(MT)==1){
      #only one pmt
      which.mt=1
    }else{
      print(paste(where, which.paleo, "has", length(MT), "measurement tables"))
      which.mt=as.integer(readline(prompt = "Which measurement table do you want to put the ensemble in? Enter an integer "))
    }
  }
  
  
  #this is the table of interest  
  MTD=MT[[which.mt]]
  
  ind = getVariableIndex(MTD,varName = varName,always.choose = always.choose,altNames = altNames)
  
  varList = MTD[[ind]]
  
  return(varList)
  
  
  
}

#' @export
getVariableIndex = function(table,varName=NA,altNames=varName,ignore=NA,always.choose=FALSE,strictSearch=FALSE){
  #restrict to lists  
  #find variables within the table, and their index
  
  
  
  allNames = names(table)
  listI=which(!sapply(table,class)=="list")
  
  if(!is.na(ignore)){
    if(is.numeric(ignore)){
      ti=ignore
    }else{
      ti=which(allNames %in% ignore)
    }
    #also ignore anything that's not a list
    
    cnames=allNames[-union(ti,listI)]
    
  }else{
    if(length(listI)>0){
      cnames=allNames[-listI]
    }else{
      cnames=allNames
    }
  }
  if(is.na(varName)){
    cat("Select a variable from this list", "\n")
    for(p in 1:length(cnames)){
      cat(paste(p,"-",cnames[p]), "\n")
    }
    n = readline(prompt="please type the number for the correct match, or a zero if there are no matches: ")
    idi=as.numeric(n)
  }else{
    idi=which(cnames==varName)
    if((length(idi)==0 | always.choose) & !strictSearch){
      cat(paste0("No variable called ", varName, ", or choosing is enforced (always.choose = TRUE)\n"))
      for(i in 1:(length(altNames)+1)){
        if(i==1){
          test = grepl(pattern = varName,cnames,ignore.case = TRUE)
        }else{
          test = (grepl(pattern = altNames[i-1],cnames,ignore.case = TRUE) | test)
        }
      }
      idi = which(test)
      if(length(idi)==0){
        cat("Cant find any candidates, please select from a list", "\n")
        for(p in 1:length(cnames)){
          cat(paste(p,"-",cnames[p]), "\n")
        }
        n = readline(prompt="please type the number for the correct match, or a zero if there are no matches: ")
        idi=as.numeric(n)
      }else if(length(idi)>1){
        cat(paste("Multiple possible matches for",varName), "\n")
        for(p in 1:length(idi)){
          cat(paste(p,"-",cnames[idi[p]]), "\n")
        }
        n = readline(prompt="please type the number for the correct match, or a zero if there are no matches: ")
        if(as.numeric(n)==0){
          idi=0
        }else{
          idi=idi[as.numeric(n)]
        }
      }else{
        cat(paste("Use",cnames[idi], "?"), "\n")
        q = readline(prompt="y or n?")
        if(!grepl(pattern="y",q,ignore.case = TRUE)){
          idi=0
        }
      }
    }
    else if(length(idi)==1){
      print("Found it! Moving on...")
      
    }
  }
  
  
  if(idi==0){
    index=NA
  }else{
    index=which(allNames==cnames[idi])
  }
  return(index)
  
  
}

#' @export
extract.timeseries = function(D){
  #preliminary/hacky version. better to come
  TS=list()
  t=1
  for(f in 1:length(D)){
    L = D[[f]]
    dum = try(ageEnsemble.to.paleoData(L,max.ensemble.members = 1000,which.chron = 1,which.model = 1,strictSearch = TRUE),silent = TRUE)
    if(!grepl(pattern = "error",class(dum)))
    {L=dum}
    for(p in 1:length(L$paleoData)){
      P=L$paleoData[[p]]
      for(pm in 1:length(P$paleoMeasurementTable)){
        PM = P$paleoMeasurementTable[[pm]]
        dontinclude = c("age","year","depth","ageEnsemble")
        tograb = which(!(names(PM) %in% dontinclude) & sapply(PM,is.list))
        for(tg in tograb){
          TS[[t]]=NA
          TS[[t]]$dataSetName = L$dataSetName
          TS[[t]]$archiveType=L$archiveType
          TS[[t]]$geo_latitude=L$geo$latitude
          TS[[t]]$geo_AHT.Region = L$geo$AHT.Region
          TS[[t]]$geo_latitude=L$geo$latitude
          TS[[t]]$geo_siteName=L$geo$siteName
          TS[[t]]$geo_longitude=L$geo$longitude
          TS[[t]]$paleoData_values=PM[[tg]]$values
          TS[[t]]$paleoData_variableName=PM[[tg]]$variableName
          TS[[t]]$paleoData_units=PM[[tg]]$units
          if(!is.null(PM[[tg]]$climateInterpretation)){
            TS[[t]]$climateInterpretation_variable=PM[[tg]]$climateInterpretation$variable
            TS[[t]]$climateInterpretation_seasonality=PM[[tg]]$climateInterpretation$seasonality
            TS[[t]]$climateInterpretation_interpDirection=PM[[tg]]$climateInterpretation$interpDirection
          }
          
          diffgrab = which((names(PM) %in% dontinclude) & sapply(PM,is.list))
          for(dg in 1:length(diffgrab)){
            TS[[t]][[names(diffgrab)[dg]]]=PM[[diffgrab[dg]]]$values
            TS[[t]][[paste0(names(diffgrab)[dg],"Units")]]=PM[[diffgrab[dg]]]$units
          }
          t=t+1
        }
      }
    }
  }
  return(TS)
}




  