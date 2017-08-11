#'@export
flipCoords = function(L){
  olat = L$geo$latitude
  olon = L$geo$longitude
  L$geo$longitude=olat
  L$geo$latitude= olon
  return(L)
}


#'  Create an ageEnsemble variable in a paleoMeasurement Table
#'
#' @param L a lipd object
#' @param which.paleo an integer that corresponds to which paleoData object (L$paleoData[[?]]) has the measurementTable you want to modify
#'
#' @return L a lipd object
#' @export
mapAgeEnsembleToPaleoData = function(L,which.paleo=NA,which.pmt=NA,which.chron=NA,which.model=NA,max.ensemble.members=NA,strictSearch=FALSE,which.ens = NA){
  #check on the model first
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
  if(length(L$chronData[[which.chron]]$model)==0){
    stop("No model in this chronData")
  }
  if(is.na(which.model)){
    if(length(L$chronData[[which.chron]]$model)==1){
      #only one model
      which.model=1
    }else{
      print(paste("ChronData", which.chron, "has", length(L$chronData[[which.chron]]$model), "models"))
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
    if(length(L$paleoData[[which.paleo]]$measurementTable)==1){
      #only one pmt
      which.pmt=1
    }else{
      print(paste("PaleoData", which.paleo, "has", length(L$paleoData[[which.paleo]]$measurementTable), "measurement tables"))
      which.pmt=as.integer(readline(prompt = "Which measurement table do you want to put the ensemble in? Enter an integer "))
    }
  }
  

  
  #make sure the ensemble is there, with data
  print("Looking for age ensemble....")
  ensDepth = selectData(L,tableType = "ensemble",varName = "depth",where = "chronData")$values
  ens = selectData(L,tableType = "ensemble",varName = "age",altNames = c("ageEnsemble","year"),where = "chronData",which.ens = which.ens)$values
  
  #get the depth from the paleo measurement table
  print("getting depth from the paleodata table...")
  depth = selectData(L,which.data = which.paleo,varName = "depth",altNames = "position",always.choose = FALSE,which.ens = which.ens)$values
  
  #restrict ensemble members
  if(!is.na(max.ensemble.members)){
    if(ncol(ens)>max.ensemble.members){
      #randomly select the appropriate number of ensemble members
      ens = ens[,sample.int(ncol(ens),size = max.ensemble.members,replace = F)]
    }
  }

  #interpolate
  aei=pbapply::pbapply(X=ens,MARGIN = 2,FUN = function(y) Hmisc::approxExtrap(ensDepth,y,xout=depth,na.rm=TRUE)$y)

  #guess
if(is.na(which.ens)){which.ens=1}
  
  
  #assign into measurementTable
  L$paleoData[[which.paleo]]$measurementTable[[which.pmt]]$ageEnsemble$values = aei
  L$paleoData[[which.paleo]]$measurementTable[[which.pmt]]$ageEnsemble$units = L$chronData[[which.chron]]$model[[which.model]]$ensembleTable[[which.ens]]$ageEnsemble$units
  L$paleoData[[which.paleo]]$measurementTable[[which.pmt]]$ageEnsemble$fromChronData = which.chron
  L$paleoData[[which.paleo]]$measurementTable[[which.pmt]]$ageEnsemble$frommodel = which.model
  L$paleoData[[which.paleo]]$measurementTable[[which.pmt]]$ageEnsemble$description = paste("age ensemble pulled from chronData", which.chron,"model",which.model,"- fit to paleoData depth with linear interpolation")
  
  
  return(L)
  
  
}
  
#' @export
getOs <- function(){
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
selectData = function(L,varName=NA,where="paleoData",which.data=NA,tableType = "measurement", which.mt=NA,always.choose=FALSE,altNames=NA,model.num = 1,which.ens=1,which.sum = 1){
  #paleo or chron
  P = L[[where]]
  
  #which <where>Data
  
  if(is.na(which.data)){
    if(length(P)==1){
      which.data=1
    }else{
      print(names(P))
      which.data=as.integer(readline(prompt = "Which do you want? Select a number "))
    }
  }
  
  if(is.na(tableType)){
    tableType=readline(prompt = "Do you want a variable from a measurementTable (m), model summaryTable (s), or model ensembleTable (e)?")
  }
  
  if(tolower(substr(tableType,1,1))=="m"){
    MT = P[[which.data]]$measurementTable
  }else{#check on which model
    if(is.na(model.num)){
      if(length(P[[which.data]]$model)==1){
        model.num=1
      }else{
        print(paste0("There are ",length(P[[which.data]]$model)," models. Which do you want?"))
        model.num=as.integer(readline(prompt = "Which model do you want? Select a number "))
      }
    }
  }
  
  
  
  if(tolower(substr(tableType,1,1))=="e"){MT = P[[which.data]]$model[[model.num]]$ensembleTable}
  if(tolower(substr(tableType,1,1))=="s"){MT = P[[which.data]]$model[[model.num]]$summaryTable}
  
  
  
  #initialize table number
  if(is.na(which.mt)){
    if(length(MT)==0){
      stop(paste0("this object in ",where,"[[",as.character(which.data),"]] has ", as.character(length(MT)), " tables"))
    }
    if(length(MT)==1){
      #only one pmt
      which.mt=1
    }else{
      print(paste0("this object in ",where,"[[",as.character(which.data),"]] has ", as.character(length(MT)), " tables"))
      which.mt=as.integer(readline(prompt = "Which table do you want? Enter an integer "))
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
        n = readline(prompt="please type the number for the correct match, or a zero if none match: ")
        idi=as.numeric(n)
      }else if(length(idi)>1){
        cat(paste("Multiple possible matches for",varName), "\n")
        for(p in 1:length(idi)){
          cat(paste(p,"-",cnames[idi[p]]), "\n")
        }
        n = readline(prompt="please type the number for the correct match, or a zero if you want more options: ")
        if(as.numeric(n)==0){
          cat("OK, here are all your options: ", "\n")
          for(p in 1:length(cnames)){
            cat(paste(p,"-",cnames[p]), "\n")
          }
          n = readline(prompt="please type the number for the correct match, or a zero if none match: ")
          idi=as.numeric(n)
        }else{
          idi = idi[as.numeric(n)]
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

alignTimeseriesBin = function(timeX,valuesX,timeY,valuesY,binvec = NA,binstep = NA ,binfun=mean,max.ens=NA,minObs=10){
  #check to see if time and values are "column lists"
  if(is.list(timeX)){
    otx=timeX
    timeX=timeX$values}
  if(is.list(timeY)){
    oty=timeY
    timeY=timeY$values}
  if(is.list(valuesX)){
    ovx=valuesX
    valuesX=valuesX$values}
  if(is.list(valuesY)){
    ovy=valuesY
    valuesY=valuesY$values}
  
  
  #make them all matrices
  timeX = as.matrix(timeX)
  timeY = as.matrix(timeY)
  valuesX = as.matrix(valuesX)
  valuesY = as.matrix(valuesY)
  
  if(nrow(timeX) != nrow(valuesX)){stop("timeX and valuesX must have the same number of rows (observations)")}
  if(nrow(timeY) != nrow(valuesY)){stop("timeY and valuesY must have the same number of rows (observations)")}
  
  if(all(is.na(binvec))){
    if(is.na(binstep)){
      stop("Either a binvec or binstep must be specified")
    }else{
      #look for common overlap
      binStart=max(c(min(timeX,na.rm=TRUE),min(timeY,na.rm=TRUE)))
      binStop=min(c(max(timeX,na.rm=TRUE),max(timeY,na.rm=TRUE)))
      binvec=seq(binStart,binStop,by=binstep)
    }
  }
  
  #create ensemble bins
  dum = binEns(time = timeX,values = valuesX,binvec = binvec,binfun=binfun,max.ens=max.ens)
  yearX = dum$time
  binX = dum$matrix
  binY = binEns(time = timeY,values = valuesY,binvec = binvec,binfun=binfun,max.ens=max.ens)$matrix
  
  #remove columns that have less than minObs datapoints
  good = which(apply(!is.na(binX),2,sum)>=minObs)
  if(length(good)==0){
    stop(paste("none of the columns have",minObs,"or more datapoints"))
  }
  binX = as.matrix(binX[,good])
  
  
  good = which(apply(!is.na(binY),2,sum)>=minObs)
  if(length(good)==0){
    stop(paste("none of the columns have",minObs,"or more datapoints"))
  }
  binY = as.matrix(binY[,good])
  
  
  if(is.na(binstep)){#if the binstep isn't specified
    binstep=abs(mean(diff(binvec,na.rm=TRUE)))
  }

return(list(binX = binX, binY=binY,binstep=binstep,yearBins = yearX))
}
  