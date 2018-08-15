
#' @export
#' @importFrom dplyr bind_cols bind_rows group_by
#' @import tibble
#' @importFrom purrr map_df
#' @import arsenal
#' @family LiPD manipulation
#' @title create tidy data.frame from TS
#' @description takes a TS object and turns it into a long, tidy, data.frame. Useful for data manipulation and analysis in the tidyverse and plotting
#' @param TS a LiPD Timeseries object
#' @return a tidy data.frame
tidyTs <- function(TS){
  options(warn = -2)
  pb <- txtProgressBar(min=0,max=length(TS),style=3)
  print(paste("Tidying your ",length(TS)," timeseries"))
  
  
  for(i in 1:length(TS)){
    setTxtProgressBar(pb, i)
    
    ti <- TS[[i]]
    
    #exclude any ensembles (For now)
    is.mat <- sapply(ti,is.matrix)
    ncolumns <- rep(0,length = length(is.mat)) 
    ncolumns[which(is.mat)] <- sapply(ti[which(is.mat)],ncol)

    if(any(ncolumns>1)){
      ti <- ti[-which(ncolumns>1)]
    }   

    
    #find which entries are vectors. Year and value should be. There could be more.
    al <- sapply(ti,length)
    
    #going to assume that we only want the longest ones here
    long <- which(al==max(al))
    
    if(!any(names(long)=="paleoData_values")){
      stop(paste0(as.character(i),": paleoData_values didn't show up as being the longest vector"))
    }
    
    if(!(any(names(long)=="year") | any(names(long)=="age") | any(names(long)=="depth") )){
      stop(paste0(as.character(i),": There must be an 'age', 'year', or 'depth' column that's the same length as paleoData_values"))
    }
    
    sdf <- tibble::as.tibble(ti[long])
    
    #separate numeric and character values
    if(is.character(sdf$paleoData_values)){
      sdf$paleoData_values_char <- sdf$paleoData_values
      sdf$paleoData_values <- NA
    }
    
    
    #handle ts variables that are longer than 1, but not the full length by concatenating
    
    med <- ti[which(al<max(al) & al>1)]
    collapsed <- sapply(med, paste,collapse = ", ")
    ti[which(al<max(al) & al>1)] <- collapsed
    
    #check length again
    al2 <- sapply(ti,length)
    
    #replicate the metadata to each observation row
    short <- which(al2==1)
    mdf <- as.data.frame(ti[short])
    meta.df <- purrr::map_df(seq_len(nrow(sdf)), ~mdf)
    
    #combine them together
    tdf <- dplyr::bind_cols(sdf,meta.df)
    if(i == 1){
      tidyData <- tdf
    }else{
      
      
      
      nt <- try(dplyr::bind_rows(tidyData,tdf),silent = T)
      if(is.data.frame(nt)){
        tidyData <- nt
      }else{#try to fix it.
        comp <- arsenal::compare(tidyData,tdf)
        class1 <- unlist(comp$vars.summary$class.x)
        class2 <- unlist(comp$vars.summary$class.y)
        tc <- comp$vars.summary$var.x[which(class1 == "character" & class2 == "numeric")]
        for(tci in 1:length(tc)){
          tdf[tc[tci]] <- as.character(tdf[tc[tci]])
        }
        tidyData <- dplyr::bind_rows(tidyData,tdf)
      }
      
    }
  }
  tidyData <- dplyr::group_by(tidyData,paleoData_TSid)
  return(tidyData)
}



#' @export
#' @family LiPD manipulation
#' @title pull variable out of TS object
#' @description pulls all instances of a single variable out of a TS
#' @param TS a LiPD Timeseries object
#' @param variable the name of variable in a TS object
#' @return a vector of the values, with NA representing instances without this variable.
pullTsVariable = function(TS,variable){
  allNames <- unique(unlist(sapply(TS,names)))
  
  #test for exact match
  which.var <- which(variable == allNames)
  if(length(which.var) == 0){#try a fuzzier search
    which.var <- which(grepl(pattern = variable,x = allNames,ignore.case = TRUE))
    if(length(which.var) == 1){#
      warning(paste0("Couldn't find exact match for '",variable,"', using ",allNames[which.var]," instead."))
    }else if(length(which.var) == 0){
      stop(paste0("Couldn't find any matches for '",variable,"', stopping"))
    }else{
      stop(paste0("Found no exact, but multiple near matches for '",variable,"'. Here they are: \n",paste0(allNames[which.var],collapse = "\n")))
    }
    variable <- allNames[which.var]  
  }
  
  #pull out the variable
  var <- sapply(TS,"[[",variable)
  
  
  if(is.list(var)){#if it's a list, try to unpack it.
    if(length(unlist(var)) < length(var)){#there are some NULS
      newVar <- matrix(NA,nrow = length(var))
      isNull <- sapply(var, is.null)
      newVar[which(!isNull)] <- unlist(var)
      var <- newVar
    }
  }
  
  return(var)
  
}
#' @export
#' @family LiPD manipulation
#' @title push variable into of TS object
#' @description pulls all instances of a single variable out of a TS
#' @param TS a LiPD Timeseries object
#' @param variable the name of variable in a TS object
#' @param vec a vector of data to be added to the TS object
#' @param createNew allow the function to create a new variable in the TS?
#' @return a vector of the values, with NA representing instances without this variable.
pushTsVariable = function(TS,variable,vec,createNew = FALSE){
  allNames <- unique(unlist(sapply(TS,names)))
  
  if(length(TS) != length(vec)){
    stop("the lengths of TS and vec must match!")
  }
  
  if(!createNew){
    #test for exact match
    which.var <- which(variable == allNames)
    
    if(length(which.var) == 0){#try a fuzzier search
      which.var <- which(grepl(pattern = variable,x = allNames,ignore.case = TRUE))
      if(length(which.var) == 1){#
        warning(paste0("Couldn't find exact match for '",variable,"', using ",allNames[which.var]," instead."))
      }else if(length(which.var) == 0){
        stop(paste0("Couldn't find any matches for '",variable,"', stopping"))
      }else{
        stop(paste0("Found no exact, but multiple near matches for '",variable,"'. Here they are: \n",paste0(allNames[which.var],collapse = "\n")))
      }
      variable <- allNames[which.var]  
    }
  }
  #loop over the variable (Is there a better solution for this? I couldn't find one.)
  for(i in 1:length(TS)){
    TS[[i]][[variable]] <- vec[i]
  }
  
  return(TS)
  
}

#' @export
#' @family LiPD manipulation
#' @title Flip Coordinates
#' @description Swap latitude and longitude in a LiPD object
#' @param L a LiPD object
#' @return a LiPD object
flipCoords = function(L){
  olat = L$geo$latitude
  olon = L$geo$longitude
  L$geo$longitude=olat
  L$geo$latitude= olon
  return(L)
}


#' @title  Map an ageEnsemble variable from a chron model to a paleoMeasurement Table
#' @family LiPD manipulation
#' @description Copies an ageEnsemble from chronData (model) to paleoData (measurementTable), by matching depth and interpolating (extrapolating) as necessary.
#' @param L a lipd object
#' @param age.var name of the age ensemble variable to search for
#' @param depth.var name of the depth variable to search for
#' @param which.paleo an integer that corresponds to which paleoData object (L$paleoData[[?]]) has the measurementTable you want to modify
#' @param which.pmt an integer that corresponds to which paleo measurementTable you want to add the ensemble to?
#' @param which.chron  an integer that corresponds to which chronData object (L$crhonData[[?]]) has the model you want to get the ensemble from
#' @param which.model an integer that corresponds to which chron model you want to get the ensemble from?
#' @param which.ens an integer that corresponds to which chron model ensembleTable you want to get the ensemble from?
#' @param max.ensemble.members Maximum number of ensemble members to map
#' @param strictSearch Use a strictSearch to look for the ageEnsemble and depth variables. TRUE(default) or FALSE. 
#' @return L a lipd object
#' @export
mapAgeEnsembleToPaleoData = function(L,age.var = "age",depth.var = "depth",which.paleo=NA,which.pmt=NA,which.chron=NA,which.model=NA,which.ens = NA,max.ensemble.members=NA,strictSearch=FALSE){
  print(L$dataSetName)
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
  copyAE  = FALSE
  
  print("Looking for age ensemble....")
  ensDepth = selectData(L,tableType = "ensemble",varName = depth.var,where = "chronData",which.data = which.chron,strictSearch = strictSearch,model.num = which.model)$values
  ensAll = selectData(L,tableType = "ensemble",varName = age.var,altNames = c("age","ensemble","year"),where = "chronData",model.num = which.model,which.ens = which.ens,which.data = which.chron,strictSearch = strictSearch)
  if(is.null(ensAll$values)){
    stop("Error: did not find the age ensemble.")
  }
  ens = ensAll$values
  if(is.null(ensDepth)){#if there are no depth data in the ensemble, try to apply the ensemble straight in (no interpolation)
    #check for the same size
    #get year, age or depth from paleodata
    pdya = selectData(L,which.data = which.paleo,varName = "year",always.choose = FALSE,which.ens = which.ens,strictSearch = strictSearch,which.mt = which.pmt)$values
    if(is.null(pdya)){
      pdya = selectData(L,which.data = which.paleo,varName = "age",always.choose = FALSE,which.ens = which.ens,strictSearch = strictSearch,which.mt = which.pmt)$values
    }
    if(is.null(pdya)){
      pdya = selectData(L,which.data = which.paleo,varName = year.var,always.choose = FALSE,which.ens = which.ens,strictSearch = strictSearch,which.mt = which.pmt)$values
    }
    if(is.null(pdya)){
      pdya = selectData(L,which.data = which.paleo,varName = depth.var,always.choose = FALSE,which.ens = which.ens,strictSearch = strictSearch,which.mt = which.pmt)$values
    }
    if(is.null(pdya)){
      stop("Couldnt find depth in the ensembleTable, or year, age or depth in the paleoTable. I need more help from you.")    
    }
    
    #check for length of that variable
    if(length(pdya)  == nrow(ens)){
      copyAE  = TRUE
    }else{
      stop("Couldnt find depth in the ensembleTable, and the paleoData measurementTable has a different number of rows thant the ensemble.")    
    }
  }
  
  
  if(!copyAE){
    #get the depth from the paleo measurement table
    print("getting depth from the paleodata table...")
    depth = selectData(L,which.data = which.paleo,varName = "depth",altNames = "position",always.choose = FALSE,which.ens = which.ens,which.mt = which.pmt)$values
    
    #restrict ensemble members
    if(!is.na(max.ensemble.members)){
      if(ncol(ens)>max.ensemble.members){
        #randomly select the appropriate number of ensemble members
        ens = ens[,sample.int(ncol(ens),size = max.ensemble.members,replace = F)]
      }
    }
    
    #interpolate
    na.depth.i = which(!is.na(depth))
    aei = matrix(nrow = length(depth),ncol = ncol(ens))
    aeig=pbapply::pbapply(X=ens,MARGIN = 2,FUN = function(y) Hmisc::approxExtrap(ensDepth,y,xout=depth[na.depth.i],na.rm=TRUE)$y)
    aei[na.depth.i,] = aeig
    
    
  }else{
    #check to see if the ensemble needs to be flipped
    #correlate pdya with ens[,1]
    
    test.cor <- cor(pdya,ens[,1])
    if(test.cor < 0){
      aei <- apply(ens,2,rev)
    }else{
      aei = ens
    }
  }
  
  #guess
  if(is.na(which.ens)){which.ens=1}
  
  
  #assign into measurementTable
  L$paleoData[[which.paleo]]$measurementTable[[which.pmt]]$ageEnsemble$variableName = ensAll$variableName
  L$paleoData[[which.paleo]]$measurementTable[[which.pmt]]$ageEnsemble$values = aei
  L$paleoData[[which.paleo]]$measurementTable[[which.pmt]]$ageEnsemble$units = ensAll$units
  L$paleoData[[which.paleo]]$measurementTable[[which.pmt]]$ageEnsemble$fromChronData = which.chron
  L$paleoData[[which.paleo]]$measurementTable[[which.pmt]]$ageEnsemble$frommodel = which.model
  L$paleoData[[which.paleo]]$measurementTable[[which.pmt]]$ageEnsemble$description = paste("age ensemble pulled from chronData", which.chron,"model",which.model,"- fit to paleoData depth with linear interpolation")
  
  
  return(L)
  
  
}

#' @title  What OS is this?
#' @description Returns the OS
#' @return A string ("osx","linux",or "windows")
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

#' @title  Select a LiPD "variable list"
#' @family LiPD manipulation
#' @description Selects and extracts a LiPD "variable list"
#' @param L a lipd object
#' @param varName string name of the variable to extract
#' @param where "paleoData" or "chronData"
#' @param which.data an integer that corresponds to which paleo or chron Data object (L$<where>Data[[?]]) has the variable you want?
#' @param which.mt an integer that corresponds to which paleo measurementTable has the variable you want?
#' @param tableType What type of table do you want to select data from? ("measurement", "summary" or "ensemble")
#' @param always.choose Force selection of the variable from a list
#' @param altNames A vector of strings for alternative names to search for
#' @param model.num an integer that corresponds to which model that has the variable you want
#' @param which.ens an integer that corresponds to which ensembleTable you want to get the variable from?
#' @param which.sum an integer that corresponds to which summaryTable you want to get the variable from?
#' @param strictSearch Use a strictSearch to look for the ageEnsemble and depth variables. TRUE(default) or FALSE. 
#' @return A LiPD "variable list" object
#' @export
selectData = function(L,varName=NA,where="paleoData",which.data=NA,tableType = "measurement", which.mt=NA,always.choose=FALSE,altNames=NA,model.num = 1,which.ens=1,which.sum = 1,strictSearch = FALSE){
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
  
  ind = getVariableIndex(MTD,varName = varName,always.choose = always.choose,altNames = altNames,strictSearch = strictSearch)
  
  varList = MTD[[ind]]
  
  return(varList)
  
  
  
}

#' @title Get the index of variable list
#' @family LiPD manipulation
#' @description Gets the index for a LiPD "variable list"
#' @param table a LiPD measurement, ensemble or summary Table
#' @param varName string name of the variable to extract
#' @param altNames A vector of strings for alternative names to search for
#' @param ignore A vector of strings of variableNames to ignore
#' @param always.choose Force selection of the variable from a list
#' @param strictSearch Use a strictSearch to look for the ageEnsemble and depth variables. TRUE(default) or FALSE. 
#' @return An integer index
#' @export
getVariableIndex = function(table,varName=NA,altNames=varName,ignore=NA,always.choose=FALSE,strictSearch=FALSE){
  
  #check to see if varName is null, and return 0 if so
  if(is.null(varName)){
    return(NA)
  }
  
  varName <- tolower(varName)
  #restrict to lists  
  #find variables within the table, and their index
  allNames = tolower(names(table))
  listI=which(!sapply(table,class)=="list")
  
  if(!is.na(ignore)){
    if(is.numeric(ignore)){
      ti=ignore
    }else{
      ti=which(allNames %in% tolower(ignore))
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
    }else{
      idi=0
    }
  }
  
  if(is.na(idi)){
    index=NA
  }else if(idi==0){
    index=NA
  }else{
    index=which(allNames==cnames[idi])
  }
  return(index)
  
  
}

#' @export
#' @title Align and bin two timeseries into comparable bins
#' @description Use this to put two timeseries on different timesteps onto equivalent bins
#' @param timeX matrix of age/time ensembles, or single column
#' @param valuesX matrix of values ensembles, or single column
#' @param timeY matrix of age/time ensembles, or single column
#' @param valuesY matrix of values ensembles, or single column
#' @param binvec vector of bin edges for binning step
#' @param binstep spacing of bins, used to build bin step
#' @param binfun function to use during binning (mean, sd, and sum all work)
#' @param max.ens maximum number of ensemble members to regress
#' @param minObs minimum number of points required to calculate regression
#' @return list of binned data output:
#' \itemize{
#' \item binX: binned values from X
#' \item binY: binned values from Y
#' \item binstep: interval of the binning
#' \item yearBins: bins along time
#' }
#' @author Nick McKay
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
