#' Create a random TSid
#' @return TSid
#' @export
createTSid <- function(){
  
  return(paste(c("R",sample(c(letters,LETTERS,seq(0,9)),size = 10,replace=TRUE)),collapse = ""))
}

#' @export
#' @family LiPD manipulation
#' @title pull variable out of TS object
#' @description pulls all instances of a single variable out of a TS
#' @inheritParams binTs
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
  
  
  if(is.list(var) & !grepl("author",variable) &!grepl("inCompilationBeta[0-9]+_compilationVersion",variable)){#if it's a list, try to unpack it. Unless it's author then don't
    if(length(unlist(var)) < length(var)){#there are some NULlS
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
#' @title Estimate uncertainty estimates from high/low range
#' @description Estimate uncertainty (plus/minus values) from a range of values
#' @importFrom crayon bold yellow cyan red green blue 
#' @importFrom matrixStats rowDiffs
#' @inheritParams selectData
#' @param range.1 name of one of the range variables
#' @param range.2 name of the other range variable
#' @param sigma.range what sigma range are the measurement uncertainties (default is 2)
#'
#' @return MT: a LiPD measurementTable with a new unc.estimate variable
#'
estimateUncertaintyFromRange = function(L,
                                        range.1=NA,
                                        range.2=NA,
                                        paleo.or.chron = "chronData",
                                        sigma.range = 2,
                                        meas.table.num = 1,
                                        paleo.or.chron.num = 1){
  
  cat(crayon::bold("Finding the low end of the range"),"\n")
  v1 <- selectData(L,
                   var.name = range.1,
                   paleo.or.chron = paleo.or.chron,
                   meas.table.num = meas.table.num,
                   paleo.or.chron.num = paleo.or.chron.num)
  
  cat(crayon::bold("Finding the high end of the range"),"\n")
  v2 <- selectData(L,
                   var.name = range.2,
                   paleo.or.chron = paleo.or.chron,
                   meas.table.num = meas.table.num,
                   paleo.or.chron.num = paleo.or.chron.num)
  
  
  
  val1 <- v1$values
  val2 <- v2$values
  diffVals <- abs(matrixStats::rowDiffs(as.matrix(cbind(val1,val2)),na.rm=TRUE))
  uncVal <-  diffVals/sigma.range
  
  MT <-L[[paleo.or.chron]][[paleo.or.chron.num]][["measurementTable"]][[meas.table.num]]
  MT$uncEstimate$values <-  uncVal
  MT$uncEstimate$variableName <-  "uncEstimate"
  MT$uncEstimate$TSid <- paste0("EUR", 
                                 format(Sys.time(), "%y%m%d"), 
                                 paste0(sample(c(letters,LETTERS, 0:9),
                                 size = 16, replace = T), collapse = ""))
  
  MT$uncEstimate$units <- v2$units
  
  L[[paleo.or.chron]][[paleo.or.chron.num]][["measurementTable"]][[meas.table.num]] <- MT
  return(L)
}


#' @title  Map an ageEnsemble variable from a chron model to a paleoMeasurement Table
#' @family LiPD manipulation
#' @description Copies an ageEnsemble from chronData (model) to paleoData (measurementTable), by matching depth and interpolating (extrapolating) as necessary.
#' @inheritParams selectData
#' @param age.var name of the age ensemble variable to search for
#' @param depth.var name of the depth variable to search for
#' @param paleo.num an integer that corresponds to paleo.numData object (L$paleoData[[?]]) has the measurementTable you want to modify
#' @param paleo.meas.table.num an integer that corresponds to paleo.num measurementTable you want to add the ensemble to?
#' @param chron.num  an integer that corresponds to chron.numData object (L$crhonData[[?]]) has the model you want to get the ensemble from
#' @param model.num an integer that corresponds to chron.num model you want to get the ensemble from?
#' @param max.ens Maximum number of ensemble members to map
#' @import pbapply
#' @return L a lipd object
#' @export
mapAgeEnsembleToPaleoData = function(L,age.var = "age",depth.var = "depth",paleo.num=NA,paleo.meas.table.num=NA,chron.num=NA,model.num=NA,ens.table.num = NA,max.ens=NA,strict.search=FALSE){
  print(L$dataSetName)
  #check on the model first
  if(is.null(L$chronData)){
    stop("There's no chronData in this file")
  }
  
  #initialize chron.num
  if(is.na(chron.num)){
    if(length(L$chronData)==1){
      chron.num=1
    }else{
      chron.num=as.integer(readline(prompt = "Which chronData do you want to pull this ensemble from? "))
    }
  }
  
  #initialize model number
  if(length(L$chronData[[chron.num]]$model)==0){
    stop("No model in this chronData")
  }
  if(is.na(model.num)){
    if(length(L$chronData[[chron.num]]$model)==1){
      #only one model
      model.num=1
    }else{
      print(paste("ChronData", chron.num, "has", length(L$chronData[[chron.num]]$model), "models"))
      model.num=as.integer(readline(prompt = "Which chron model do you want to get the ensemble from? Enter an integer "))
    }
  }
  
  
  #initialize paleo.num
  if(is.na(paleo.num)){
    if(length(L$paleoData)==1){
      paleo.num=1
    }else{
      paleo.num=as.integer(readline(prompt = "Which paleoData do you want to put this age ensemble in? "))
    }
  }
  
  #initialize measurement table number
  if(is.na(paleo.meas.table.num)){
    if(length(L$paleoData[[paleo.num]]$measurementTable)==1){
      #only one pmt
      paleo.meas.table.num=1
    }else{
      print(paste("PaleoData", paleo.num, "has", length(L$paleoData[[paleo.num]]$measurementTable), "measurement tables"))
      paleo.meas.table.num=as.integer(readline(prompt = "Which measurement table do you want to put the ensemble in? Enter an integer "))
    }
  }
  
  
  #make sure the ensemble is there, with data
  copyAE  = FALSE
  
  print("Looking for age ensemble....")
  ensDepth = selectData(L,table.type = "ensemble",var.name = depth.var,paleo.or.chron = "chronData",paleo.or.chron.num = chron.num,strict.search = strict.search,model.num = model.num)$values
  ensAll = selectData(L,table.type = "ensemble",var.name = age.var,alt.names = c("age","ensemble","year"),paleo.or.chron = "chronData",model.num = model.num,ens.table.num = ens.table.num,paleo.or.chron.num = chron.num,strict.search = strict.search)
  if(is.null(ensAll$values)){
    stop("Error: did not find the age ensemble.")
  }
  ens = ensAll$values
  if(is.null(ensDepth)){#if there are no depth data in the ensemble, try to apply the ensemble straight in (no interpolation)
    #check for the same size
    #get year, age or depth from paleodata
    pdya = selectData(L,paleo.or.chron.num = paleo.num,var.name = "year",always.choose = FALSE,ens.table.num = ens.table.num,strict.search = strict.search,meas.table.num = paleo.meas.table.num)$values
    if(is.null(pdya)){
      pdya = selectData(L,paleo.or.chron.num = paleo.num,var.name = "age",always.choose = FALSE,ens.table.num = ens.table.num,strict.search = strict.search,meas.table.num = paleo.meas.table.num)$values
    }
    if(is.null(pdya)){
      pdya = selectData(L,paleo.or.chron.num = paleo.num,var.name = year.var,always.choose = FALSE,ens.table.num = ens.table.num,strict.search = strict.search,meas.table.num = paleo.meas.table.num)$values
    }
    if(is.null(pdya)){
      pdya = selectData(L,paleo.or.chron.num = paleo.num,var.name = depth.var,always.choose = FALSE,ens.table.num = ens.table.num,strict.search = strict.search,meas.table.num = paleo.meas.table.num)$values
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
    depth = selectData(L,paleo.or.chron.num = paleo.num,var.name = "depth",alt.names = "position",always.choose = FALSE,ens.table.num = ens.table.num,meas.table.num = paleo.meas.table.num)$values
    
    #check that depth is numeric
    if(!is.numeric(depth)){
      stop("Uh oh, paleo depth is not a numeric vector. That will cause problems - check paleoData[[p]]measurementTable[[m]]$depth$values (or similar if var.name is not depth)")
    }
    
    #restrict ensemble members
    if(!is.na(max.ens)){
      if(ncol(ens)>max.ens){
        #randomly select the appropriate number of ensemble members
        ens = ens[,sample.int(ncol(ens),size = max.ens,replace = F)]
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
  if(is.na(ens.table.num)){ens.table.num=1}
  
  
  #assign into measurementTable
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]]$ageEnsemble$variableName = ensAll$variableName
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]]$ageEnsemble$values = aei
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]]$ageEnsemble$units = ensAll$units
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]]$ageEnsemble$fromChronData = chron.num
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]]$ageEnsemble$frommodel = model.num
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]]$ageEnsemble$description = paste("age ensemble pulled from chronData", chron.num,"model",model.num,"- fit to paleoData depth with linear interpolation")
  
  
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
#' @param L A LiPD object - an R serialization of a single LiPD file. It's a list, and is typically created by `readLipd()`
#' @param var.name string name of the variable to extract
#' @param paleo.or.chron "paleoData" or "chronData"
#' @param paleo.or.chron.num an integer that corresponds to paleo.num or chron Data object (L$<paleo.or.chron>[[?]]) has the variable you want?
#' @param meas.table.num an integer that corresponds to paleo.num measurementTable has the variable you want?
#' @param table.type What type of table do you want to select data from? ("measurement", "summary" or "ensemble")
#' @param always.choose Force selection of the variable from a list
#' @param alt.names A vector of strings for alternative names to search for
#' @param model.num an integer that corresponds to model.num that has the variable you want
#' @param ens.table.num an integer that corresponds to ensembleTable you want to get the variable from?
#' @param sum.table.num an integer that corresponds to which summaryTable you want to get the variable from?
#' @param strict.search Use a strict.search to look for the ageEnsemble and depth variables. TRUE(default) or FALSE. 
#' @return A LiPD "variable list" object
#' @export
selectData = function(L,
                      var.name=NA,
                      paleo.or.chron="paleoData",
                      paleo.or.chron.num=NA,
                      table.type = "measurement",
                      meas.table.num=NA,
                      always.choose=FALSE,
                      alt.names=NA,
                      model.num = 1,
                      ens.table.num=1,
                      sum.table.num = 1,
                      strict.search = FALSE){
  #paleo or chron
  P = L[[paleo.or.chron]]
  
  #which <paleo.or.chron>
  
  if(is.na(paleo.or.chron.num)){
    if(length(P)==1){
      paleo.or.chron.num=1
    }else{
      print(names(P))
      paleo.or.chron.num=as.integer(readline(prompt = "Which do you want? Select a number "))
    }
  }
  
  if(is.na(table.type)){
    table.type=readline(prompt = "Do you want a variable from a measurementTable (m), model summaryTable (s), or model ensembleTable (e)?")
  }
  
  if(tolower(substr(table.type,1,1))=="m"){
    MT = P[[paleo.or.chron.num]]$measurementTable
  }else{#check on model.num
    if(is.na(model.num)){
      if(length(P[[paleo.or.chron.num]]$model)==1){
        model.num=1
      }else{
        print(paste0("There are ",length(P[[paleo.or.chron.num]]$model)," models. Which do you want?"))
        model.num=as.integer(readline(prompt = "Which model do you want? Select a number "))
      }
    }
  }
  
  
  
  if(tolower(substr(table.type,1,1))=="e"){MT = P[[paleo.or.chron.num]]$model[[model.num]]$ensembleTable}
  if(tolower(substr(table.type,1,1))=="s"){MT = P[[paleo.or.chron.num]]$model[[model.num]]$summaryTable}
  
  
  
  #initialize table number
  if(is.na(meas.table.num)){
    if(length(MT)==0){
      stop(paste0("this object in ",paleo.or.chron,"[[",as.character(paleo.or.chron.num),"]] has ", as.character(length(MT)), " tables"))
    }
    if(length(MT)==1){
      #only one pmt
      meas.table.num=1
    }else{
      print(paste0("this object in ",paleo.or.chron,"[[",as.character(paleo.or.chron.num),"]] has ", as.character(length(MT)), " tables"))
      meas.table.num=as.integer(readline(prompt = "Which table do you want? Enter an integer "))
    }
  }
  
  
  #this is the table of interest  
  MTD=MT[[meas.table.num]]
  
  ind = getVariableIndex(MTD,var.name = var.name,always.choose = always.choose,alt.names = alt.names,strict.search = strict.search)
  
  varList = MTD[[ind]]
  
  return(varList)
  
  
  
}

#' @title Get the index of variable list
#' @family LiPD manipulation
#' @description Gets the index for a LiPD "variable list"
#' @inheritParams selectData
#' @param table a LiPD measurement, ensemble or summary Table
#' @param ignore A vector of strings of variableNames to ignore
#' @return An integer index
#' @export
getVariableIndex = function(table,
                            var.name=NA,
                            alt.names=var.name,
                            ignore=NA,
                            always.choose=FALSE,
                            strict.search=FALSE){
  
  #check to see if var.name is null, and return 0 if so
  if(is.null(var.name)){
    return(NA)
  }
  if(isTRUE(var.name == "NULL")){
    return(NA)
  }
  
  var.name <- tolower(var.name)
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
  if(is.na(var.name)){
    cat("Select a variable from this list", "\n")
    for(p in 1:length(cnames)){
      cat(paste(p,"-",cnames[p]), "\n")
    }
    n = readline(prompt="please type the number for the correct match, or a zero if there are no matches: ")
    idi=as.numeric(n)
  }else{
    idi=which(cnames==var.name)
    if((length(idi)==0 | always.choose) & !strict.search){
      cat(paste0("No variable called ", var.name, ", or choosing is enforced (always.choose = TRUE)\n"))
      for(i in 1:(length(alt.names)+1)){
        if(i==1){
          test = grepl(pattern = var.name,cnames,ignore.case = TRUE)
        }else{
          test = (grepl(pattern = alt.names[i-1],cnames,ignore.case = TRUE) | test)
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
        cat(paste("Multiple possible matches for",var.name), "\n")
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
#' @param time.1 matrix of age/time ensembles, or single column
#' @param values.1 matrix of values ensembles, or single column
#' @param time.2 matrix of age/time ensembles, or single column
#' @param values.2 matrix of values ensembles, or single column
#' @param bin.vec vector of bin edges for binning step
#' @param bin.step spacing of bins, used to build bin step
#' @param bin.fun function to use during binning (mean, sd, and sum all work)
#' @param max.ens maximum number of ensemble members to regress
#' @param min.obs minimum number of points required to calculate regression
#' @return list of binned data output:
#' \itemize{
#' \item binX: binned values from X
#' \item binY: binned values from Y
#' \item bin.step: interval of the binning
#' \item yearBins: bins along time
#' }
#' @author Nick McKay
alignTimeseriesBin = function(time.1,values.1,time.2,values.2,bin.vec = NA,bin.step = NA ,bin.fun=mean,max.ens=NA,min.obs=10){
  #check to see if time and values are "column lists"
  if(is.list(time.1)){
    otx=time.1
    time.1=time.1$values}
  if(is.list(time.2)){
    oty=time.2
    time.2=time.2$values}
  if(is.list(values.1)){
    ovx=values.1
    values.1=values.1$values}
  if(is.list(values.2)){
    ovy=values.2
    values.2=values.2$values}
  
  
  #make them all matrices
  time.1 = as.matrix(time.1)
  time.2 = as.matrix(time.2)
  values.1 = as.matrix(values.1)
  values.2 = as.matrix(values.2)
  
  if(nrow(time.1) != nrow(values.1)){stop("time.1 and values.1 must have the same number of rows (observations)")}
  if(nrow(time.2) != nrow(values.2)){stop("time.2 and values.2 must have the same number of rows (observations)")}
  
  if(all(is.na(bin.vec))){
    if(is.na(bin.step)){
      stop("Either a bin.vec or bin.step must be specified")
    }else{
      #look for common overlap
      binStart=max(c(min(time.1,na.rm=TRUE),min(time.2,na.rm=TRUE)))
      binStop=min(c(max(time.1,na.rm=TRUE),max(time.2,na.rm=TRUE)))
      bin.vec=seq(binStart,binStop,by=bin.step)
    }
  }
  
  #create ensemble bins
  dum = binEns(time = time.1,values = values.1,bin.vec = bin.vec,bin.fun=bin.fun,max.ens=max.ens)
  yearX = dum$time
  binX = dum$matrix
  binY = binEns(time = time.2,values = values.2,bin.vec = bin.vec,bin.fun=bin.fun,max.ens=max.ens)$matrix
  
  #remove columns that have less than min.obs datapoints
  good = which(apply(!is.na(binX),2,sum)>=min.obs)
  if(length(good)==0){
    stop(paste("none of the columns have",min.obs,"or more datapoints"))
  }
  binX = as.matrix(binX[,good])
  
  
  good = which(apply(!is.na(binY),2,sum)>=min.obs)
  if(length(good)==0){
    stop(paste("none of the columns have",min.obs,"or more datapoints"))
  }
  binY = as.matrix(binY[,good])
  
  
  if(is.na(bin.step)){#if the bin.step isn't specified
    bin.step=abs(mean(diff(bin.vec,na.rm=TRUE)))
  }
  
  return(list(binX = binX, binY=binY,bin.step=bin.step,yearBins = yearX))
}
