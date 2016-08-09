load.Bacon.output.LiPD = function(L,site.name=L$dataSetName,which.chron=NA,baconDir=NA,modelNum=NA,makeNew=NA){
  #initialize bacon directory
  if(is.na(baconDir)){
    cat("please select your Bacon.R file","\n")
    baconFile=file.choose()
    baconDir=dirname(baconFile)
  }
  
  cur.dir = getwd()
  #see if there's an appropriate folder.
  setwd(baconDir)
  setwd("Cores")
  if(!any(grepl(site.name,dir()))){
    print(paste0("can't find a directory called",site.name))
    return(L)
  }
  
  
  
  #initialize which.chron
  if(is.na(which.chron)){
    if(length(L$chronData)==1){
      which.chron=1
    }else{
      which.chron=as.integer(readline(prompt = "Which chronData do you want to run Bacon for? "))
    }
  }
  
  
  
  #initialize model number
  if(is.na(modelNum)){
    if(is.null(L$chronData[[which.chron]]$chronModel[[1]])){
      #no models, this is first
      modelNum=1
    }else{
      print(paste("You already have", length(L$chronData[[which.chron]]$chronModel), "chron model(s) in chronData" ,which.chron))
      modelNum=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  if(is.na(makeNew)){
    makeNew = FALSE
  }
  
  if(length(L$chronData[[which.chron]]$chronModel)<modelNum){
    if(makeNew){
      L$chronData[[which.chron]]$chronModel[[modelNum]]=NA
    }else{
      nm=readline(prompt = paste("model",modelNum,"doesn't exist. Create it? y or n "))
      if(grepl(pattern = "y",x = tolower(nm))){
        L$chronData[[which.chron]]$chronModel[[modelNum]]=NA
      }else{
        stop("Stopping, since you didn't want to create a new model")
      }
    }
  }
  
  
  
  
  #grab methods first
  #   setwd(baconDir)
  #   setwd("Cores")
  setwd(site.name)
  sf=dir(pattern="*settings.txt")
  if(length(sf)!=1){
    cat("select the settings.txt file","\n")
    sf=file.choose()
  }
  bacMethods=read.delim(sf,sep="#",header = FALSE)
  m=bacMethods[,1]
  names(m)=bacMethods[,2]  
  parameters= list()
  keys = names(m)
  for(mi in 1:length(m)){
    parameters[[keys[mi]]]=as.character(m[mi])
  }
  methods = list("parameters"=parameters)
  methods$algorithm = "Bacon"
  methods$version = 2.2
  
  
  
  if(is.na(L$chronData[[which.chron]]$chronModel[[modelNum]])){
    L$chronData[[which.chron]]$chronModel[[modelNum]]=list("methods"=methods)
    
  }else{
    L$chronData[[which.chron]]$chronModel[[modelNum]]$methods=methods
  }
  
  #summary table!
  st=dir(pattern="*ages.txt")
  if(length(st)!=1){
    cat("select the correct ages.txt file","\n")
    st=file.choose()
  }
  K=as.numeric(regmatches(st, gregexpr("[0-9]*?(?=\\_ages.txt)", st, perl=T))[[1]])[1]
  summTable = read.table(st,header = TRUE)
  
  #assign names in.
  origNames = c("depth","min","max","median","wmean")
  newNames = c("depth","ageRangeLow","ageRangeHigh","age","age")
  
  depthUnits = L$chronData[[which.chron]]$chronMeasurementTable[[1]]$depth$units
  if(is.null(depthUnits)){
    depthUnits="cm"
  }
  units = c("cm", "yr BP","yr BP","yr BP","yr BP")
  description = c("modeled depth","low end of the uncertainty range","high end of the uncertainty range","median age estimate","weighted mean age estimate")
  uncertaintyLevel = c(NA,"2.5%","97.5%",NA,NA)
  
  summaryTable = list()
  for(n in 1:length(origNames)){
    summaryTable[[newNames[n]]]$variableName = newNames[n]
    summaryTable[[newNames[n]]]$values = summTable[,origNames[n]]
    summaryTable[[newNames[n]]]$units = units[n]
    summaryTable[[newNames[n]]]$description = description[n]
    if(!is.na(uncertaintyLevel[n])){
      summaryTable[[newNames[n]]]$uncertaintyLevel = uncertaintyLevel[n]
    }
  }
  
  L$chronData[[which.chron]]$chronModel[[modelNum]]$summaryTable=summaryTable
  #
  
  
  
  
  #now grab ensemble data.
  ageEns = sampleBaconAgesLiPD(corename=site.name,baconDir = baconDir)
  
  ageEns$depth$units = L$chronData[[which.chron]]$chronModel[[modelNum]]$summaryTable$depth$units
  ageEns$ageEnsemble$units = L$chronData[[which.chron]]$chronModel[[modelNum]]$summaryTable$age$units
  
  L$chronData[[which.chron]]$chronModel[[modelNum]]$ensembleTable = ageEns
  
  
  #TBD - grab probability distribution data
  #possible in bacon?
  
  
  return(L)
  
  
  
  
  
  
  
  
  
  
  
}
