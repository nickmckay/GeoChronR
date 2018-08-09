#' @export
#' @author Nick McKay
#' @title Get the name of bacon core directory
#' @description This is a mostly internal function that returns the core directory. Interactive if given a NA
#' @param baconDir if you already have it, it just returns this, (default=NA)
#' @return baconDir the bacon core directory
getBaconDir <- function(baconDir = NA){
  #initialize bacon directory
  if(is.na(baconDir)){
    #check global first
    if(!exists("baconDir",where = .GlobalEnv)){
      cat('please select the "MSB2K.csv" file inside your "Bacon_runs" or "Cores" directory',"\n")
      baconFile=file.choose()
      baconDir<<-dirname(dirname(baconFile))
      baconDir=baconDir
    }else{
      baconDir=get("baconDir",envir = .GlobalEnv)
      if(is.na(baconDir)){
        cat('please select the "MSB2K.csv" file inside your "Bacon_runs" or "Cores" directory',"\n")
        baconFile=file.choose()
        baconDir<<-dirname(dirname(baconFile))
        baconDir=baconDir
      }
    }
  }
  return(baconDir)
}


#' @export
#' @author Nick McKay
#' @author Maarten Blaauw (Bacon)
#' @family Bacon
#' @import rbacon
#' @title Generate a Bayesian Reconstruction Age Model  (Bacon) and add it into a LiPD object
#' @description This is a high-level function that uses Bacon to simulate an age model, and stores this as an age-ensemble in a model in chronData. If needed input variables are not entered, and cannot be deduced, it will run in interactive mode. See Blaauw and Christen (2011) doi:10.1214/11-BA618 for details.
#' @inheritParams writeBacon
#' @param maxEns the maximum number of ensembles to load in (default = 1000)
#' @return L The single LiPD object that was entered, with methods, ensembleTable, summaryTable and distributionTable added to the chronData model.
#' @examples 
#' Run in interactive mode:
#' L = runBacon(L)
#' 
#' Run in noninteractive mode, describing everything:
#' L = runBacon(L,which.chron = 1, which.mt = 1, modelNum = 3, baconDir = "~/Bacon/",site.name = "MSB2K", cc = 1)
runBacon <-  function(L,which.chron=NA,which.mt = NA,baconDir=NA,site.name=L$dataSetName,modelNum=NA,remove.rejected=TRUE,overwrite=TRUE,cc=NA,maxEns = 1000,useMarine = NULL,labIDVar="labID", age14CVar = "age14C", age14CuncertaintyVar = "age14CUnc", ageVar = "age",ageUncertaintyVar = "ageUnc", depthVar = "depth", reservoirAge14CVar = "reservoirAge",reservoirAge14CUncertaintyVar = "reservoirAge14C",rejectedAgesVar="rejected",BaconAsk = TRUE,BaconSuggest = TRUE,baconThick = NA,baconAccMean = 20){
  cur.dir = getwd()
  #initialize which.chron
  if(is.na(which.chron)){
    if(length(L$chronData)==1){
      which.chron=1
    }else{
      which.chron=as.integer(readline(prompt = "Which chronData do you want to run bacon for? "))
    }
  }
  
  
  #get bacon directory
  baconDir <- getBaconDir(baconDir)
  
  
  
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
  
  
  #write bacon file
  L=writeBacon(L,which.chron = which.chron,which.mt = which.mt,baconDir = baconDir,remove.rejected = remove.rejected,site.name = site.name,overwrite = overwrite,cc=cc,modelNum = modelNum,useMarine = useMarine, labIDVar=labIDVar, age14CVar = age14CVar, age14CuncertaintyVar = age14CuncertaintyVar, ageVar = ageVar,ageUncertaintyVar = ageUncertaintyVar, depthVar = depthVar, reservoirAge14CVar = reservoirAge14CVar,reservoirAge14CUncertaintyVar = reservoirAge14CUncertaintyVar,rejectedAgesVar=rejectedAgesVar)
  
  #estimate thickness parameter
  if(is.na(baconThick)){
  thick = abs(diff(range(L$chronData[[which.chron]]$model[[modelNum]]$inputTable[,4])))/100
  }else{
    thick = baconThick
  }

  #run bacon
  setwd(baconDir)
  #if(is.null(baconFile)){baconFile = "Bacon.R"}
  
  #source(baconFile)
  rbacon::Bacon(core=site.name,coredir = baconDir,thick=thick,ask = BaconAsk,acc.mean = baconAccMean,suggest = BaconSuggest)
  
  print("taking a short break...")
  Sys.sleep(5)
  #pull bacon data into lipd structure
  L = loadBaconOutput(L,site.name=L$dataSetName,which.chron=which.chron,baconDir=baconDir,modelNum=modelNum,maxEns = maxEns)
  return(L)
}

#' @export
#' @title Sample ensemble ages from Bacon
#' @description Pulls ensemble members from Bacon output. Will be run in interactive mode if necessary parameters aren't specified. Most users will want to use runBacon for their bacon needs. 
#' @family Bacon
#' @author Simon Goring
#' @author Nick McKay
#' @param corename the name used for the bacon model (and directories)
#' @param K the number of intervals over which the model is run, this is appended onto all the Bacon files after the underscore. If NA, will attempt to deduce from the directory.
#' @param baconDir the directory where Bacon is installed on this computer. Willimport if bossible. 
#' @param maxEns the maximum number of ensemble members to import
#' @return An ensemble table in the LiPD structure
#' @examples 
#' ensTable = sampleBaconAges("MSB2K",maxEns = 1000)
#' 
sampleBaconAges <- function(corename,K=NA,baconDir=NA,maxEns=NA){
  #from Simon Goring, modified by Nick McKay
  
  #get bacon directory
  baconDir <- getBaconDir(baconDir)
  
  
  setwd(baconDir)
  setwd(corename)
  
  
  if(is.na(K)){
    t=dir(pattern=".bacon")
    #To do? pick from multiple? 
    K=as.numeric(regmatches(t[1], gregexpr("[0-9]*?(?=\\.bacon)", t[1], perl=TRUE))[[1]])[1]
  }
  
  out.file=read.table(paste0(corename,'_',as.character(K),'.out'))
  
  #get start and end depths
  bfname=paste0(corename,'_',as.character(K),'.bacon')
  
  
  sline=1
  while(sline<10000){
    bacData=try(read.table(bfname,skip=sline,sep=c(",",";")),silent=TRUE)
    if(!is.character(bacData)){
      if(strsplit(levels(bacData$V1)," ")[[1]][1]=="Bacon"){
        break
      }else{
        sline=sline+1
      }
    }else{
      sline=sline+1
    }
  }
  end.depth=as.numeric(gsub(levels(bacData$V12),pattern=";",replacement=""))
  start.depth=bacData$V11
  Dc=(end.depth-start.depth)/(K-1)
  depths=seq(start.depth,end.depth,by=Dc)
  
  
  #out_file <- out.file[,1:(ncol(out.file)-2)]
  
  
  
  BACages = kronecker(matrix(1,1,K),out.file[,1])+t(Dc*apply(out.file[,2:(K+1)],1,cumsum))
  BACages = cbind(out.file[,1],BACages[,-ncol(BACages)])
  
  
  
  if(is.na(maxEns)){maxEns=nrow(BACages)}
  
  ages.out <- plyr::laply(1:min(nrow(BACages),maxEns), function(x){approx(x=depths, 
                                                                          y = BACages[x,], 
                                                                          xout=depths)$y})
  
  ensembleTable=list()
  ensembleTable$depth$values = depths
  ensembleTable$depth$variableName = "depth"
  
  
  ensembleTable$ageEnsemble$values = t(ages.out)
  ensembleTable$ageEnsemble$variableName = "ageEnsemble"
  
  
  return(ensembleTable)
}

#' @export
#' @author Nick McKay
#' @title Create the input file for a Bacon model from a LiPD object
#' @description This generates the csv file that is used for input to Bacon. Will be run in interactive mode if necessary parameters aren't specified. Most users will want to use runBacon for their bacon needs. 
#' @family Bacon
#' @param L a single LiPD object
#' @param which.chron the number of the chronData object that you'll be working in
#' @param which.mt the number of the measurementTable you'll be working in
#' @param baconDir the directory where Bacon is installed on this computer.
#' @param remove.rejected don't write out dates that are marked as rejected
#' @param overwrite overwrite files and directories
#' @param cc An integer, or vector of integers corresponding to age that describes the calibration curve. You can specify here (see below) or if it's NA the code will guess based on archiveType
#' \itemize{
#' \item cc=1 IntCal13
#' \item cc=2 MarineCal 
#' \item cc=3 SHCal13
#' }
#' @param site.name the name used for the bacon model (and directories)
#' @param modelNum which chronModel do you want to use?
#' @return L the input LiPD file with methods added to the chronModel.
#' @examples 
#' writeBacon(L)
#' #Run in interactive mode
#' 
#' writeBacon(L,which.chron=1,which.mt = 1,baconDir="~/Bacon/",remove.rejected=TRUE,overwrite=TRUE,cc=NA,site.name=L$dataSetName,modelNum=NA)
writeBacon <-  function(L,which.chron=NA,which.mt = NA,baconDir=NA,remove.rejected=TRUE,overwrite=TRUE,cc=NA,site.name=L$dataSetName,modelNum=NA,useMarine = NULL,labIDVar="labID",
                        age14CVar = "age14C", age14CuncertaintyVar = "age14CUnc", ageVar = "age", 
                        ageUncertaintyVar = "ageUnc", depthVar = "depth", reservoirAge14CVar = "reservoirAge",
                        reservoirAge14CUncertaintyVar = "reservoirAge14C",rejectedAgesVar="rejected"){
  cur.dir = getwd()
  #initialize which.chron
  if(is.na(which.chron)){
    if(length(L$chronData)==1){
      which.chron=1
    }else{
      which.chron=as.integer(readline(prompt = "Which chronData do you want to run bacon for? "))
    }
  }
  if(is.na(modelNum)){
    if(is.null(L$chronData[[which.chron]]$model[[1]])){
      #no models, this is first
      modelNum=1
    }else{
      print(paste("You already have", length(L$chronData[[which.chron]]$model), "chron model(s) in chronData" ,which.chron))
      modelNum=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  
  #get bacon directory
  baconDir <- getBaconDir(baconDir)
  
  #pull out chronology
  
  
  C=L$chronData[[which.chron]]
  
  #check for measurementTables
  if(is.na(which.mt)){
    if(length(C$measurementTable)==1){
      which.mt = 1  
    }else{
      print(paste("There are", length(L$chronData[[which.chron]]$measurementTable), "measurement tables in chronData " ,which.chron))
      which.mt=as.integer(readline(prompt = "Which do you want to use here? (Enter an integer)"))
    }
  }
  
  MT=C$measurementTable[[which.mt]]
  
  
  #go through required fields for bacon
  
  #labID
  print("Looking for laboratory ID...")
  idi = getVariableIndex(MT,labIDVar,altNames = "id")
  if(is.na(idi)){
    labIDVar <- NULL
  }else{
    labIDVar <- MT[[idi]]$variableName
  }
  
  
  #14C age
  print("Looking for radiocarbon ages...")
  c14i = getVariableIndex(MT,age14CVar,altNames = "age")
  if(is.na(c14i)){
    age14CVar <- NULL
  }else{
    age14CVar <- MT[[c14i]]$variableName
  }
  
  
  #14C age uncertainty
  print("Looking for radiocarbon age uncertainty...")
  c14unci = getVariableIndex(MT,age14CuncertaintyVar,altNames = c("age","uncertainty"))
  if(is.na(c14unci)){
    age14CuncertaintyVar <- NULL
  }else{
    age14CuncertaintyVar <- MT[[c14unci]]$variableName
  }
  
  #age (calibrated)
  print("Looking for calibrated ages...")
  agei = getVariableIndex(MT,ageVar,altNames = "age")
  if(is.na(agei)){
    ageVar <- NULL
  }else{
    ageVar <- MT[[agei]]$variableName
  }
  
  #age uncertainty (calibrated)
  print("Looking for calibrated age uncertainty...")
  ageunci = getVariableIndex(MT,ageUncertaintyVar,altNames = c("age","uncertainty"))
  if(is.na(ageunci)){
    ageUncertaintyVar <- NULL
  }else{
    ageUncertaintyVar <- MT[[ageunci]]$variableName
  }
  
  #depth
  print("Looking for depth...")
  depthi = getVariableIndex(MT,depthVar)
  if(is.na(depthi)){
    depthVar <- NULL
  }else{
    depthVar <- MT[[depthi]]$variableName
  }
  
  #reservoir age
  print("Looking for radiocarbon reservoir age offsets (deltaR)...")
  print("can also use radiocarbon reservoir ages if need be...")
  resi = getVariableIndex(MT,reservoirAge14CVar,altNames = "reservoir")
  if(is.na(resi)){
    reservoirAge14CVar <- NULL
  }else{
    reservoirAge14CVar <- MT[[resi]]$variableName
  }
  
  #reservoir uncertainty
  print("Looking for radiocarbon reservoir age uncertainties...")
  resUnci = getVariableIndex(MT,reservoirAge14CUncertaintyVar,altNames = c("reservoir","unc"))
  if(is.na(resUnci)){
    reservoirAge14CUncertaintyVar <- NULL
  }else{
    reservoirAge14CUncertaintyVar <- MT[[resUnci]]$variableName
  }
  
  #rejected ages
  print("Looking for column of reject ages, or ages not included in age model")
  rejeci = getVariableIndex(MT,rejectedAgesVar,altNames = c("reject","ignore"))
  if(is.na(rejeci)){
    rejectedAgesVar <- NULL
  }else{
    rejectedAgesVar <- MT[[rejeci]]$variableName
  }
  
  stringifyVariables <- function(varName){
    if(is.null(varName)){
      so <- paste0(deparse(substitute(varName))," = NULL")
    }else{
      so <- paste0(deparse(substitute(varName))," = '", varName, "'")
    }
    return(so)
}
  
  #print results...
  print("Variable choices for reuse...")
  varUsedStr <- paste0(stringifyVariables(labIDVar),", ", stringifyVariables(age14CVar),", ", stringifyVariables(age14CuncertaintyVar),", ", stringifyVariables(ageVar),", ", 
               stringifyVariables(ageUncertaintyVar),", ", stringifyVariables(depthVar),", ", stringifyVariables(reservoirAge14CVar),", ", stringifyVariables(reservoirAge14CUncertaintyVar),", ", stringifyVariables(rejectedAgesVar))
  print(varUsedStr)
  assign("bacon_varUsedStr",value = varUsedStr,envir = .GlobalEnv)

  
  
  #merge variables as needed
  depth <- MT[[depthi]]$values
  if(is.null(depth)){
    stop("No depth. Depth is required by bacon")
  }
  
  
  #ages in uncertainties. Assign calibrated ages when 14C ages are empty
  #14C age
  cage <- MT[[c14i]]$values
  if(is.null(cage)){
    cage <- rep(NA,len=length(depth))
  }
  
  #calibrated age
  calage <- MT[[agei]]$values
  if(is.null(calage)){
    calage <- rep(NA,len=length(depth))
  }
  
  age <- cage
  which.calage <- which(is.na(cage) & !is.na(calage))
  age[which.calage] <- calage[which.calage]
  
  
  #load in uncertainties. Assign calibrated uncertainties when 14C uncertainty is empty
  error <- MT[[c14unci]]$values
  if(is.null(error)){
    error <- rep(NA,len=length(depth))
  }
  
  calageerror <-  MT[[ageunci]]$values
  if(is.null(calageerror)){
    calageerror <- rep(NA,len=length(depth))
  }
  
  error[which.calage] <- calageerror[which.calage]
  
  reservoir <- MT[[resi]]$values
  if(is.null(reservoir)){
    reservoir <- rep(NA,len=length(depth))
  }else{
    print("bacon uses delta-R: deviation from the reservoir curve")
    
    print("If your data are in abolute reservoir years, you probably want to subtract a reservoir estimate (400 yr) from your data")
    print("Take a look at the values")
    print(head(reservoir))
    R=as.numeric(readline(prompt = "enter the number of years you'd like to subtract from your data (0 leaves the data unchanged of course) "))
    reservoir=reservoir-R
  }
  
  
  reservoir_error <- MT[[resUnci]]$values
  if(is.null(reservoir_error)){
    reservoir_error <- rep(NA,len=length(depth))
  }
  
  rejected <- MT[[rejeci]]$values
  if(is.null(rejected)){
    rejected <- rep(NA,len=length(depth))
  }
  
  id <- MT[[idi]]$values
  if(is.null(idi)){
    id <- rep(NA,len=length(depth))
  }
  
  
  #Now build the table
  
  
  #nrows <- NROW(cbind(id,age,error,reservoir,depth))
  nrows <- length(depth)
  print(nrows)
  ta <- rep(3,len=nrows)
  tb <- rep(4,len=nrows)
  if(is.na(cc)){
    if(any(names(L)=="archiveType")){
      if(grepl("marine",tolower(L$archiveType))){#assume a marine cal curve if it's marine
        cc <- rep(2,len=nrows)
      }else{
        cc <- rep(1,len=nrows)
      }
    }else{
      if(is.null(useMarine)){
        useMarine = readline(prompt = "Do you want to use the Marine13 curve?")
      }
      if(grepl(useMarine,pattern = "y")){
        cc <- rep(2,len=nrows)
        
      }else{
        cc <- rep(1,len=nrows)
      } 
      
      
    }
  }else{# force it to be what was prescribed
    cc <- rep(cc,len=nrows)
    
  }
  
  #for calibrated ages, use a gaussian and no calibration
  ta[which.calage] <- 33
  tb[which.calage] <- 34
  cc[which.calage] <- 0
  
  
  out.table <- as.data.frame(matrix(NA,nrow=nrows,ncol=9))
  if(!is.null(id)){out.table[,1] <- id[1:nrows]}
  
  if(!is.null(age)) {
    
    out.table[,2] <- age}
  if(!is.null(error) ) {
    out.table[,3] <- error}
  
  
  if(!is.null(depth) ) {
    
    out.table[,4] <- depth}
  
  if(!is.null(cc) ) {
    out.table[,5] <- cc}
  
  if(!is.null(reservoir) ) {
    out.table[,6] <- reservoir}else
    {out.table[,6] <- 0}
  
  if(!is.null(reservoir_error) ) {
    out.table[,7] <-reservoir_error}else
    {out.table[,7] <- 0}
  
  if(!is.null(ta) ) {
    out.table[,8] <-ta}
  if(!is.null(tb) ) {
    out.table[,9] <-tb}
  
  
  #replace NAs appropriately
  out.table[is.na(out.table[,1]),1] <- "unknown"
  out.table[is.na(out.table[,3]),3] <- 1
  out.table[is.na(out.table[,6]),6] <- 0
  out.table[is.na(out.table[,7]),7] <- 0
  out.table[is.na(out.table[,8]),8] <- 0
  out.table[is.na(out.table[,9]),9] <- 0
  
  out.table <- na.omit(out.table)
  
  colnames(out.table) <- c("id","age","error","depth","cc","dR","dSTD","ta","tb")
  
  which.rejected <- which(!is.na(rejected))
  
  if(remove.rejected & length(which.rejected)>1){
    out.table <- out.table[-which.rejected,]
  }
  
  print(out.table)
  
  
  #check for core directory
  setwd(baconDir)
  et <- dir()
  write=FALSE
  if(any(site.name == et)){
    writedir=FALSE
    if(overwrite){
      write=TRUE
    }
  }else{
    writedir=TRUE
    write=TRUE
  }
  
  if(writedir){
    setwd(baconDir)
    
    dir.create(site.name)
  }
  if(write){
    setwd(baconDir)
    
    setwd(site.name)
    write.csv(out.table,paste0(site.name,".csv"),row.names=FALSE)
    
  }
  
  setwd(cur.dir)
  if(modelNum>length(L$chronData[[which.chron]]$model)){
    L$chronData[[which.chron]]$model[[modelNum]]=list(inputTable = out.table)
  }
  return(L)
  
  
}

#' @export
#' @author Nick McKay
#' @title Load the ensemble, summary and distribution data from a Bacon simulation
#' @description Loads the ensemble, summary and distribution data from a Bacon simulation and stores them in the LiPD structure. Will be run in interactive mode if necessary parameters aren't specified. Most users will want to use runBacon for their bacon needs. 
#' @family Bacon
#' @param L a single LiPD object
#' @param which.chron the number of the chronData object that you'll be working in
#' @param baconDir the directory where Bacon is installed on this computer.
#' @param site.name the name used for the bacon model (and directories)
#' @param modelNum which chronModel do you want to use?
#' @param makeNew do you want to create a new model in chronData? (TRUE, FALSE, NA). NA will try be smart, or ask you for advice.
#' @return L the input LiPD file with methods and data added to the chronModel.
#' @examples 
#' loadBaconOutput(L)
#' #Run in interactive mode
loadBaconOutput = function(L,site.name=L$dataSetName,which.chron=NA,baconDir=NA,modelNum=NA,makeNew=NA,maxEns = 1000){
  
  #get bacon directory
  baconDir <- getBaconDir(baconDir)
  
  
  cur.dir = getwd()
  #see if there's an appropriate folder.
  setwd(baconDir)
  
  
  if(!any(grepl(site.name,dir()))){
    print(paste0("can't find a directory called",site.name))
    return(L)
  }
  
  
  
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
  
  
  
  
  #grab methods first
  setwd(baconDir)
  
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
  methods$algorithm = "bacon"
  methods$version = 2.3
  
  
  
  if(is.na(L$chronData[[which.chron]]$model[[modelNum]])){
    L$chronData[[which.chron]]$model[[modelNum]]=list("methods"=methods)
    
  }else{
    L$chronData[[which.chron]]$model[[modelNum]]$methods=methods
  }
  
  #summary table!
  st=dir(pattern="*ages.txt")
  if(length(st)!=1){
    cat("select the correct ages.txt file","\n")
    st=file.choose()
  }
  
  
  K=as.numeric(regmatches(st, gregexpr("[0-9]*?(?=\\_ages.txt)", st, perl=TRUE))[[1]])[1]
  summTable = read.table(st,header = TRUE)
  
  #assign names in.
  origNames = c("depth","min","max","median","mean")
  newNames = c("depth","ageRangeLow","ageRangeHigh","age","age")
  
  depthUnits = L$chronData[[which.chron]]$measurementTable[[1]]$depth$units
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
  
  L$chronData[[which.chron]]$model[[modelNum]]$summaryTable[[1]]=summaryTable
  #
  
  #now grab ensemble data.
  ageEns = sampleBaconAges(corename=site.name,baconDir = baconDir,K = K,maxEns = maxEns)
  
  ageEns$depth$units = L$chronData[[which.chron]]$model[[modelNum]]$summaryTable[[1]]$depth$units
  ageEns$ageEnsemble$units = L$chronData[[which.chron]]$model[[modelNum]]$summaryTable[[1]]$age$units
  
  L$chronData[[which.chron]]$model[[modelNum]]$ensembleTable[[1]] = ageEns
  
  if(exists("info")){
    #grab distribution data
    for(dd in 1:length(info$calib$probs)){
      dTable = list()
      dTable$age = list(values = info$calib$probs[[dd]][,1], units =  L$chronData[[which.chron]]$model[[modelNum]]$summaryTable[[1]]$age$units, variableName = "age")
      dTable$probabilityDensity = list(values = info$calib$probs[[dd]][,2], variableName = "probabilityDensity")
      dTable$depth = info$calib$d[dd]
      dTable$depthUnits = L$chronData[[which.chron]]$model[[modelNum]]$summaryTable[[1]]$depth$units
      
      L$chronData[[which.chron]]$model[[modelNum]]$distributionTable[[dd]] = dTable
    }
  }
  
  
  return(L)
}

