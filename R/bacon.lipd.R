#create an environment for package variables
geoChronREnv <- new.env()

#' @export
#' @author Nick McKay
#' @title Get the name of bacon core directory
#' @description This is a mostly internal function that returns the core directory. Interactive if given a NA
#'
#' @param bacon.dir if you already have it, it just returns this, (default=NA)
#' @param overwrite.tmp.dir Erase if using a tempdir before running?
#' @return bacon.dir the bacon core directory
#' @family Bacon
getBaconDir <- function(bacon.dir = NA,overwrite.tmp.dir = TRUE){
  #initialize bacon directory
  if(is.na(bacon.dir) | !is.character(bacon.dir)){
    #check geoChronR env first
    if(!exists("bacon.dir",where = geoChronREnv)){
      bacon.dir <- file.path(tempdir(),"bacon")
    }else{
      bacon.dir=get("bacon.dir",envir = geoChronREnv)
      if(is.na(bacon.dir) | !is.character(bacon.dir)){
        bacon.dir <- tempdir()
      }
    }
  }
  
  if(bacon.dir == file.path(tempdir(),"bacon") & overwrite.tmp.dir){
    unlink(bacon.dir)
  }
  
  #create the directory if it doesn't exist
  if(!dir.exists(bacon.dir)){
    dir.create(bacon.dir)
  }
  
  return(bacon.dir)
}

#' @export
#' @author Nick McKay
#' @title Set the name of bacon core directory
#' @description Use this to programmatically set the bacon.dir 
#' @param bacon.dir if you already have it
#' @family Bacon
#' @return bacon.dir the bacon core directory
setBaconDir <- function(bacon.dir){
      assign("bacon.dir",value = bacon.dir,envir = geoChronREnv)
      print(paste0("bacon.dir set to ",bacon.dir))
}

#' @export
#' @author Nick McKay
#' @author Maarten Blaauw (Bacon)
#' @family Bacon
#' @import rbacon
#' @title Generate a Bayesian Reconstruction Age Model  (Bacon) and add it into a LiPD object
#' @description This is a high-level function that uses Bacon to simulate an age model, and stores this as an age-ensemble in a model in chronData. If needed input variables are not entered, and cannot be deduced, it will run in interactive mode. See Blaauw and Christen (2011) doi:10.1214/11-BA618 for details.
#' @inheritParams writeBacon
#' @param max.ens the maximum number of ensembles to load in (default = 1000)
#' @param bacon.thick thickness parameter to pass to bacon (How thick is each chunk to model)
#' @param bacon.acc.mean prior mean accumulation rate estimate for bacon
#' @inheritDotParams rbacon::Bacon
#' @inheritParams createChronMeasInputDf
#' @return L The single LiPD object that was entered, with methods, ensembleTable, summaryTable and distributionTable added to the chronData model.
#' @examples 
#' \dontrun{
#' #Run in interactive mode:
#' L = runBacon(L)
#' 
#' #Run in noninteractive mode, describing everything:
#' L = runBacon(L,chron.num = 1, meas.table.num = 1, model.num = 3, bacon.dir = "~/Bacon/",site.name = "MSB2K", cc = 1)
#' }
#' @section Long-form example:
#' \href{http://nickmckay.github.io/GeoChronR/articles/Introduction.html}{View a full-fledged example of how to use this function.}
runBacon <-  function(L,
                      chron.num=NA,
                      meas.table.num = NA,
                      bacon.dir=NA,
                      site.name=L$dataSetName,
                      model.num=NA,
                      remove.rejected=TRUE,
                      overwrite=TRUE,
                      cc=NA,
                      max.ens = 1000,
                      use.marine = NULL,
                      lab.id.var="labID",
                      age.14c.var = "age14C", 
                      age.14c.uncertainty.var = "age14CUnc", 
                      age.var = "age",
                      age.uncertainty.var = "ageUnc", 
                      depth.var = "depth", 
                      reservoir.age.14c.var = "reservoirAge",
                      reservoir.age.14c.uncertainty.var = "reservoirAge14C",
                      rejected.ages.var="rejected",
                      ask.reservoir = TRUE,
                      bacon.thick = NA,
                      bacon.acc.mean = NA,
                      ...){
  
  
  
  cur.dir = getwd()
  
  
  #initialize chron.num
  if(is.na(chron.num)){
    if(length(L$chronData)==1){
      chron.num=1
    }else{
      chron.num=as.integer(readline(prompt = "Which chronData do you want to run bacon for? "))
    }
  }
  
  
  #get bacon directory
  bacon.dir <- getBaconDir(bacon.dir)
  
  
  
  #initialize model number
  if(is.na(model.num)){
    if(is.null(L$chronData[[chron.num]]$model[[1]])){
      #no models, this is first
      model.num=1
    }else{
      print(paste("You already have", length(L$chronData[[chron.num]]$model), "chron model(s) in chronData" ,chron.num))
      print(paste("If you want to create a new model, enter", length(L$chronData[[chron.num]]$model)+1))
      
      model.num=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
#  writeBacon <-  function(L,bacon.dir=NA,remove.rejected=TRUE,overwrite=TRUE,cc=NA,site.name=L$dataSetName,model.num=NA,use.marine = NULL,...){
  #write bacon file
  L=writeBacon(L,
               bacon.dir = bacon.dir, 
               chron.num = chron.num,
               remove.rejected = remove.rejected,
               site.name = site.name,
               overwrite = overwrite,
               cc=cc,
               model.num = model.num,
               use.marine = use.marine,
               meas.table.num = meas.table.num,
               lab.id.var=lab.id.var,
               age.14c.var = age.14c.var, 
               age.14c.uncertainty.var = age.14c.uncertainty.var, 
               age.var = age.var,age.uncertainty.var = age.uncertainty.var, 
               depth.var = depth.var, 
               reservoir.age.14c.var = reservoir.age.14c.var,
               reservoir.age.14c.uncertainty.var = reservoir.age.14c.uncertainty.var,
               rejected.ages.var=rejected.ages.var,
               ask.reservoir = ask.reservoir)
  
  totalDepth <- abs(diff(range(L$chronData[[chron.num]]$model[[model.num]]$inputTable[,4])))
  totalAge <- abs(diff(range(L$chronData[[chron.num]]$model[[model.num]]$inputTable[,2])))
  
  #estimate thickness parameter
  if(is.na(bacon.thick)){
    thick <- totalDepth/100
    K <- 100
  }else{
    thick <- bacon.thick
    K <- ceiling(ceiling(totalDepth)/thick)
  }
  
  #estimate acc mean
  #estimate thickness parameter
  if(is.na(bacon.acc.mean)){
    bacon.acc.mean <- totalAge/totalDepth
  }
  
  #run bacon
  setwd(bacon.dir)
  #if(is.null(baconFile)){baconFile = "Bacon.R"}
  
  #run Bacon
  rbacon::Bacon(core=site.name,coredir = bacon.dir,thick=thick,acc.mean = bacon.acc.mean,...)

  print("taking a short break...")
  Sys.sleep(5)
  #pull bacon data into lipd structure
  L = loadBaconOutput(L,
                      site.name=L$dataSetName,
                      K = K, 
                      chron.num=chron.num,
                      bacon.dir=bacon.dir,
                      model.num=model.num,
                      max.ens = max.ens)
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
#' @param bacon.dir the directory where Bacon is installed on this computer. Willimport if bossible. 
#' @param max.ens the maximum number of ensemble members to import
#' @return An ensemble table in the LiPD structure
#' @importFrom plyr laply
#' @examples 
#' \dontrun{
#' ensTable = sampleBaconAges("MSB2K",max.ens = 1000)
#' }
#' 
sampleBaconAges <- function(corename,K=NA,bacon.dir=NA,max.ens=NA){
  #from Simon Goring, modified by Nick McKay
  
  #get bacon directory
  bacon.dir <- getBaconDir(bacon.dir)
  
  
  setwd(bacon.dir)
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
      if(getRversion()>=4){
        toTest <- bacData$V1
      }else{
        toTest <- levels(bacData$V1)
      }
      if(strsplit(toTest," ")[[1]][1]=="Bacon"){
        break
      }else{
        sline=sline+1
      }
    }else{
      sline=sline+1
    }
  }
  
  
  if(is.numeric(bacData$V12)){
    end.depth <- bacData$V12
  }else if(is.character(bacData$V12)){
    end.depth=as.numeric(gsub(bacData$V12,pattern=";",replacement=""))
  }else if(is.factor(bacData$V12)){
    end.depth=as.numeric(gsub(levels(bacData$V12),pattern=";",replacement=""))
  }
  
  #Old strategy. Updated 1/8/21 by NPM.
  # if(getRversion()>=4){
  #   end.depth=as.numeric(gsub(bacData$V12,pattern=";",replacement=""))
  # }else{
  #   if(packageVersion("rbacon") < '2.5.0'){
  #     end.depth=as.numeric(gsub(levels(bacData$V12),pattern=";",replacement=""))
  #   }else{
  #     end.depth=as.numeric(gsub(bacData$V12,pattern=";",replacement=""))
  #   }
  # }
  # 
  
  
  start.depth=bacData$V11
  Dc=(end.depth-start.depth)/(K-1)
  depths=seq(start.depth,end.depth,by=Dc)
  
  
  #out_file <- out.file[,1:(ncol(out.file)-2)]
  
  
  
  BACages = kronecker(matrix(1,1,K),out.file[,1])+t(Dc*apply(out.file[,2:(K+1)],1,cumsum))
  BACages = cbind(out.file[,1],BACages[,-ncol(BACages)])
  
  
  
  if(is.na(max.ens)){max.ens=nrow(BACages)}
  
  ages.out <- plyr::laply(1:min(nrow(BACages),max.ens), function(x){approx(x=depths, 
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
#' @inheritDotParams selectData
#' @param chron.num the number of the chronData object that you'll be working in
#' @param bacon.dir the directory where Bacon is installed on this computer.
#' @param remove.rejected don't write out dates that are marked as rejected
#' @param overwrite overwrite files and directories
#' @param cc An integer, or vector of integers corresponding to age that describes the calibration curve. You can specify here (see below) or if it's NA the code will guess based on archiveType
#' \itemize{
#' \item cc=1 IntCal20
#' \item cc=2 MarineCal20
#' \item cc=3 SHCal20
#' }
#' @param L a LiPD object
#' @param site.name the name used for the bacon model (and directories)
#' @param model.num chron.numModel do you want to use?
#' @param use.marine use the marine 13C curve? (yes or no, or NULL to choose)
#' @param ask.reservoir ask about reservoir corrections
#' @return L the input LiPD file with methods added to the chronModel.
#' @examples 
#' \dontrun{
#' writeBacon(L)
#' #Run in interactive mode
#' 
#' writeBacon(L,chron.num=1,meas.table.num = 1,bacon.dir="~/Bacon/",remove.rejected=TRUE,overwrite=TRUE,cc=NA,site.name=L$dataSetName,model.num=NA)
#' }
writeBacon <-  function(L,
                        bacon.dir=NA,
                        chron.num = 1,
                        remove.rejected=TRUE,
                        overwrite=TRUE,
                        cc=NA,
                        site.name=L$dataSetName,
                        model.num=NA,
                        use.marine = NULL,
                        ask.reservoir = TRUE,
                        ...){
  
  #deal with directories
  cur.dir = getwd()
  
  #get bacon directory
  bacon.dir <- getBaconDir(bacon.dir)
  
  #pull out chronology
  cdf <- createChronMeasInputDf(L,
                                chron.num = chron.num,
                                ...)
  

  #merge variables as needed
  if(all(is.na(cdf$depth))){
    stop("No depth values. Depth is required by bacon")
  }
  
  if(!all(is.na(cdf$reservoirAge))){
    if(ask.reservoir){
    print("bacon uses delta-R: deviation from the reservoir curve")
    print("If your data are in absolute reservoir years, you probably want to subtract a reservoir estimate (400 yr) from your data")
    print("Take a look at the values")
    print(head(cdf$reservoirAge))
    R=as.numeric(askUser("enter the number of years you'd like to subtract from your data (0 leaves the data unchanged of course) "))
    cdf$reservoirAge=cdf$reservoirAge-R
    }
  }
  
  nrows <- nrow(cdf)
  cdf$ta <- 3
  cdf$tb <- 4
  
  if(is.na(cc)){
    if(any(names(L)=="archiveType")){
      if(grepl("marine",tolower(L$archiveType))){#assume a marine cal curve if it's marine
        cdf$cc <- rep(2,len=nrows)
      }else{
        cdf$cc <- rep(1,len=nrows)
      }
    }else{
      if(is.null(use.marine)){
        use.marine = readline(prompt = "Do you want to use the Marine curve?")
      }
      if(grepl(use.marine,pattern = "y")){
        cdf$cc <- rep(2,len=nrows)
        
      }else{
        cdf$cc <- rep(1,len=nrows)
      } 
      
      
    }
  }else{# force it to be what was prescribed
    cdf$cc <- rep(cc,len=nrows)
    
  }
  
  #for calibrated ages, use a gaussian and no calibration
  which.calage <- which(grepl(cdf$ageType,pattern = "cal"))
  cdf$ta[which.calage] <- 33
  cdf$tb[which.calage] <- 34
  cdf$cc[which.calage] <- 0
  
  
  out.table <- cdf %>% 
    select(labID,
           allAge,
           allUnc,
           depth, 
           cc, 
           reservoirAge, 
           reservoirAgeUnc,
           ta,
           tb)
      
  #     as.data.frame(matrix(NA,nrow=nrows,ncol=9))
  # if(!is.null(id)){out.table[,1] <- id[1:nrows]}
  # 
  # if(!is.null(age)) {
  #   
  #   out.table[,2] <- age}
  # if(!is.null(error) ) {
  #   out.table[,3] <- error}
  # 
  # 
  # if(!is.null(depth) ) {
  #   
  #   out.table[,4] <- depth}
  # 
  # if(!is.null(cc) ) {
  #   out.table[,5] <- cc}
  # 
  # if(!is.null(reservoir) ) {
  #   out.table[,6] <- reservoir}else
  #   {out.table[,6] <- 0}
  # 
  # if(!is.null(reservoir_error) ) {
  #   out.table[,7] <-reservoir_error}else
  #   {out.table[,7] <- 0}
  # 
  # if(!is.null(ta) ) {
  #   out.table[,8] <-ta}
  # if(!is.null(tb) ) {
  #   out.table[,9] <-tb}
  # 
  
  #replace NAs appropriately
  out.table[is.na(out.table[,1]),1] <- "unknown"
  out.table[is.na(out.table[,3]),3] <- out.table[is.na(out.table[,3]),2]*.05 #estimate as five percent of age
  out.table[is.na(out.table[,6]),6] <- 0
  out.table[is.na(out.table[,7]),7] <- 0
  out.table[is.na(out.table[,8]),8] <- 0
  out.table[is.na(out.table[,9]),9] <- 0
  
  out.table <- na.omit(out.table)
  
  colnames(out.table) <- c("id","age","error","depth","cc","dR","dSTD","ta","tb")
  
  which.rejected <- which(!is.na(cdf$rejected))
  
  if(remove.rejected & length(which.rejected) >= 1){
    out.table <- out.table[-which.rejected,]
  }
  
  print(out.table)
  
  
  #check for core directory
  setwd(bacon.dir)
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
    setwd(bacon.dir)
    
    dir.create(site.name)
  }
  if(write){
    setwd(bacon.dir)
    
    setwd(site.name)
    write.csv(out.table,paste0(site.name,".csv"),row.names=FALSE)
    
  }
  
  setwd(cur.dir)
  if(model.num>length(L$chronData[[chron.num]]$model) | overwrite){
    L$chronData[[chron.num]]$model[[model.num]]=list(inputTable = out.table)
  }
  return(L)
}

#' @export
#' @author Nick McKay
#' @title Load the ensemble, summary and distribution data from a Bacon simulation
#' @description Loads the ensemble, summary and distribution data from a Bacon simulation and stores them in the LiPD structure. Will be run in interactive mode if necessary parameters aren't specified. Most users will want to use runBacon for their bacon needs. 
#' @family Bacon
#' @inheritParams selectData
#' @param chron.num the number of the chronData object that you'll be working in
#' @param bacon.dir the directory where Bacon is installed on this computer.
#' @param site.name the name used for the bacon model (and directories)
#' @param K The bacon K parameter (number of segments to model)
#' @param model.num chron.numModel do you want to use?
#' @param make.new do you want to create a new model in chronData? (TRUE, FALSE, NA). NA will try be smart, or ask you for advice.
#' @param max.ens The maximum number of ensembles to export
#' @return L the input LiPD file with methods and data added to the chronModel.
#' @examples 
#' \dontrun{
#' loadBaconOutput(L)
#' #Run in interactive mode
#' }
loadBaconOutput <- function(L,
                           site.name=L$dataSetName, 
                           K = NA, 
                           chron.num=NA,
                           bacon.dir=NA,
                           model.num=NA,
                           make.new=NA,
                           max.ens = 1000){
  
  #get bacon directory
  bacon.dir <- getBaconDir(bacon.dir)
  
  
  cur.dir = getwd()
  #see if there's an appropriate folder.
  setwd(bacon.dir)
  
  
  if(!any(grepl(site.name,dir()))){
    print(paste0("can't find a directory called",site.name))
    return(L)
  }
  
  
  
  #initialize chron.num
  if(is.na(chron.num)){
    if(length(L$chronData)==1){
      chron.num=1
    }else{
      chron.num=as.integer(readline(prompt = "Which chronData do you want to grab bacon data for? "))
    }
  }
  
  
  
  #initialize model number
  if(is.na(model.num)){
    if(is.null(L$chronData[[chron.num]]$model[[1]])){
      #no models, this is first
      model.num=1
    }else{
      print(paste("You already have", length(L$chronData[[chron.num]]$model), "chron model(s) in chronData" ,chron.num))
      print(paste("If you want to create a new model, enter", length(L$chronData[[chron.num]]$model)+1))
      model.num=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  if(is.na(make.new)){
    make.new = FALSE
  }
  
  if(length(L$chronData[[chron.num]]$model)<model.num){
    if(make.new){
      L$chronData[[chron.num]]$model[[model.num]]=NA
    }else{
      nm=readline(prompt = paste("model",model.num,"doesn't exist. Create it? y or n "))
      if(grepl(pattern = "y",x = tolower(nm))){
        L$chronData[[chron.num]]$model[[model.num]]=NA
      }else{
        stop("Stopping, since you didn't want to create a new model")
      }
    }
  }
  
  
  
  
  #grab methods first
  setwd(bacon.dir)
  
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
  
  
  
  if(is.null(L$chronData[[chron.num]]$model[[model.num]]$methods)){
    L$chronData[[chron.num]]$model[[model.num]]=list("methods"=methods)
    
  }else{
    L$chronData[[chron.num]]$model[[model.num]]$methods=methods
  }
  
  #summary table!
  if(is.na(K)){
    st=dir(pattern="*ages.txt")
  }else{
    st=dir(pattern=paste0("*_",K,"_ages.txt"))
    if(length(st)==0){#wrong K?
      st=dir(pattern="*ages.txt")
    }
  }
  if(length(st)!=1){
    cat("select the correct ages.txt file")
    cat("\n")
    cat("\n")
    st=file.choose()
  }
  
  K=as.numeric(regmatches(st, gregexpr("[0-9]*?(?=\\_ages.txt)", st, perl=TRUE))[[1]])[1]
  summTable = read.table(st,header = TRUE)
  
  #assign names in.
  origNames = c("depth","min","max","median","mean")
  newNames = c("depth","age.rangeLow","age.rangeHigh","age","age")
  
  depth.units = L$chronData[[chron.num]]$measurementTable[[1]]$depth$units
  if(is.null(depth.units)){
    depth.units="cm"
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
  
  L$chronData[[chron.num]]$model[[model.num]]$summaryTable[[1]]=summaryTable
  #
  
  #now grab ensemble data.
  ageEns = sampleBaconAges(corename=site.name,bacon.dir = bacon.dir,K = K,max.ens = max.ens)
  
  ageEns$depth$units = L$chronData[[chron.num]]$model[[model.num]]$summaryTable[[1]]$depth$units
  ageEns$ageEnsemble$units = L$chronData[[chron.num]]$model[[model.num]]$summaryTable[[1]]$age$units
  
  L$chronData[[chron.num]]$model[[model.num]]$ensembleTable[[1]] = ageEns
  
  if(exists("info")){
    #grab distribution data
    for(dd in 1:length(info$calib$probs)){
      dTable = list()
      dTable$age = list(values = info$calib$probs[[dd]][,1], units =  L$chronData[[chron.num]]$model[[model.num]]$summaryTable[[1]]$age$units, variableName = "age")
      dTable$probabilityDensity = list(values = info$calib$probs[[dd]][,2], variableName = "probabilityDensity")
      dTable$depth = info$calib$d[dd]
      dTable$depth.units = L$chronData[[chron.num]]$model[[model.num]]$summaryTable[[1]]$depth$units
      
      L$chronData[[chron.num]]$model[[model.num]]$distributionTable[[dd]] = dTable
    }
  }
  
  
  return(L)
}

