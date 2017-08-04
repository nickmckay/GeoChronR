#' @export

run.clam.lipd=function(L,which.chron=1,clamDir=NA,remove.reverse=TRUE,overwrite=TRUE,cc=NA,site.name=L$dataSetName,modelNum=NA){
  #initialize clam directory
  if(is.na(clamDir)){
    #check global first
    if(!exists("clamDir",where = .GlobalEnv)){
      cat("please select your clam.R file","\n")
      clamFile=file.choose()
      clamDir<<-dirname(clamFile)
      clamDir=clamDir
    }else{
      clamFile = "clam.R"
    }
    clamDir=get("clamDir",envir = .GlobalEnv)
  }else{
    clamFile = "clam.R"
  }
  setwd(clamDir)
  
  
}

#' @export
write.clam.lipd <-function(L,which.chron=1,clamDir=NA,remove.reverse=TRUE,overwrite=TRUE,cc=NA,site.name=L$dataSetName,modelNum=NA){
  cur.dir = getwd()
  if(is.na(modelNum)){
    if(is.null(L$chronData[[which.chron]]$chronModel[[1]])){
      #no models, this is first
      modelNum=1
    }else{
      print(paste("You already have", length(L$chronData[[which.chron]]$chronModel), "chron model(s) in chronData" ,which.chron))
      modelNum=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  #initialize clam directory
  if(is.na(clamDir)){
    #check global first
    if(!exists("clamDir",where = .GlobalEnv)){
      cat("please select your clam.R file","\n")
      clamFile=file.choose()
      clamDir<<-dirname(clamFile)
      clamDir=clamDir
    }else{
      clamFile = "clam.R"
    }
    clamDir=get("clamDir",envir = .GlobalEnv)
  }else{
    clamFile = "clam.R"
  }
  
  
  
  #pull out chronology
  
  
  C=L$chronData[[which.chron]]
  
  #check for measurementTables
  if(length(C$measurementTable)!=1){
    stop("clam doesn't know how to handle more (or less) than 1 measurement table. You should teach it!")
  }
  
  MT=C$measurementTable[[1]]
  
  
  #go through required fields for clam
  
  #labID
  print("Looking for laboratory ID...")
  idi = getVariableIndex(MT,"labID",altNames = "id")
  
  #14C age
  print("Looking for radiocarbon ages...")
  c14i = getVariableIndex(MT,"age14C",altNames = "age")
  
  #14C age uncertainty
  print("Looking for radiocrabon age uncertainty...")
  c14unci = getVariableIndex(MT,"age14Cuncertainty",altNames = c("age","uncertainty"))
  
  #age (calibrated)
  print("Looking for calibrated ages...")
  agei = getVariableIndex(MT,"age",altNames = "age")
  
  #age uncertainty (calibrated)
  print("Looking for calibrated age uncertainty...")
  ageunci = getVariableIndex(MT,"ageUncertainty",altNames = c("age","uncertainty"))
  
  #depth
  print("Looking for depth...")
  depthi = getVariableIndex(MT,"depth")
  
  #thickness
  print("Looking for thickness...")
  thicki = getVariableIndex(MT,"thickness",altNames = "thick")
  
  
  #reservoir age
  print("Looking for radiocarbon reservoir age offsets (deltaR)...")
  print("can also use radiocarbon reservoir ages if need be...")
  resi = getVariableIndex(MT,"reservoirAge14C",altNames = "reservoir")
  
  
  #rejected ages
  print("Looking for column of reject ages, or ages not included in age model")
  rejeci = getVariableIndex(MT,"rejectedAges",altNames = c("reject","ignore"))
  
  
  #merge variables as needed
  depth <- MT[[depthi]]$values
  if(is.null(depth)){
    stop("No depth. Depth is required by clam")
  }
  
  
  
  #assign data 
  
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
    print("Clam uses delta-R: deviation from the reservoir curve")
    
    print("If your data are in abolute reservoir years, you probably want to subtract a reservoir estimate (400 yr) from your data")
    print("Take a look at the values")
    print(head(reservoir))
    R=as.numeric(readline(prompt = "enter the number of years you'd like to subtract from your data (0 leaves the data unchanged of course) "))
    reservoir=reservoir-R
  }
  
  
  rejected <- MT[[rejeci]]$values
  if(is.null(rejected)){
    rejected <- rep(NA,len=length(depth))
  }
  
  id <- MT[[idi]]$values
  if(is.null(idi)){
    idi <- rep(NA,len=length(depth))
  }
  
  thickness <- MT[[thicki]]$values
  if(is.null(thicki)){
    thicki <- rep(NA,len=length(depth))
  }
  
  nrows <- NROW(cbind(id,cage,calage,error,reservoir,depth,thickness))
  print(cage)
  out.table <- matrix(NA,nrow=nrows,ncol=7)
  if(!is.null(id)) {
    out.table[,1] <- id}
  
  if(!is.null(cage)) {
    
    out.table[,2] <- cage}
  if(!is.null(calage) ) {
    
    out.table[,3] <- calage}
  if(!is.null(error) ) {
    
    out.table[,4] <- error}
  
  if(!is.null(reservoir) ) {
    out.table[,5] <- reservoir}
  
  if(!is.null(depth) ) {
    out.table[,6] <- depth}
  
  if(!is.null(thickness) ) {
    out.table[,7] <- thickness}
  colnames(out.table) <- c("id","cage","calage","error","reservoir","depth","thickness")
  out.table[which(!is.na(out.table[,2])),3]=NA
  which.rejected <- which(grepl("X",rejected))
  
  print(out.table)
  if(remove.reverse){
    if(!all(is.na(which.rejected))){
    out.table <- out.table[-which.rejected,]
    }
  }
  
  
  #check for core directory
  et <- clamDir
  write=FALSE
  if(any(grepl(site.name,et))){
    writedir=FALSE
    if(overwrite){
      write=TRUE
    }
  }else{
    writedir=TRUE
    write=TRUE
    
  }
  
  if(writedir){
    setwd(clamDir)
    setwd("Cores")
    dir.create(site.name)
  }
  if(write){
    setwd(clamDir)
    setwd("Cores")
    setwd(site.name)
    write.csv(out.table,paste0(site.name,".csv"),row.names=FALSE)
  }
}
