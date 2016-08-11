write.bacon.LiPD <-  function(L,which.chron=1,baconDir=NA,remove.reverse=TRUE,overwrite=TRUE,cc=NA,site.name=L$dataSetName,modelNum=NA){
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


  if(is.na(baconDir)){
    cat("please select your Bacon.R file","\n")
    d=file.choose()
    baconDir=dirname(d)
  }

  #pull out chronology
  C=L$chronData[[which.chron]]

  #check for measurementTables
  if(length(C$chronMeasurementTable)!=1){
    stop("Bacon doesn't know how to handle more (or less) than 1 measurement table. You should teach it!")
  }

  MT=C$chronMeasurementTable[[1]]


  #go through required fields for BACON

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

  #reservoir age
  print("Looking for radiocarbon reservoir age offsets (deltaR)...")
  print("can also use radiocarbon reservoir ages if need be...")
    resi = getVariableIndex(MT,"reservoirAge14C",altNames = "reservoir")

  #reservoir uncertainty
  print("Looking for radiocarbon reservoir age uncertainties...")
  resUnci = getVariableIndex(MT,"reservoirAge14CUncertainty",altNames = c("reservoir","unc"))

  #rejected ages
  print("Looking for column of reject ages, or ages not included in age model")
  rejeci = getVariableIndex(MT,"rejectedAges",altNames = c("reject","ignore"))


  #merge variables as needed
  depth <- MT[[depthi]]$values
  if(is.null(depth)){
    stop("No depth. Depth is required by BACON")
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
    print("Bacon uses delta-R: deviation from the reservoir curve")

    print("If your data are in abolute reservoir years, you probably want to subtract a reservoir estimate (400 yr) from your data")
    print("Take a look at the values")
    print(head(reservoir))
    R=as.numeric(readline(prompt = "enter a number to subtract from your data (0 leaves the data unchanged of course) "))
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
    idi <- rep(NA,len=length(depth))
  }


  #Now build the table


  #nrows <- NROW(cbind(id,age,error,reservoir,depth))
  nrows <- length(depth)
  print(nrows)
  ta <- rep(3,len=nrows)
  tb <- rep(4,len=nrows)
  if(is.na(cc)){
    if(grepl("marine",tolower(L$archiveType))){#assume a marine cal curve if it's marine
      cc <- rep(2,len=nrows)

    }else{
      cc <- rep(1,len=nrows)
    }
  }else{# force it to be what was prescribed
    cc <- rep(cc,len=nrows)

  }

  #for calibrated ages, use a gaussian and no calibration
  ta[which.calage] <- 33
  tb[which.calage] <- 34
  cc[which.calage] <- 0


  out.table <- matrix(NA,nrow=nrows,ncol=9)
  if(!is.null(id)) {
    out.table[,1] <- id[1:nrows]}

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
  out.table[is.na(out.table[,3]),3] <- 1
  out.table[is.na(out.table[,6]),6] <- 0
  out.table[is.na(out.table[,7]),7] <- 0
  out.table[is.na(out.table[,8]),8] <- 0
  out.table[is.na(out.table[,9]),9] <- 0

  colnames(out.table) <- c("id","age","error","depth","cc","dR","dSTD","ta","tb")

  which.rejected <- which(!is.na(rejected))

  if(remove.reverse & length(which.rejected)>1){
    out.table <- out.table[-which.rejected,]
  }

  print(out.table)


  #check for core directory
  setwd(baconDir)
  setwd("Cores")
  et <- dir()
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
    setwd(baconDir)
    setwd("Cores")
    dir.create(site.name)
  }
  if(write){
    setwd(baconDir)
    setwd("Cores")
    setwd(site.name)
    write.csv(out.table,paste0(site.name,".csv"),row.names=FALSE)

  }

  setwd(cur.dir)
   if(modelNum>length(L$chronData[[which.chron]]$chronModel)){
    L$chronData[[which.chron]]$chronModel[[modelNum]]=NA
  }
  L$chronData[[which.chron]]$chronModel[[modelNum]]$inputTable = out.table

  return(L)


}
