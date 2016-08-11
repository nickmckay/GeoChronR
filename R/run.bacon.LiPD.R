run.bacon.LiPD <-  function(L,which.chron=NA,baconDir=NA,site.name=L$dataSetName,modelNum=NA,remove.reverse=TRUE,overwrite=TRUE,cc=NA){
  cur.dir = getwd()
  #initialize which.chron
if(is.na(which.chron)){
  if(length(L$chronData)==1){
    which.chron=1
  }else{
    which.chron=as.integer(readline(prompt = "Which chronData do you want to run Bacon for? "))
  }
}

#initialize bacon directory
if(is.na(baconDir)){
  #check global first
  if(!exists("baconDir",where = .GlobalEnv)){
    cat("please select your Bacon.R file","\n")
    baconFile=file.choose()
    baconDir<<-dirname(baconFile)
    baconDir=baconDir
  }else{
  baconFile = "Bacon.R"
  }
  baconDir=get("baconDir",envir = .GlobalEnv)
}else{
  baconFile = "Bacon.R"
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


#write bacon file
  L=write.bacon.LiPD(L,which.chron = which.chron,baconDir = baconDir,remove.reverse = remove.reverse,site.name = site.name,overwrite = overwrite,cc=cc,modelNum = modelNum)

  #estimate thickness parameter
  thick = abs(diff(range(L$chronData[[which.chron]]$chronModel[[modelNum]]$inputTable[,4])))/100

#run bacon
  setwd(baconDir)
  source(baconFile)
  Bacon(core=site.name,thick=thick)
  
  print("taking a short break...")
  Sys.sleep(5)
  #pull bacon data into LiPD structure
  L = load.Bacon.output.LiPD(L,site.name=L$dataSetName,which.chron=which.chron,baconDir=baconDir,modelNum=modelNum)
    return(L)
}



