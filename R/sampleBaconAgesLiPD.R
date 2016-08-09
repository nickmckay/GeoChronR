sampleBaconAgesLiPD <- function(corename,K=NA,baconDir=NA,maxEns=NA){
  #modified by Nick McKay
  setwd(baconDir)
  setwd("Cores")
  setwd(corename)
  
  
  if(is.na(K)){
    t=dir(pattern=".bacon")
    #To do? pick from multiple? 
    K=as.numeric(regmatches(t[1], gregexpr("[0-9]*?(?=\\.bacon)", t[1], perl=T))[[1]])[1]
    }
  
  out.file=read.table(paste0(corename,'_',as.character(K),'.out'))
  
  #get start and end depths
  bfname=paste0(corename,'_',as.character(K),'.bacon')

  
  sline=1
  while(TRUE){
    bacData=try(read.table(bfname,skip=sline,sep=c(",",";")),silent=TRUE)
    if(!is.character(bacData)){
      if(strsplit(levels(bacData$V1)," ")[[1]][1]=="Bacon"){
        break
      }
    }else{
      sline=sline+1
    }
  }
  end.depth=as.numeric(gsub(levels(bacData$V12),pattern=";",replacement=""))
  start.depth=bacData$V11
  Dc=(end.depth-start.depth)/(K-1)
  depths=seq(start.depth,end.depth,by=Dc)
  
  
  library(plyr)
  #out_file <- out.file[,1:(ncol(out.file)-2)]
  
  
  
  BACages=kronecker(matrix(1,1,K),out.file[,1])+t(Dc*apply(out.file[,2:(K+1)],1,cumsum))
  
  if(is.na(maxEns)){maxEns=nrow(BACages)}
  
  ages.out <- laply(1:min(nrow(BACages),maxEns), function(x){approx(x=depths, 
                                                         y = BACages[x,], 
                                                         xout=depths)$y})
  
  ensembleTable=list()
  ensembleTable$depth$values = depths
  ensembleTable$depth$variableName = "depth"

  
  ensembleTable$ageEnsemble$values = t(ages.out)
  ensembleTable$ageEnsemble$variableName = "ageEnsemble"
  
  
  return(ensembleTable)
}