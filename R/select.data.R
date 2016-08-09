select.data = function(L,varName=NA,where="paleoData",which.data=NA, which.mt=NA){
  #paleo or chron
  P = L[[where]]
  
  #which <where>Data

  if(is.na(which.data)){
    if(length(P)==1){
      which.data=1
    }else{
      print(names(P))
      which.data=as.integer(readline(prompt = "Which paleoData do you want to put this age ensemble in? Select a number "))
    }
  }
  
  #initialize measurement table number
  if(is.na(which.mt)){
    mti=which(grepl(names(P[[which.data]]),pattern = "MeasurementTable"))
    MT = P[[which.data]][[mti]]
    if(length(MT)==1){
      #only one pmt
      which.mt=1
    }else{
      print(paste(where, which.paleo, "has", length(MT), "measurement tables"))
      which.mt=as.integer(readline(prompt = "Which measurement table do you want to put the ensemble in? Enter an integer "))
    }
  }
  
  
  #this is the table of interest  
  MTD=MT[[which.mt]]
  
  ind = getVariableIndex(MTD,varName = varName,always.choose = TRUE)
  
varList = MTD[[ind]]
  
return(varList)
  
 
  
}