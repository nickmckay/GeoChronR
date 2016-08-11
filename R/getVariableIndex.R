getVariableIndex = function(table,varName=NA,altNames=varName,ignore=NA,always.choose=FALSE){
  #restrict to lists  
  #find variables within the table, and their index
  
  
  
  allNames = names(table)
  listI=which(!sapply(table,class)=="list")
  
  if(!is.na(ignore)){
    if(is.numeric(ignore)){
      ti=ignore
    }else{
      ti=which(allNames %in% ignore)
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
    if(length(idi)==0 | always.choose){
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
        n = readline(prompt="please type the number for the correct match, or a zero if there are no matches: ")
        idi=as.numeric(n)
      }else if(length(idi)>1){
        cat(paste("Multiple possible matches for",varName), "\n")
        for(p in 1:length(idi)){
          cat(paste(p,"-",cnames[idi[p]]), "\n")
        }
        n = readline(prompt="please type the number for the correct match, or a zero if there are no matches: ")
        if(as.numeric(n)==0){
          idi=0
        }else{
          idi=idi[as.numeric(n)]
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
      
    }
  }
  
  
  if(idi==0){
    index=NA
  }else{
    index=which(allNames==cnames[idi])
  }
  return(index)
  
  
}