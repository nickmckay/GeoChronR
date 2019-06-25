#A function that goes through on or more LiPD files and checks/standardizes the depth/age/year name and units

#ADD A LIST OF DATASETS THAT HAVE NOTHING!
if(FALSE){
  library(lipdR)
  # D = readLipd()
  # Dold <- D
  report <- data.frame("dataSetName" = NA,"paleo" = NA,"measurementTable" = NA,"variable" = NA,"units" = NA,"heurUnits" = NA,"allMatch" = NA)
  names2check <- c("year","age")
  units2check <- c("AD","BP")
  for(d in 1:length(D)){#FOR ALL FILES
    L <- D[[d]]
    for(p in 1:length(L$paleoData)){#for all paleodatas
      for(m in 1:length(L$paleoData[[p]]$measurementTable)){
        MT <- L$paleoData[[p]]$measurementTable[[m]]
        
        #check for relevant names  
        nc <- names2check %in% tolower(names(MT))
        
        if(all(!nc)){#None of them are here, uh oh
          #look for alternate match
          for(n in 1:length(names2check)){
            altMatch <- grepl(names2check[n],names(MT),ignore.case = T)
            if (any(altMatch)){
              MT[[which(altMatch)[1]]]$variableName <- names2check[n]
            }
            names(MT)[which(altMatch)[1]] <- names2check[n] #rename it
          }
        }
        
        
        #check AGAIN for relevant names  
        nc <- names2check %in% tolower(names(MT))
        
        if(all(!nc)){#None of them are here, uh oh
          #do something here... probably report out.
          reportLine <- c(L$dataSetName,as.character(p),as.character(m),"NO MATCHING COLUMNS!","","","")
          report <- rbind(report,reportLine)
        }
        
        for(n in 1:length(names2check)){
          if(nc[n]){#it exists!
            ci <- which(grepl(names2check[n],names(MT),ignore.case = T))
            ci <- ci[1]#only grab the first one.
            if(any("units" %in% names(MT[[ci]]))){#units exist
              units <- MT[[ci]]$units
            }else{
              units <- "no unit label!"
            }
            
            if(tolower(units) == "ce"){
              units <- "AD"
            }
            heurUnits <- heuristicUnits(MT[[ci]])
            
            match <- tolower(units2check[n]) == tolower(c(units,heurUnits))
            
            allmatch = all(match,na.rm = T)
            
            
            
            if(!allmatch){
              if(units==units2check[n]){#case 0, name and units match, but heuristics failed
                allmatch <- TRUE
              }else if(units2check[n] == heurUnits){#case 1 name & heuristic matches - rename units
                MT[[ci]]$units <- heurUnits
                allmatch <- TRUE
                
              }else if(units == heurUnits){#case 2 - units an heuristic units match, change the name?
                MT[[ci]]$variableName <- names2check[which(heurUnits==units2check)]
                allmatch <- TRUE
                
              }        
            }
            
            reportLine <- c(L$dataSetName,as.character(p),as.character(m),names2check[n],units,heurUnits,allmatch)
            
            report <- rbind(report,reportLine)
          }
        }
        
        #if age exists but year doesn't, lets convert it.
        if(!nc[1] & nc[2]){
          ci <- which(grepl("age",names(MT),ignore.case = T))[1]
          MT[[ci]] <- convertBP2AD(MT[[ci]])
          names(MT)[ci] <- "year"
          print(paste0("Converted age to year for ",L$dataSetName))
        }
        
        L$paleoData[[p]]$measurementTable[[m]] <- MT
        D[[d]] <- L
      }
    }
  }
  
  
  
}
# 
# 
# 
# # troubleshooting ---------------------------------------------------------
# 
# L <- D$LS09SAJU
# 
# 
# 
# pd <- 2
# L[["paleoData"]][[pd]][["measurementTable"]][[1]][["d18O"]][["values"]] <- fixCommaDecimalIssue(L[["paleoData"]][[pd]][["measurementTable"]][[1]][["d18O"]][["values"]])
# pd <- 1
# L[["paleoData"]][[pd]][["measurementTable"]][[1]][["d18O"]][["values"]] <- fixCommaDecimalIssue(L[["paleoData"]][[pd]][["measurementTable"]][[1]][["d18O"]][["values"]])
# pd <- 3
# L[["paleoData"]][[pd]][["measurementTable"]][[1]][["d18O"]][["values"]] <- fixCommaDecimalIssue(L[["paleoData"]][[pd]][["measurementTable"]][[1]][["d18O"]][["values"]])
# 
# 
# 
# L[["paleoData"]][[pd]][["measurementTable"]][[1]][["year"]][["values"]]
# fixCommaDecimalIssue <- function(X,beforeDecimalDigits = 1){
# #only works if all have same number of digits before the decimal
#   count = nchar(trunc(X))
# den <- 10^(count-(beforeDecimalDigits+1))
# Y <- X/den
# return(Y)
# }
# 
