

#' Convert var.names to strings
#' @family utility
#' @inheritParams selectData 
#' @param used.value 
#' @return the var.name as a string
#' @export
stringifyVariables <- function(var.name,used.value){
  if(is.null(used.value)){
    so <- paste0(var.name," = NULL")
  }else if(isTRUE(used.value == "NULL")){
    so <- paste0(var.name," = ", used.value)
  }else{
    so <- paste0(var.name," = '", used.value, "'")
  }
  return(so)
}


#' askUser
#' @description Helper function to consistently get user input
#' @param query 
#' @family utility
#' @importFrom crayon bold
#' @return user input
#' @export
askUser <- function(query){
  return(readline(prompt = cat(crayon::bold(query))))
}


#' Create a data frame for chron measurement data
#' @inheritParams selectData
#' @family utility
#' @param chron.num 
#' @param overwrite 
#' @param site.name 
#' @param use.marine 
#' @param lab.id.var 
#' @param age.14c.var 
#' @param age.14c.uncertainty.var 
#' @param age.var 
#' @param age.uncertainty.var 
#' @param depth.var 
#' @param reservoir.age.14c.var 
#' @param reservoir.age.14c.uncertainty.var 
#' @param rejected.ages.var 
#' @param split.ages if there's an age_type column, and only one age column, intelligently split between age and age14C
#' @importFrom purrr map_dbl map_lgl map_chr
#' @importFrom crayon bold yellow cyan red green blue 
#' @return a standardized dataframe of chron measurements
#' @export
createChronMeasInputDf <- function(L,
                                   chron.num=NA,
                                   meas.table.num = NA,
                                   lab.id.var="labID",
                                   age.14c.var = "age14C", 
                                   age.14c.uncertainty.var = "age14CUnc", 
                                   age.var = "age", 
                                   age.uncertainty.var = "ageUnc", 
                                   depth.var = "depth", 
                                   reservoir.age.14c.var = "reservoirAge",
                                   reservoir.age.14c.uncertainty.var = "reservoirAge14C",
                                   rejected.ages.var="rejected",
                                   split.ages = TRUE){
  
  
  #initialize chron.num
  if(is.na(chron.num)){
    if(length(L$chronData)==1){
      chron.num=1
    }else{
      chron.num=as.integer()
    }
  }
  
  #We no longer get this
  # if(is.na(model.num)){
  #   if(is.null(L$chronData[[chron.num]]$model[[1]])){
  #     #no models, this is first
  #     model.num=1
  #   }else{
  #     print(cat(crayon::yellow(paste("You already have", length(L$chronData[[chron.num]]$model), "chron model(s) in chronData" ,chron.num))))
  #     model.num=as.integer(askUser("Enter the number for this model- will overwrite if necessary "))
  #   }
  # }
  # 
  # 
  #pull out chronology
  C=L$chronData[[chron.num]]
  
  #check for measurementTables
  if(is.na(meas.table.num)){
    if(length(C$measurementTable)==1){
      meas.table.num = 1  
    }else{
      print(paste("There are", length(L$chronData[[chron.num]]$measurementTable), "measurement tables in chronData " ,chron.num))
      meas.table.num=as.integer(askUser("Which do you want to use here? (Enter an integer)"))
    }
  }
  
  MT=C$measurementTable[[meas.table.num]]
  
  #NM: move this to google speadsheet import?
  
  age.vars <- c(2,3,4,5,7,8)
  depth.vars <- 6
  
  #go through required fields for bacon
  v2go <- c("labID",
            "age14C",
            "age14CUnc",
            "age",
            "ageUnc",
            "depth",
            "reservoirAge",
            "reservoirAgeUnc",
            "rejected")
  
  #user input var.names
  v2gu <- c(
    ifelse(is.null(lab.id.var),"NULL",lab.id.var),
    ifelse(is.null(age.14c.var),"NULL",age.14c.var),
    ifelse(is.null(age.14c.uncertainty.var),"NULL",age.14c.uncertainty.var),
    ifelse(is.null(age.var),"NULL",age.var),
    ifelse(is.null(age.uncertainty.var),"NULL",age.uncertainty.var),
    ifelse(is.null(depth.var),"NULL",depth.var),
    ifelse(is.null(reservoir.age.14c.var),"NULL",reservoir.age.14c.var),
    ifelse(is.null(reservoir.age.14c.uncertainty.var),"NULL",reservoir.age.14c.uncertainty.var),
    ifelse(is.null(rejected.ages.var),"NULL",rejected.ages.var)
  )
  
  #input var.names
  v2gus <-  c("lab.id.var",
              "age.14c.var",
              "age.14c.uncertainty.var",
              "age.var",
              "age.uncertainty.var",
              "depth.var",
              "reservoir.age.14c.var",
              "reservoir.age.14c.uncertainty.var",
              "rejected.ages.var")
  
  #alt names
  v2ga <- c("id",#labIDvar,
            "age",#age.14c.var,
            "unc",#age.14c.uncertainty.var,
            "age",#age.var,
            "unc",#age.uncertainty.var,
            "depth",#depth.var,
            "reservoir",#reservoir.age.14c.var,
            reservoir.age.14c.uncertainty.var,#reservoir.age.14c.uncertainty.var no good alt name here
            "reject")#rejected.ages.var)
  
  #verbose names
  v2gv <-   c("laboratory ID",  #labIDvar,
              "radiocarbon ages",   #age.14c.var,
              "1-sigma radiocarbon age uncertainty (+/-)",  #age.14c.uncertainty.var,
              "calibrated/calendar ages",  #age.var
              "2-sigma calibrated age uncertainty (+/-)", #age.uncertainty.var,
              "depth or position",   #depth.var,
              "radiocarbon reservoir age offsets (deltaR)",  #reservoir.age.14c.var,
              "radiocarbon reservoir age offsets (deltaR) uncertainties", #reservoir.age.14c.uncertainty.var,
              "rejected ages")  #rejected.ages.var))
  
  #set up used string
  v2gUsed <- rep("empty string",times = length(v2gu))
  
  nObsTable <- max(purrr::map_dbl(MT,function(x){
    if(is.list(x)){
      l <- length(x$values)
    }else{
      l <- 0
    }
    return(l)
  }
  ))
  
  chronDf <- as.data.frame(matrix(NA,nrow = nObsTable,ncol = length(v2go)))
  names(chronDf) <- v2go
  for(tv in 1:length(v2go)){#loop through the variables
    cat(crayon::cyan(paste("Looking for",crayon::bold(v2gv[tv]),"\n")))
    ci <- getVariableIndex(MT,v2gu[tv],alt.names = v2ga[tv])
    if(!is.na(ci)){
      if(MT[[ci]]$variableName %in% v2gUsed){
        cont <- askUser(paste0("The variable ",crayon::red(MT[[ci]]$variableName)," has already been used\n Do you want to continue?\n If not you can select a different variable, or specify in function input"))
        if(tolower(stringr::str_sub(cont,1,1)) == "n"){
          ci <- getVariableIndex(MT)

        }
      }
    }
    if(!is.na(ci)){  
      if(tv %in% age.vars){
        unitConversionFactor <- 1
        
        #check units
        u <- MT[[ci]]$units
       if(!is.na(u)){
         if(grepl(pattern = "k",x = u,ignore.case = T)){#it's probably ka
           unitConversionFactor <- 1000
           cat(crayon::red(paste("converting",v2gv[tv],"from ka to yr BP")),"\n")
         }
       } 
        chronDf[,tv] <- MT[[ci]]$values * unitConversionFactor
        v2gUsed[tv] <- MT[[ci]]$variableName
      }else{#not ages
      chronDf[,tv] <- MT[[ci]]$values
      v2gUsed[tv] <- MT[[ci]]$variableName
      }
    }else{
      cat(crayon::red(paste(crayon::bold(v2gv[tv]),"does not seem to exist, moving on.\n")))
      v2gUsed[tv] <- "NULL"
    }
  }       
  
  #print results...
  print("Variable choices for reuse...")
  varUsedStr <- ""
  for(tv in 1:length(v2gu)){
    varUsedStr <- paste0(varUsedStr,stringifyVariables(v2gus[tv],v2gUsed[tv]),", ") 
  }
  
  
  #store this for later use
  assign("chron_varUsedStr",value = varUsedStr,envir = geoChronREnv)
  cat(crayon::green(crayon::bold("For future reference: here are the options you chose:\n Find later with getLastVarString()\n")))
  cat(crayon::green(paste0(varUsedStr,"\n")))
  
  
  #split if possible
  il <- purrr::map_lgl(MT,is.list)
  var.names <- purrr::map_chr(MT[il],function(x) x$variableName)
  
  if(split.ages & "age_type" %in% var.names){#check for age type
    #see if only one age column was used
    a14v <- v2gUsed[v2gus == "age.14c.var"]
    av <- v2gUsed[v2gus == "age.var"]
    
    if(a14v == "NULL"){#only cal was used
      is14 <- grepl(MT$age_type$values,pattern = "14")
      
      oa <- chronDf$age
      oau <- chronDf$ageUnc
      #reassign 14C
      chronDf$age14C <- NA
      chronDf$age14CUnc <- NA
      chronDf$age14C[is14] <- oa[is14] 
      chronDf$age14CUnc[is14] <- oau[is14] 
      
      #and calibrated
      chronDf$age <- NA
      chronDf$ageUnc <- NA
      chronDf$age[!is14] <- oa[!is14] 
      chronDf$ageUnc[!is14] <- oau[!is14] 
      
    }else if(av == "NULL"){#only 14C was used
      is14 <- grepl(MT$age_type$values,pattern = "14")
      oa <- chronDf$age14C
      oau <- chronDf$age14CUnc
      #reassign 14C
      chronDf$age14C <- NA
      chronDf$age14CUnc <- NA
      chronDf$age14C[is14] <- oa[is14] 
      chronDf$age14CUnc[is14] <- oau[is14] 
      
      #and calibrated
      chronDf$age <- NA
      chronDf$ageUnc <- NA
      chronDf$age[!is14] <- oa[!is14] 
      chronDf$ageUnc[!is14] <- oau[!is14] 
      
    }
    
    
  }
  
  
  
  #calculate a few more columns
  #create combined age column.  Assign calibrated ages when 14C ages are empty
  chronDf$allAge <- chronDf$age14C
  no14Ci <- which(is.na(chronDf$age14C))
  chronDf$allAge[no14Ci] <- chronDf$age[no14Ci]
  
  #Create combined age uncertainty column. Assign calibrated uncertainties when 14C uncertainty is empty
  chronDf$allUnc <- chronDf$age14CUnc
  chronDf$allUnc[no14Ci] <- chronDf$ageUnc[no14Ci]
  
  #Createa an age type column
  chronDf$ageType <- "14C"
  chronDf$ageType[no14Ci] <- "cal"
  
  #check to make sure that labIDs exist and are unique
  li <- as.character(chronDf$labID)
  inli <- which(is.na(li))
  for(ili in inli){
    li[ili] <-  paste0("randomLabId_",paste(sample(c(letters,LETTERS,0:9),10),collapse = ""))
  }
  
  #check duplication
  while(any(duplicated(li))){
    idup <- which(duplicated(li))
    for(idu in idup){
      li[idu] <- paste0(li[idu],sample(0:9,size = 1))
    }
  }
  
  chronDf$labID <- li
  
  
  
  return(chronDf)
  
}

#' Get the last set of parameters you used in createChronMeasInputDf()
#'
#' @return
#' @family utility
#' @export
getLastVarString <- function(){
  if(exists("chron_varUsedStr",envir = geoChronREnv)){
    
    return(get("chron_varUsedStr",envir = geoChronREnv))
  }else{
    return("createChronMeasInputDf() has not yet been successfully run this session")
  }
}

