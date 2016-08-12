extract.timeseries = function(D){
  #preliminary/hacky version. better to come
TS=list()
t=1
for(f in 1:length(D)){
  L = D[[f]]
  dum = try(ageEnsemble.to.paleoData(L,max.ensemble.members = 1000))
  if(!grepl(pattern = "error",class(dum)))
  {L=dum}
  for(p in 1:length(L$paleoData)){
    P=L$paleoData[[p]]
    for(pm in 1:length(P$paleoMeasurementTable)){
      PM = P$paleoMeasurementTable[[pm]]
      dontinclude = c("age","year","depth","ageEnsemble")
      tograb = which(!(names(PM) %in% dontinclude) & sapply(PM,is.list))
      for(tg in tograb){
        TS[[t]]=NA
        TS[[t]]$dataSetName = L$dataSetName
        TS[[t]]$archiveType=L$archiveType
        TS[[t]]$geo_latitude=L$geo$latitude
        TS[[t]]$geo_AHT.Region = L$geo$AHT.Region
        TS[[t]]$geo_latitude=L$geo$latitude
        TS[[t]]$geo_siteName=L$geo$siteName
        TS[[t]]$geo_longitude=L$geo$longitude
        TS[[t]]$paleoData_values=PM[[tg]]$values
        TS[[t]]$paleoData_variableName=PM[[tg]]$variableName
        TS[[t]]$paleoData_units=PM[[tg]]$units
        if(!is.null(PM[[tg]]$climateInterpretation)){
          TS[[t]]$climateInterpretation_variable=PM[[tg]]$climateInterpretation$variable
          TS[[t]]$climateInterpretation_seasonality=PM[[tg]]$climateInterpretation$seasonality
          TS[[t]]$climateInterpretation_interpDirection=PM[[tg]]$climateInterpretation$interpDirection
        }
        
        diffgrab = which((names(PM) %in% dontinclude) & sapply(PM,is.list))
        for(dg in 1:length(diffgrab)){
          TS[[t]][[names(diffgrab)[dg]]]=PM[[diffgrab[dg]]]$values
          TS[[t]][[paste0(names(diffgrab)[dg],"Units")]]=PM[[diffgrab[dg]]]$units
        }
        t=t+1
      }
    }
  }
return(TS)
}





