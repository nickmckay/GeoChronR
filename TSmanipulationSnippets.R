

civar = lapply(TS,"[[","climateInterpretation_variable")
newvar=c()
newvar[sapply(civar,is.null)]=NA
newvar[!sapply(civar,is.null)]=unlist(civar)

ti=which(newvar=="T")
agee = lapply(TS,"[[","ageEnsemble")
region = sapply(TS,"[[","geo_AHT.Region")
fenn = which(grepl("fenn",region,ignore.case = TRUE) | grepl("atlan",region,ignore.case = TRUE))

has.ageEnsemble = which(!sapply(agee,is.null))
FTS=TS[intersect(intersect(ti,fenn),has.ageEnsemble)]




values = lapply(TS,"[[","paleoData_values")



