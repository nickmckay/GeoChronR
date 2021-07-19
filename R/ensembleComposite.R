
#' Pull one instance from an ensemble
#'
#' @param X A LiPD column object
#' @family utility
#' @return a vector randomly drawn from the matrix
#' @export
pullInstance <- function(X){
  if(is.matrix(X$values)){
    xo <- X$values[,sample.int(ncol(X$values),size = 1)]
  }else{
    xo <- X$values
  }
  return(xo)
}

#' Concatenate two independent ensemble timeseries
#'
#' @param age.ens.1 Age ensemble list for the first dataset
#' @param paleo.data.1 Paleo data list for the first dataset (ensembles optional)
#' @param age.ens.2 Age ensemble list for the second dataset
#' @param paleo.data.2 Paleo data  list for the second dataset (ensembles optional)
#' @description This "shuffles" together two *independent* ensemble datasets, making no assumptions about stratigraphic order, but assuming that each dataset is representing the same phenomenon.
#' @return a matrix with one instance of concatenate ensemble data
#' @family utility
#' @export
concatEnsembleTimeseries <- function(age.ens.1,paleo.data.1,age.ens.2,paleo.data.2){
  #option 1. dont enforce stratigraphic order

tage.ens.1 <- pullInstance(age.ens.1)
tage.ens.2 <- pullInstance(age.ens.2)
tpaleo.data.1 <- pullInstance(paleo.data.1)
tpaleo.data.2 <- pullInstance(paleo.data.2)

ss <- sort(c(tage.ens.1,tage.ens.2),index.return = TRUE)
sa <- ss$x #the sorted ages
sp <- c(tpaleo.data.1,tpaleo.data.2)[ss$ix] #paleodata in the right order

#return as a matrix together
return(cbind(sa,sp))

}

#' Create a measurementTable from concatenated ensembles
#' @inheritParams concatEnsembleTimeseries
#' @param n.ens How many ensemble members?
#' @description This "shuffles" together two *independent* ensemble datasets, making no assumptions about stratigraphic order, but assuming that each dataset is representing the same phenomenon.
#' @return A measurement table object
#' @family LiPD manipulation
#' @export
#'
#' @examples
#' \dontrun{
#' library(lipdR)
#' library(geoChronR)
#' library(magrittr)
#' 
#' L1 <- readLipd("~/Downloads/ANS1.Kjellman.2019.lpd")
#' L2 <- readLipd("~/Downloads/ANP3.Kjellman.2019.lpd")
#' 
#' 
#' #composite two independent records
#' L1 <- mapAgeEnsembleToPaleoData(L1,paleo.meas.table.num = 1,age.var = "ageEnsemble")
#' 
#' age.ens.1 <- selectData(L1,"ageEnsemble",meas.table.num = 1)
#' paleo.data.1 <- selectData(L1,"C28d2H",,meas.table.num = 1)
#' 
#' L2 <- mapAgeEnsembleToPaleoData(L2,paleo.meas.table.num = 1,age.var = "ageEnsemble")

#' age.ens.2 <- selectData(L2,"ageEnsemble",meas.table.num = 1)
#' paleo.data.2 <- selectData(L2,"C28d2H",,meas.table.num = 1)
#' cmt <- createConcatenatedEnsembleMeasurementTable(age.ens.1,paleo.data.1,age.ens.2,paleo.data.2)
#' plotTimeseriesEnsRibbons(X = cmt$ageEnsemble,Y = cmt$C28d2HComposite) %>%
#' plotTimeseriesEnsLines(X = cmt$ageEnsemble,Y = cmt$C28d2HComposite,n.ens.plot = 5,color = "red")
#' }
#' 
#' 
createConcatenatedEnsembleMeasurementTable <- function(age.ens.1,paleo.data.1,age.ens.2,paleo.data.2,n.ens = 1000){
#preallocate
nae <- matrix(NA,nrow = nrow(age.ens.1$values)+nrow(age.ens.2$values),ncol = n.ens)
npd <- matrix(NA,nrow = nrow(nae),ncol = n.ens)

for(i in 1:n.ens){
  ts <- concatEnsembleTimeseries(age.ens.1,paleo.data.1,age.ens.2,paleo.data.2)
  nae[,i] <- ts[,1]
  npd[,i] <- ts[,2]
}

#create a new measurement table of the ensemble
nael <- list()
nael$values <- nae
nael$variableName <- "ageEnsemble"
nael$units <- paste(unique(c(age.ens.1$units,age.ens.2$units),collapse = ","))
nael$description <- "composite age ensemble created by geoChronR::cancateEnsembleTimeseries()"

npdl <- list()
npdl$values <- npd
npdl$variableName <-  paste0(paste(unique(c(paleo.data.1$variableName,paleo.data.2$variableName),collapse = ",")),"Composite")
npdl$units <- paste(unique(c(paleo.data.1$units,paleo.data.2$units),collapse = ","))
npdl$description <- "composite paleo ensemble created by geoChronR::concateEnsembleTimeseries()"

nmt <- vector(mode = "list",length = 2)
nmt[[1]] <- nael
nmt[[2]] <- npdl

names(nmt) <- sapply(nmt,"[[","variableName")

return(nmt)

}

