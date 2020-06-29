
#' Pull one instance from an ensemble
#'
#' @param x A LiPD column object
#'
#' @return a vector randomly drawn from the matrix
#' @export
pullInstance <- function(x){
  if(is.matrix(x$values)){
    xo <- x$values[,sample.int(ncol(x$values),size = 1)]
  }else{
    xo <- x$values
  }
  return(xo)
}

#' Concatenate two independent ensemble timeseries
#'
#' @param ae1 Age ensemble list for the first dataset
#' @param pd1 Paleo data list for the first dataset
#' @param ae2 Age ensemble list for the second dataset
#' @param pd2 Paleo data  list for the second dataset
#' @description This "shuffles" together two *independent* ensemble datasets, making no assumptions about stratigraphic order, but assuming that each dataset is representing the same phenomenon.
#' @return a matrix with one instance of concatenate ensemble data
#' @export
concatEnsembleTimeseries <- function(ae1,pd1,ae2,pd2){
  #option 1. dont enforce stratigraphic order

tae1 <- pullInstance(ae1)
tae2 <- pullInstance(ae2)
tpd1 <- pullInstance(pd1)
tpd2 <- pullInstance(pd2)

ss <- sort(c(tae1,tae2),index.return = TRUE)
sa <- ss$x #the sorted ages
sp <- c(tpd1,tpd2)[ss$ix] #paleodata in the right order

#return as a matrix together
return(cbind(sa,sp))

}

#' Create a measurementTable from concatenated ensembles
#' @inheritParams concatEnsembleTimeseries
#' @param nens How many ensemble members?
#' @description This "shuffles" together two *independent* ensemble datasets, making no assumptions about stratigraphic order, but assuming that each dataset is representing the same phenomenon.
#' @return
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
#' L1 <- mapAgeEnsembleToPaleoData(L1,which.pmt = 1,age.var = "ageEnsemble")
#' 
#' ae1 <- selectData(L1,"ageEnsemble",which.mt = 1)
#' pd1 <- selectData(L1,"C28d2H",,which.mt = 1)
#' 
#' L2 <- mapAgeEnsembleToPaleoData(L2,which.pmt = 1,age.var = "ageEnsemble")

#' ae2 <- selectData(L2,"ageEnsemble",which.mt = 1)
#' pd2 <- selectData(L2,"C28d2H",,which.mt = 1)
#' cmt <- createConcatenatedEnsembleMeasurementTable(ae1,pd1,ae2,pd2)
#' plotTimeseriesEnsRibbons(X = cmt$ageEnsemble,Y = cmt$C28d2HComposite) %>%
#' plotTimeseriesEnsLines(X = cmt$ageEnsemble,Y = cmt$C28d2HComposite,maxPlotN = 5,color = "red")
#' }
#' 
#' 
createConcatenatedEnsembleMeasurementTable <- function(ae1,pd1,ae2,pd2,nens = 1000){
#preallocate
nae <- matrix(NA,nrow = nrow(ae1$values)+nrow(ae2$values),ncol = nens)
npd <- matrix(NA,nrow = nrow(nae),ncol = nens)

for(i in 1:nens){
  ts <- concatEnsembleTimeseries(ae1,pd1,ae2,pd2)
  nae[,i] <- ts[,1]
  npd[,i] <- ts[,2]
}

#create a new measurement table of the ensemble
nael <- list()
nael$values <- nae
nael$variableName <- "ageEnsemble"
nael$units <- paste(unique(c(ae1$units,ae2$units),collapse = ","))
nael$description <- "composite age ensemble created by geoChronR::cancateEnsembleTimeseries()"

npdl <- list()
npdl$values <- npd
npdl$variableName <-  paste0(paste(unique(c(pd1$variableName,pd2$variableName),collapse = ",")),"Composite")
npdl$units <- paste(unique(c(pd1$units,pd2$units),collapse = ","))
npdl$description <- "composite paleo ensemble created by geoChronR::concateEnsembleTimeseries()"

nmt <- vector(mode = "list",length = 2)
nmt[[1]] <- nael
nmt[[2]] <- npdl

names(nmt) <- sapply(nmt,"[[","variableName")

return(nmt)

}

