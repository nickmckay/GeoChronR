.onAttach <- function(...){
  if(!interactive()){
    packageStartupMessage(paste("Welcome to geoChronR version", utils::packageVersion("geoChronR")))
  }
}
