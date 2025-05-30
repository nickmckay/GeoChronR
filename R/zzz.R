.onAttach <- function(...){
  if(!interactive()){
    packageStartupMessage(glue::glue("Welcome to geoChronR version {utils::packageVersion('geoChronR')}!"))
    
  }
}