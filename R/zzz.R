.onAttach <- function(...){
  if(!interactive()){
    packageStartupMessage(
      cat(crayon::bold(glue::glue("Welcome to geoChronR version {utils::packageVersion('geoChronR')}!")),"\n")
    )
  }
}