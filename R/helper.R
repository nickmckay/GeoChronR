#' @export
installLipd = function(){devtools::install_github("nickmckay/lipd-utilities",subdir = "R")}

#' @export

installGeoChronR= function(){devtools::install_github("nickmckay/geochRonR")}


#' @export
convertBP2AD = function(X){

if(is.list(X)){
  if(tolower(X$units)=="ad" | tolower(X$units)=="ce"){
    print("already in AD or CE, aborting...")
    return(X)
  }
  
  X$units = "AD"
  X$values=1950-X$values
}else{
  X=1950-X
  }
  
  return(X)
 
}