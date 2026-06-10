# Plotting helpers: themes, axis transforms, labels, legends, and the
# 2D gridding/quantile machinery used by the ribbon plots.


#get ggplot x and y ranges
#' @export
#' @family plot help
#' @title Get ggplot x and y ranges
#' @description Use this to extract x and y ranges from ggplot, dealing with changes in version
#' @param h a ggplot object
#' @return a list of x and y ranges
getPlotRanges <- function(h){
  if(packageVersion("ggplot2") >= "2"){ #deal with ggplot versions
    if(packageVersion("ggplot2") == "3"){#then version > 3
      y.lims = suppressWarnings(ggplot_build(h)$layout$panel_scales_y[[1]]$get_limits())
      x.lims = suppressWarnings(ggplot_build(h)$layout$panel_scales_x[[1]]$get_limits())
    }else if(packageVersion("ggplot2") == "2"){#version 2
      y.lims = ggplot_build(h)$layout$panel_scales$y[[1]]$get_limits()
      x.lims = ggplot_build(h)$layout$panel_scales$x[[1]]$get_limits()
    }else{#version 4
      y.lims = suppressWarnings(ggplot_build(h)$layout$panel_scales_y[[1]]$get_limits())
      x.lims = suppressWarnings(ggplot_build(h)$layout$panel_scales_x[[1]]$get_limits())
    }
  }else{#version 1
    y.lims = ggplot_build(h)$panel$ranges[[1]]$y.range # get y range
    x.lims = ggplot_build(h)$panel$ranges[[1]]$x.range # get x range
  }
  return(list(x.lims = x.lims, y.lims = y.lims))
}  


#' @export
#' @family plot help
#' @title Define a plot theme for GeoChronR
#' @description Use this to define a theme across geoChronR
#' @import ggplot2
#' @param font.family Specify a font family to use for the theme (default = "Helvetica")
#' @param ... parameters to pass to theme function 
geoChronRPlotTheme <- function(font.family = "Helvetica",...){
  ggplot2::theme_bw(base_family = font.family,...)
}


#' @export
#' @family plot help
#' @importFrom scales trans_new
#' @title convert a BP age scale to AD
#' @description a ggplot scale to convert a BP axis to AD
BP2AD_trans <- function() scales::trans_new("BP2AD",convertBP2AD,convertAD2BP)

#' @export
#' @family plot help
#' @importFrom scales trans_new
#' @title convert an AD age scale to BP
#' @description a ggplot scale to convert a BP axis to AD
AD2BP_trans <- function() scales::trans_new("AD2BP",convertAD2BP,convertAD2BP)

#' @export
#' @import scales
#' @title Reverse axis in log10 scale
#' @description Reverse axis in log10 scale
#' @family plot help
#' @family spectra
#' @author Nick McKay, Julien Emile-Geay
reverselog10_trans <- function(){
  trans <- function(x) -log(x, 10)
  inv <- function(x) 10^(-x)
  return(scales::trans_new("reverselog10-", trans, inv, 
                           scales::log_breaks(base = 10), 
                           domain = c(1e-100, Inf)))
}


#' @export
#' @title Find quantiles across an ensemble
#' @family plot help
#' @family gridding
#' @description Determine quantiles across ensembles of x and/or y, as a function of x, using interpolation
#'
#' @param x n by m matrix where n is the number of observations and m is >= 1
#' @param y n by j matrix where n is the number of observations and j is >= 1 
#' @param n.bins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param probs quantiles to calculate
#' @param seed set a seed for reproducibility
#' @param n.ens how many ensemble members?
#' @param limit.outliers.x limit the plotting of outliers on the x axis to exclude values below this probability limit (default = 0.025)
#'
#' @return list of quantiles and x.bin
#' @author Nick McKay
#' @examples 
#' \dontrun{
#' quantiles <- quantile2d(ageEnsemble,paleoEnsemble)
#' }
#' 
quantile2d = function(x,
                      y,
                      n.bins=500,
                      x.bin = NA,
                      probs = c(0.025,0.25,0.5,0.75, 0.975),
                      n.ens = max(c(ncol(x),ncol(y))), 
                      seed = 111, 
                      limit.outliers.x = .0025){
  #error checking
  if(nrow(x)!=nrow(y)){
    stop("x and y must have the same number of rows")
  }
  
  #set a seed for reproducibility
  set.seed(seed)
  
  #interpolate option...
  sx = sort(c(x))
  
  #cut the range to exclude outliers
  if(!any(is.na(limit.outliers.x))){
    cuts <- quantile(sx,probs = c(limit.outliers.x,1-limit.outliers.x))
    sx <- sx[sx > min(cuts) & sx < max(cuts)]
  }
  
  if(all(is.na(x.bin))){
    x.bin <- approx(1:length(sx),sx,seq(1,length(sx),length.out = n.bins),ties = min)$y #adjust it along y
  }
  y.int = matrix(NA,ncol = n.ens,nrow= length(x.bin))
  
  for(int in 1:n.ens){
    y.int[,int] = approx(x = x[,sample.int(ncol(x),size = 1)] , y = y[,sample.int(ncol(y),size = 1)],xout = x.bin,ties = min)$y
  }
  
  x = x.bin
  y = y.int
  
  #now calculate quantiles for 
  quants = matrix(NA,ncol = length(probs),nrow = length(x))
  
  for(i in 1:length(x)){
    quants[i,] = quantile(y[i,],probs = probs,na.rm  = T)
  }
  return(list(quants = quants,x.bin = x.bin))
}


# 
#' @export
#' @title Two dimensional binning
#' @family gridding
#' @family bin
#' @family plot help
#' @description Calculate the density of samples along a 2-dimensional grid
#' @param x n by m matrix where n is the number of observations and m is >= 1
#' @param y n by j matrix where n is the number of observations and j is >= 1 
#' @param n.bins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param y.bin vector of bin edges over which to bin.
#' @param filter.frac Used to beef up sampling for poorly sampled intervals. Interpolates intervals with less than filter.frac coverage.
#' @param interpolate use interpolation? T/F
#' @return A list with a matrix of density, x.bin and y.bin
#' 
bin2d = function(x,y,n.bins=100,x.bin=NA,y.bin=NA,filter.frac = NA,interpolate = TRUE){
  if(nrow(x)!=nrow(y)){
    stop("x and y must have the same number of rows")
  }
  
  if(interpolate){
    #interpolate option...
    sx = sort(c(x))
    x.bin <- approx(1:length(sx),sx,seq(1,length(sx),length.out = n.bins))$y #adjust it along y
    
    n.ens = max(c(ncol(x),ncol(y)))
    y.int = matrix(NA,ncol = n.ens,nrow= length(x.bin))
    for(int in 1:n.ens){
      y.int[,int] = approx(x = x[,sample.int(ncol(x),size = 1)] , y = y[,sample.int(ncol(y),size = 1)],xout = x.bin )$y
    }
    
    x = x.bin
    y = y.int
  }
  
  
  
  
  #make sure that then number of columns are multiples of each other
  if(length(x)>length(y)){
    if(length(x)%%length(y) != 0){
      x = x[,sample.int(ncol(x),size=floor(ncol(x)/ncol(y)) * ncol(y),replace = FALSE)]
    }
  }
  #again for y
  if(length(y)>length(x)){
    if(length(y)%%length(x) != 0){
      y = y[,sample.int(ncol(y),size=floor(ncol(y)/ncol(x)) * ncol(x),replace = FALSE)]
    }
  }
  
  df = data.frame(x=c(x),y=c(y))
  
  if(all(is.na(x.bin))){
    if(ncol(x)==1){
      x.bin = sort(unique(x))
    }else{
      #range.x=abs(diff(range(df[,1],na.rm=TRUE)))
      #x.bin <- seq((min(df[,1],na.rm=TRUE)-range.x/2), (max(df[,1],na.rm=TRUE)+range.x/2), length=n.bins)
      x.bin <- unique(approx(1:length(sort(df$x)),sort(df$x),seq(1,length(sort(df$x)),length.out = n.bins))$y) #adjust it along y
      #x.bin  = unique(qbins(df$x,n.bins))
      #x.bin = unique(quantile(unique(df$x),probs = seq(0,1,length.out = n.bins)))
    }
  }
  if(all(is.na(y.bin))){
    if(ncol(y)==1){
      y.bin = sort(unique(y))
    }else{
      #range.y=abs(diff(range(df[,2],na.rm=TRUE)))
      #y.bin <- seq((min(df[,2],na.rm=TRUE)-range.y/2), (max(df[,2],na.rm=TRUE)+range.y/2), length=n.bins)
      y.bin <- unique(approx(1:length(sort(df$y)),sort(df$y),seq(1,length(sort(df$y)),length.out = n.bins))$y) #adjust it along y
      #y.bin  = unique(qbins(df$y,n.bins))
      #y.bin  = unique(quantile(unique(df$y),probs = seq(0,1,length.out = n.bins)))
    }
  }
  
  
  fiX = as.numeric(findInterval(df[,1], x.bin))
  fiY = as.numeric(findInterval(df[,2], y.bin))
  ufX = sort(unique(fiX))
  ufY=sort(unique(fiY))
  freq <-  as.data.frame(table(fiX,fiY,deparse.level = 2))
  
  freq[,1] <- as.numeric(ufX[freq[,1]])
  freq[,2] <- as.numeric(ufY[freq[,2]])
  
  freq2D <- matrix(data=0,nrow=length(y.bin),ncol=length(x.bin))
  freq2D[cbind(freq[,2], freq[,1])] <- freq[,3]
  
  #beef up sampling with interpolation? for plotting...
  if(!any(is.na(filter.frac))){
    sumX = apply(freq2D,MARGIN = 1,FUN = sum)
    sumY =  apply(freq2D,MARGIN = 2,FUN = sum)
    freq2D = freq2D[sumX > (length(x.bin)*filter.frac) ,sumY > (length(y.bin)*filter.frac)]
    y.bin = y.bin[sumY > (length(y.bin)*filter.frac)]
    x.bin = x.bin[sumX > (length(x.bin)*filter.frac)]
    
  }
  
  
  density = (freq2D/sum(freq2D))
  
  out = list("density" = density,"x.bin"= x.bin,"y.bin"=y.bin)
  
  return(out)
}

#' @export
#' @family gridding
#' @family plot help
#' @importFrom MASS kde2d
#' @title Two dimensional kernel density estimation
#' @description Use a kernel density estimator to model the density of samples along a 2-dimensional grid
#' @param x n by m matrix where n is the number of observations and m is >= 1
#' @param y n by j matrix where n is the number of observations and j is >= 1 
#' @param n.bins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param y.bin vector of bin edges over which to bin.
#' @return A list with a matrix of density, x.bin and y.bin
kde2d <- function(x,y,n.bins=100,x.bin=NA,y.bin=NA){
  if(nrow(x)!=nrow(y)){
    stop("x and y must have the same number of rows")
  }
  
  #make sure that then number of columns are multiples of each other
  if(length(x)>length(y)){
    if(length(x)%%length(y) != 0){
      x = x[,sample.int(ncol(x),size=floor(ncol(x)/ncol(y)) * ncol(y),replace = FALSE)]
    }
  }
  #again for y
  if(length(y)>length(x)){
    if(length(y)%%length(x) != 0){
      y = y[,sample.int(ncol(y),size=floor(ncol(y)/ncol(x)) * ncol(x),replace = FALSE)]
    }
  }
  
  df = data.frame(x=c(x),y=c(y))
  kde = MASS::kde2d(df$x,df$y,h=1,  n =n.bins)
  out = list("density" = kde$z,"x.bin"= kde$x,"y.bin"=kde$y)
  return(out)
}


#' @export
#' @title get a ggplot legend object
#' @family plot help
#' @author Nick McKay
#' @import ggplot2
#' @param a.gplot ggplot object
#' @return a legend grob
#' 
getLegend<-function(a.gplot){
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#' @export
#' @family plot help
#' @author Nick McKay
#' @title Label axes
#' @description Create an axis label string from a LiPD column vector 
#' @import ggplot2
#' @param varList LiPD "variable list"
#' @return axis label as a string
axisLabel = function(varList){
  #create a string label from a column variable list...
  if(!is.list(varList)){#if it's not a list just return the name of the variable
    return(deparse(substitute(varList)))
  }
  
  vn = varList$variableName
  un = varList$units
  
  if(is.null(vn)){
    vn = deparse(substitute(varList))
  }
  
  if(is.null(un)){
    un = "NA"
  }
  return(paste0(vn," (",un,")"))
  
}
