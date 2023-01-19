
#get ggplot x and y ranges
#' @export
#' @family plot help
#' @title Get ggplot x and y ranges
#' @description Use this to extract x and y ranges from ggplot, dealing with changes in version
#' @param h a ggplot object
#' @return a list of x and y ranges
getPlotRanges <- function(h){
  if(packageVersion("ggplot2") >= 2){ #deal with ggplot versions
    if(packageVersion("ggplot2") >= 3){#then version > 3
      y.lims = suppressWarnings(ggplot_build(h)$layout$panel_scales_y[[1]]$get_limits())
      x.lims = suppressWarnings(ggplot_build(h)$layout$panel_scales_x[[1]]$get_limits())
    }else{#version 2
      y.lims = ggplot_build(h)$layout$panel_scales$y[[1]]$get_limits()
      x.lims = ggplot_build(h)$layout$panel_scales$x[[1]]$get_limits()
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
#' @title Plot ensemble spectra output
#' @description Plot the output of `computeSpectraEns` as a ribbon plot of distributions, with specified confidence levels
#' @details `plotSpectraEns` re-uses `plotTimeseriesRibbons` and therefore the same graphical conventions. Spectra are plotted on a log-log scale, with the x-axis labeled by periods instead of frequencies, for improved intelligibility. 
#' @family plot
#' @inheritParams plotTimeseriesEnsRibbons
#' @param spec.ens list or dataframe containing frequency (freq) and power (pwr); typically output of computeSpectraEns
#' @param cl.df list or dataframe containing confidence limits (90, 95 and 99\%) as well as frequency (freq)
#' @param x.lims 2-element vector defining the range of periods (x-axis)
#' @param x.ticks n-element vector of the periods labeled
#' @param y.lims 2-element vector defining the range of spectral power (y-axis)
#' @param color.cl color of the lines representing the confidence limits (90, 95, 99\%)
#' @return a ggplot object
#' @author Julien Emile-Geay
#' @import ggplot2
#' @import reshape2
plotSpectraEns = function (spec.ens,
                           cl.df = NULL,
                           x.lims = NULL,
                           x.ticks = c(10, 20, 50, 100, 200, 500, 1000),
                           y.lims = NULL,
                           color.low="white",
                           color.high="grey70",
                           color.line="Black",
                           color.cl="red",
                           alp=0.5){
  
  freq = spec.ens$freqs 
  if (is.matrix(freq)){freq <- rowMedians(freq)}
  
  period <- 1 / freq 
  
  if (is.null(x.lims)) {
    x.lims = c(min(period), max(period))
  } else {
    f.low = 1 / x.lims[2]
    f.high = 1 / x.lims[1]
  }
  freq_range = which(freq >= f.low & freq <= f.high)
  
  if (is.null(y.lims)) {
    m <- floor(log10(min(spec.ens$power[freq_range,])))
    M <- ceiling(log10(max(spec.ens$power[freq_range,])))
  }
  else {
    m <- log10(y.lims[1])
    M <- log10(y.lims[2])
  }
  
  specPlot = plotTimeseriesEnsRibbons(X = period,
                                      Y = spec.ens$power,
                                      color.low = color.low,
                                      color.high = color.high,
                                      color.line = color.line,
                                      alp = alp) +
    scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  limits = c(10^m,10^M)) + 
    scale_x_continuous(breaks=x.ticks, minor_breaks = NULL, trans=reverselog10_trans(), limits = rev(x.lims)) +
    xlab("Period") + ylab("PSD")
  
  if (!is.null(cl.df)) {# if data about confidence limit are provided, plot them
    cl.df = reshape2::melt(cl.df,id = 1) # reshape to facilitate on-line plotting call
    specPlot <- specPlot + geom_line(data=cl.df,aes(x=1/freq,y=value,linetype=variable),color=color.cl)
  }
  
  #if(!is.na(spec.ens$powerSyn)){
  #  specPlot = plotTimeseriesEnsRibbons(X = spec.ens$freqs, Y = spec.ens$powerSyn,add.to.plot = specPlot,probs = c(.9,.95),color.high = "red",alp = .5)
  #}
  
  # Other option: https://stackoverflow.com/questions/37326686/ggplot2-geom-ribbon-with-alpha-dependent-on-data-density-along-y-axis-for-each
  
  return(specPlot)
}

#' @export
#' @title Plot spectrum with confidence limits
#' @description Plot a a single spectrum, with confidence limits (no age ensemble). Useful for comparison with plotSpectraEns() in cases of no age uncertainty (e.g. GCM output)
#' @family plot
#' @family spectra
#' @param spec.df list or dataframe containing frequency (freq) and power (pwr)
#' @param cl.df list or dataframe containing confidence limits (90, 95 and 99\%) as well as frequency (freq)
#' @param x.lims range of plotted periodicities
#' @param x.ticks ticks to mark on the period axis. if NULL, defaults to (10, 20, 50, 100, 200, 500, 1000)
#' @param y.lims 2-vector for the y-axis. If NULL, computed from range(pwr)
#' @param color.line color of the line representing the spectrum
#' @param color.cl color of the lines representing the confidence limits (90, 95, 99\%)
#' @return a ggplot object
#' @author Julien Emile-Geay
#' @import ggplot2
#' @import reshape2
plotSpectrum = function (spec.df,
                         cl.df = NULL,
                         x.lims=NULL,
                         x.ticks= c(10, 20, 50, 100, 200, 500, 1000),
                         y.lims = NULL,
                         color.line="black", 
                         color.cl = "red"){
  # TO DO: general handling of colors (theme)
  
  period <- 1/spec.df$freq
  if (is.null(x.lims)) {
    x.lims = c(min(period),max(period))
  } else {
    f.low = 1/x.lims[2]
    f.high = 1/x.lims[1]
  }
  freq_range = (spec.df$freq>= f.low & spec.df$freq<=f.high)
  
  if (is.null(y.lims)) {
    m <- floor(log10(min(spec.df$pwr[freq_range]))) 
    M <- ceiling(log10(max(spec.df$pwr[freq_range]))) 
  }
  else {
    m <- log10(y.lims[1])
    M <- log10(y.lims[2])
  }
  
  specPlot <- ggplot() + geom_line(aes(x=period,y=spec.df$pwr),color=color.line) + 
    scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  limits = c(10^m,10^M)) + 
    scale_x_continuous(breaks=x.ticks, minor_breaks = NULL, trans=reverselog10_trans(), limits = rev(x.lims)) +
    xlab("Period") + ylab("Power")
  
  if (!is.null(cl.df)) {# if data about confidence limit are provided, plot them
    cl.df = reshape2::melt(cl.df,id = 1) # reshape to facilitate one-line plotting call
    specPlot <- specPlot + geom_line(data=cl.df,aes(x=1/freq,y=value,linetype=variable),color=color.cl)
  }
  
  return(specPlot)
}


#' @export
#' @title Annotate plot of spectra with given periodicities
#' @description Annotate plot of spectra (ensemble or otherwise) with vertical lines at specific periodicities (assumes log10 scaling)
#' @family plot help
#' @family spectra
#'
#' @param specPlot ggplot handle to figure containing spectrum
#' @param periods the periods to highlight in the spectrum
#' @param log10scale Use a log 10 scale TRUE (default) or FALSE
#' @param y.lims optionally specify y limits (default = NULL)
#' @param size font size for labels (default = 4)
#' @param color the color of the text and lines
#'
#' @return ggplot object of spectrum plot
#' @author Julien Emile-Geay
periodAnnotate = function (specPlot, 
                           periods, 
                           color = "orange",
                           log10scale = T, 
                           y.lims = NULL, 
                           size = 4){
  
  if (is.null(y.lims)) {
    ggp <- ggplot_build(specPlot)
    y.lims <- ggp$layout$panel_params[[1]]$y.range # this could break with multiplots... 
  }
  
  for(per in periods){
    specPlot <- specPlot + annotate("segment", x = per, xend = per, y = y.lims[1], yend = y.lims[2],
                                    color = color, alpha = 0.3, linetype = "dotdash")
    specPlot <- specPlot +  annotate("text", x = 1.03*per, y = 1.2*y.lims[2], label = format(per,digits=2, nsmall=0), color = color, size = size)
  }
  return(specPlot)  
}

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
                      interp.method = "linear",
                      limit.outliers.x = .0025,
                      na.thresh = 0.50,
                      ...){
  #error checking
  if(nrow(x)!=nrow(y)){
    stop("x and y must have the same number of rows")
  }
  
  #set a seed for reproducibility
  set.seed(seed)
  
  #interpolate option...
  sx = sort(c(x))
  
  #cut the range to exclude outliers
  if(!is.na(limit.outliers.x)){
    cuts <- quantile(sx,probs = c(limit.outliers.x,1-limit.outliers.x))
    sx <- sx[sx > min(cuts) & sx < max(cuts)]
  }
  
  if(all(is.na(x.bin))){
    x.bin <- approx(1:length(sx),sx,seq(1,length(sx),length.out = n.bins),ties = min)$y #adjust it along y
  }
  y.int = matrix(NA,ncol = n.ens,nrow= length(x.bin))
  
  if(interp.method == "linear"){
    for(int in 1:n.ens){
      y.int[,int] = approx(x = x[,sample.int(ncol(x),size = 1)], 
                           y = y[,sample.int(ncol(y),size = 1)],
                           xout = x.bin,
                           ties = min)$y
    }
  }else if(interp.method == "spread"){
    if(!requireNamespace("compositeR",quietly = TRUE)){
      stop("This options requires the compositeR package (github.com/nickmckay/compositeR)")
    }
    for(int in 1:n.ens){
      te <-  compositeR::spreadPaleoData(age = x[,sample.int(ncol(x),size = 1)],
                                                 value = y[,sample.int(ncol(y),size = 1)],
                                                 newAge = x.bin,
                                         ...)
      
      #don't include extra values
      y.int[,int] <- te$spreadVal[which(te$spreadAge %in% x.bin)]
    }
  }else{
    stop(glue::glue("I don't recognize the interp.method {interp.method}. The options are 'linear' or 'spread'"))
  }

  
  
  x = x.bin
  y = y.int
  
  #screen out if more than na.frac.screen
  fracMissing <- apply(is.na(y),1,sum)/ncol(y)
  
  y[fracMissing > na.thresh,] <- NA
  
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
#'
#' @param x n by m matrix where n is the number of observations and m is >= 1
#' @param y n by j matrix where n is the number of observations and j is >= 1 
#' @param n.bins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param y.bin vector of bin edges over which to bin.
#' @param filter.frac Used to beef up sampling for poorly sampled intervals. Interpolates intervals with less than filter.frac coverage.
#' @param interp.method Method to use for interpolation "linear" or "spread" (nearest neighbor with optional gaps)
#' @param interpolate use interpolation? T/F
#'
#' @return A list with a matrix of density, x.bin and y.bin
#' 
bin2d = function(x,y,n.bins=100,x.bin=NA,y.bin=NA,filter.frac = NA,interpolate = TRUE,interp.method = "linear"){
  if(nrow(x)!=nrow(y)){
    stop("x and y must have the same number of rows")
  }
  
  if(interpolate){
    #interpolate option...
    sx = sort(c(x))
    x.bin <- approx(1:length(sx),sx,seq(1,length(sx),length.out = n.bins))$y #adjust it along y
    
    n.ens = max(c(ncol(x),ncol(y)))
    y.int = matrix(NA,ncol = n.ens,nrow= length(x.bin))
    if(interp.method == "linear"){
      for(int in 1:n.ens){
        y.int[,int] = approx(x = x[,sample.int(ncol(x),size = 1)] , y = y[,sample.int(ncol(y),size = 1)],xout = x.bin )$y
      }
    }else if(interp.method == "spread"){
      if(!requireNamespace("compositeR",quietly = TRUE)){
        stop("This options requires the compositeR package (github.com/nickmckay/compositeR)")
      }
      y.int[,int] = compositeR::spreadPaleoData(age = x[,sample.int(ncol(x),size = 1)] ,
                                                value = y[,sample.int(ncol(y),size = 1)],
                                                newAge = x.bin, maxPct = 0.75 )
      
    }else{
      stop(glue::glue("I don't recognize the interp.method {interp.method}. The options are 'linear' or 'spread'"))
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
  if(!is.na(filter.frac)){
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
#' @family plot
#' @author Nick McKay
#' @title Plot a summary figure
#' @description shows a map, timeseries, and age model diagram, and basic simple metadata
#' @import ggplot2
#' @import grid
#' @importFrom gridExtra grid.arrange
#' @inheritDotParams plotChronEns
#' @inheritParams selectData
#' @param paleo.age.var variableName to use for x axis of the paleo plot ("age" by default)
#' @param paleo.data.var variableName to use for the y axis of the paleo plot (NA by default, which lets you choose)
#' @param chron.number chron.numData object to use (NA by default, will ask if needed)
#' @param paleo.meas.num paleo.num measurement table to use (NA by default, will ask if needed)
#' @param chron.meas.num chron.num measurement table to use (NA by default, will ask if needed)
#' @param chron.depth.var variableName to use for chron depth ("depth" by default)
#' @param chron.age.var variableName to use for chron calibrated age ("age" by default)
#' @param chron.age.14c.var variableName to use for chron 14C age ("age" by default)
#' @param dot.size what size dot for the chron plot? Only used if not plotting by plotChronEns() (default = 5)
#' @param summary.font.size Font size for the summary
#' @param text.width Width of the text panel
#' @param legend.position location of the legend
#' @return A gridArrange of ggplot grobs
#' @examples 
#' \dontrun{
#' myPlot = summaryPlot(L)
#' }
plotSummary = function(L,
                       paleo.age.var = "age",
                       paleo.data.var = NA,
                       chron.number = NA, 
                       paleo.meas.num = NA, 
                       chron.meas.num = NA, 
                       chron.depth.var = "depth", 
                       chron.age.var = "age", 
                       chron.age.14c.var = "age14C",
                       dot.size = 5, 
                       summary.font.size = 6, 
                       text.width = 400/summary.font.size, 
                       legend.position = c(0.7,0.3),
                       ...){
  
  #is this a LiPD file?
  if(is.list(L)){
    if(is.null(L$dataSetName)){
      stop("This is either not a single LiPD object, or it has no dataSetName. plotSummary requires a single LiPD object as input")
    }
  }else{
    stop("plotSummary requires a single LiPD object as input")
  }
  
  map <- mapLipd(L,extend.range = 4,map.type = "line")
  
  #plot paleoData
  
  if(is.na(paleo.age.var)){
    print("What should we plot on the X-axis?")
    print("We'll look for age or year...")
    age=selectData(L,var.name = "age",alt.names = "year",meas.table.num = paleo.meas.num)
  }else{
    age=selectData(L,var.name = paleo.age.var, meas.table.num = paleo.meas.num)
  }
  
  if(is.na(paleo.data.var)){
    print("What should we plot on the Y-axis?")
    variable=selectData(L,meas.table.num = paleo.meas.num)
  }else{
    variable=selectData(L,var.name = paleo.data.var,meas.table.num = paleo.meas.num)
  }
  
  paleoPlot = plotLine(X = age,Y = variable)
  paleoPlot = paleoPlot + ggtitle(paste("PaleoData:",variable$variableName))
  
  #do chron.
  chronPlot <- plotChron(L,chron.number = chron.number, meas.num = chron.meas.num, depth.var = chron.depth.var, age.var = chron.age.var,age.14c.var = chron.age.14c.var, dot.size = dot.size,legend.position = legend.position, ...)
  
  if(!is.list(chronPlot)){
    if(is.na(chronPlot)){
      chronPlot = grid::grobTree(grid::rectGrob(gp = grid::gpar(fill = 1,alpha=.1)),grid::textGrob("No chronData"))
    }
  }
  
  
  
  lay = rbind(c(1,1,2,2),
              c(3,3,2,2),
              c(3,3,4,4),
              c(3,3,4,4))
  
  
  if(!is.null(L$pub[[1]]$citation)){
    citation <- L$pub[[1]]$citation
  }else{
    authors <- L$pub[[1]]$author
    if(is.list(authors)){
      authors <-  unlist(authors)
    }
    year <- L$pub[[1]]$year
    if(is.null(year)){
      year <-  L$pub[[1]]$pubYear
    }
    title <- L$pub[[1]]$title
    journal <- L$pub[[1]]$journal
    volume <- L$pub[[1]]$volume
    pages <- L$pub[[1]]$pages
    
    citation <- paste0(authors," (",as.character(year),"). ",title,". ",journal, " ", as.character(volume),", ",pages,".")
  }
  citation <- paste(strwrap( citation, width = text.width, simplify = FALSE)[[1]],collapse = "\n          ")
  dataSetText = paste("DataSetName:",L$dataSetName,"\nArchive Type: ",L$archiveType,"\nCitation:",citation)
  summaryText = grid::grobTree(grid::rectGrob(gp = grid::gpar(fill = 1,alpha=.1)), grid::textGrob(x = unit(0.03, "npc"), y = unit(0.5, "npc"),dataSetText,just = "left",check.overlap = FALSE,gp = grid::gpar(fontfamily = "mono",fontsize = summary.font.size)))
  
  summary = gridExtra::grid.arrange(grobs = list(summaryText,paleoPlot,map,chronPlot),layout_matrix=lay)    
  return(summary)
  
}

#' @family plot
#' @export
#' @author Nick McKay
#' @title Plot or add a line to plot
#' @description Plots or adds a line to aplot
#' @import ggplot2
#' @param X A LiPD variable list to plot, including values, units, names, and more
#' @param Y A LiPD variable list to plot, including values, units, names, and more
#' @param color Line color (following ggplot rules)
#' @param alp Line transparency
#' @param add.to.plot A ggplot object to add these lines to. Default is ggplot() . 
#' @return A ggplot object
plotLine = function(add.to.plot=ggplot(),X,Y,color="black",alp = 1){
  
  #X and Y and are LiPD variable list, including values, units, names, etc...
  df = data.frame(x = X$values, y = Y$values)
  plot = add.to.plot+ geom_line(data=df,aes(x=x,y=y),color =color, alpha = alp)+
    ylab(axisLabel(Y))+
    geoChronRPlotTheme()
  
  if(grepl("AD",X$units) | grepl("CE",X$units)){
    plot = plot+scale_x_continuous(name = axisLabel(X))
  }else{
    plot = plot+scale_x_reverse(name = axisLabel(X))
  }
  return(plot)
}


#' @export
#' @family plot
#' @author Nick McKay
#' @title Plot an ensemble timeseries as a set of lines
#' @description Plot an ensemble timeseries as a set of lines. Useful for displaying a handful of ensemble members to characterize individual paths. 
#' @import ggplot2 dplyr RColorBrewer
#' @importFrom tidyr pivot_longer
#'
#' @param X A LiPD variable list to plot, including values, units, names, and more
#' @param Y A LiPD variable list to plot, including values, units, names, and more
#' @param color Either 1) A line color (following ggplot rules) to use for all lines (e.g., "blue"), 2) An RColorBrewer pallette to repeat over the lines (e.g. "Blues") or 3) a vector specifying the color for all lines (e.g., c("red","white","blue"))
#' @param n.ens.plot Whats the maximum number of lines to plot?
#' @param alp Line transparency
#' @param na.rm Remove NAs from X and Y? Set to FALSE to preserve line breaks where data are missing. (default = TRUE)
#' @param add.to.plot A ggplot object to add these lines to. Default is ggplot() . 
#'
#' @return A ggplot object
plotTimeseriesEnsLines = function(add.to.plot=ggplot(),
                                  X,
                                  Y,
                                  alp=.2,
                                  color = "blue",
                                  n.ens.plot=100,
                                  na.rm = TRUE){
  #check to see if time and values are "column lists"
  
  oX = X
  oY = Y
  if(is.list(X)){X=as.data.frame(X$values)}
  if(is.list(Y)){Y=as.data.frame(Y$values)}
  
  
  
  X=as.data.frame(X)
  Y=as.data.frame(Y)
  
  if(nrow(X)!=nrow(Y)){
    stop("X and Y must have the same number of observations")
  }
  
  np = min(n.ens.plot,ncol(X)*ncol(Y))
  #sample randomly what to plot
  pX = sample.int(ncol(X),size = np,replace = TRUE)
  pY = sample.int(ncol(Y),size = np,replace = TRUE)
  
  Xplot <- tidyr::pivot_longer(X[,pX],cols = everything(),names_to = "xEns",values_to = "x")
  Yplot <- tidyr::pivot_longer(Y[,pY],cols = everything(),names_to = "yEns",values_to = "y")
  
  dfXY <- dplyr::bind_cols(Xplot,Yplot)
  
  #  names(dfXY) <- c("xEns","x","yEns","y")
  dfXY <- dplyr::arrange(dfXY,xEns)
  
  if(na.rm){
    dfXY <- filter(dfXY,!is.na(x)) %>% 
      filter(!is.na(y))
  }
  
  #deal with colors
  if(color %in% rownames(RColorBrewer::brewer.pal.info)){#
    #then it's an RColorBrewer pallette
    colorScale <- rep_len(suppressWarnings(RColorBrewer::brewer.pal(n = np,name = color)),length.out = np)
  }else{#it's not
    if(length(color) == 1){#apply one color to all
      colorScale <- rep(color,times = np)
    }else{
      if(length(color) == np){
        colorScale <- color
      }else{
        stop("color must be either 1) a single color to repeated, 2) an RColorBrewer palette or 3) a string the same length of the number of lines to be plotted")
      }
    }
  }
  
  linePlot = add.to.plot+
    geom_path(data=dfXY,
              stat = "identity",
              aes(x=x,y=y,color = xEns),
              alpha=alp)+
    scale_color_manual(values = colorScale)+
    geoChronRPlotTheme()+
    theme(legend.position = "none")
  
  #add labels
  linePlot = linePlot+xlab(axisLabel(oX))+ylab(axisLabel(oY))
  
  #reverse the xaxis if the units are BP
  if(any(grepl(pattern = "BP",x = axisLabel(oX))) | (grepl(pattern = "ka",x = axisLabel(oX))) | (grepl(pattern = "B2k",x = axisLabel(oX))) | (grepl(pattern = "kyr",x = axisLabel(oX)))){
    linePlot = linePlot + scale_x_reverse(name = axisLabel(oX))
  }
  
  
  
  return(linePlot)
  
}
#' @export
#' @family plot
#' @author Nick McKay
#' @title Plot an ensemble timeseries as ribbons of probabilities
#' @description Plot an ensemble timeseries as a set of bands of probability. Useful for displaying the full range of probability across ensemble members.
#' @import ggplot2 tibble
#' @param X A LiPD variable list to plot, including values, units, names, and more
#' @param Y A LiPD variable list to plot, including values, units, names, and more
#' @param probs a vector of probabilities to plot as ribbons. It will create bands as ribbons of quantiles moving inward. If there's an odd number, it plots the middle quantile as a line. 
#' @param color.low Color of the outermost band; the extreme quantiles of the distribution
#' @param color.high Color of the innermost band; the central quantiles of the distribution
#' @param color.line Line color (following ggplot rules)
#' @param color.vector A vector (of length equal to the number of bands) that specifies the colors for the ribbons from the outermost band in (default = NA). Colors specified as string according to ggplot2 conventions. If present, this overrules color.high and color.low
#' @param line.width Width of the line
#' @param n.bins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param y.bin vector of bin edges over which to bin.
#' @param alp alpha (transparency) parameter for the ribbons
#' @param add.to.plot A ggplot object to add this plot to. Default is ggplot() . 
#' @param export.quantiles If TRUE, return the plotted quantiles rather than the plot
#' @inheritDotParams quantile2d
#' @return A ggplot object OR list of plotted quantiles, depending on export.quantiles
plotTimeseriesEnsRibbons = function(add.to.plot=ggplot(),
                                    X,
                                    Y,
                                    alp=1,
                                    probs=c(0.025,.25,.5,.75,.975),
                                    x.bin=NA,
                                    y.bin=NA,
                                    n.bins=200,
                                    color.low="white",
                                    color.high="grey70",
                                    color.line="Black",
                                    color.vector = NA,
                                    line.width=1,
                                    export.quantiles = FALSE,
                                    ...){
  
  
  #check to see if time and values are "column lists"
  oX = X
  oY = Y
  if(is.list(X)){X=as.data.frame(X$values)}
  if(is.list(Y)){Y=as.data.frame(Y$values)}
  
  X=as.matrix(X)
  Y=as.matrix(Y)
  
  #check to make sure that at least one is a matrix
  if(ncol(X)==1 & ncol(Y)==1){
    #then just plot a line
    df = data.frame(x=X,y=Y)
    bandPlot=add.to.plot+geom_line(data=df,aes(x=X,y=Y),color=color.line)+geoChronRPlotTheme()
    
  }else{
    
    if(nrow(X)!=nrow(Y)){
      stop("X and Y must have the same number of observations")
    }
    
    ###DEPRECATED - old method.
    # binned = bin2d(X,Y,x.bin=x.bin,y.bin = y.bin,n.bins=n.bins)
    # binned = kde2d(X,Y,x.bin=x.bin,y.bin = y.bin,n.bins=n.bins)
    # find cum sum probabilities  
    
    
    # #nbox = prod(dim(binned$density))
    # colSums = apply(binned$density,2,sum)
    # colCount = colSums*nbox
    # good.cols = which(colCount>nrow(binned$density))
    # sumMat= t(matrix(colSums, nrow=length(colSums),ncol=nrow(binned$density)))
    # bmcs = apply(binned$density/sumMat,2,cumsum)
    # good.cols = which(!apply(is.na(bmcs),2,all) & scale(colSums)>-3)
    
    # probMat = matrix(data = NA,nrow=length(good.cols),ncol=length(probs))
    
    # for(p in 1:length(probs)){
    #   probMat[,p]=apply(bmcs[,good.cols],MARGIN=2,function(x) approx(x,binned$y.bin,probs[p],method = "constant")$y)
    # }
    ###END DEPRECATED - old method.
    
    probMatList <- quantile2d(X,
                              Y,
                              n.bins = n.bins,
                              x.bin = x.bin,
                              probs = probs,
                              ...)
    
    if(export.quantiles){
      probMat <- cbind(probMatList$x.bin,probMatList$quants)
      colnames(probMat) <- c("ages",as.character(probs))
      probMat <- tibble::as_tibble(probMat)
      return(probMat)
    }
    
    
    
    probMat  <-  probMatList$quants
    
    probMat=as.data.frame(probMat)
    
    #Line labels are deprecated
    
    # line.labels=as.character(probs)
    
    
    # #make labels better
    # goodName= c("-2 sigma","-1 sigma","Median","1  sigma","2  sigma")
    # realProb= c(pnorm(-2:2))
    # for(i in 1:length(line.labels)){
    #   p=which(abs(as.numeric(line.labels[i])-realProb)<.001)
    #   if(length(p)==1){
    #     line.labels[i]=goodName[p]
    #   }
    # }
    # names(probMat) = line.labels
    
    #plot it!
    #make pairs of bands moving in 
    
    #if probs is odd, the center one is just a line
    if(ncol(probMat)%%2==1){
      center = data.frame(x=probMatList$x.bin,y=probMat[,median(1:length(probs))])
      
      bandMat =  probMat[,-median(1:length(probs))]
    }else{
      center =NA
      bandMat =  probMat
    }
    
    #if the colors are specified, use the that
    if(!all(is.na(color.vector))){
      if(length(color.vector) != ncol(bandMat)/2){
        stop("The number of colors provided in color.vector does not match the number of bands")
      }
      fillCol <- color.vector
    }else{ #use a ramp
      fillCol=colorRampPalette(c(color.low,color.high))( ncol(bandMat)/2+1 )[-1]
    }
    
    for(b in 1:(ncol(bandMat)/2)){
      if(b==1){
        bandPlot = add.to.plot+geoChronRPlotTheme()
      }
      bands=data.frame(x=probMatList$x.bin,
                       ymin = bandMat[,b],
                       ymax = bandMat[,ncol(bandMat)-b+1])
      
      bandPlot = bandPlot+
        geom_ribbon(data=bands,aes(x=x,ymin=ymin,ymax=ymax),fill=fillCol[b],alpha=alp)
    }
    
    if(!all(is.na(center))){
      bandPlot <- bandPlot +
        geom_line(data=center,aes(x=x,y=y),color=color.line,size=line.width)
    }
    
  }
  
  #add labels
  bandPlot = bandPlot+xlab(axisLabel(oX))+ylab(axisLabel(oY))
  
  #reverse the xaxis if the units are BP
  if(any(grepl(pattern = "BP",x = axisLabel(oX))) | (grepl(pattern = "ka",x = axisLabel(oX))) | (grepl(pattern = "B2k",x = axisLabel(oX))) | (grepl(pattern = "kyr",x = axisLabel(oX)))){
    bandPlot = bandPlot + scale_x_reverse(axisLabel(oX))
  }
  
  
  return(bandPlot)
  
}

#' @export
#' @family plot
#' @family regress
#' @author Nick McKay
#' @title Plot an ensemble of data as a scatterplot
#' @description Plot an ensemble timeseries as a scatter plot. Useful in showing the general impact of uncertainty on a bivariate relationship.
#' @import ggplot2
#' @param X A LiPD variable list to plot, including values, units, names, and more
#' @param Y A LiPD variable list to plot, including values, units, names, and more
#' @param alp Marker transparency
#' @param n.ens.plot Whats the maximum number of points to plot?
#' @param add.to.plot A ggplot object to add this plot to. Default is ggplot() . 
#' @return A ggplot object
plotScatterEns = function(X,Y,alp=.2,n.ens.plot=1000,add.to.plot = ggplot()){
  X=as.matrix(X)
  Y=as.matrix(Y)
  
  if(nrow(X)!=nrow(Y)){
    stop("X and Y must have the same number of observations")
  }
  
  np = min(n.ens.plot,ncol(X)*ncol(Y))
  #sample randomly what to plot
  pX = sample.int(ncol(X),size = np,replace = TRUE)
  pY = sample.int(ncol(Y),size = np,replace = TRUE)
  #create data frame of uncertain X, Y data
  Xplot = c(X[,pX])
  Yplot = c(Y[,pY])
  dfXY = data.frame("x"=Xplot,"y"=Yplot)
  
  scatterplot = add.to.plot+
    geom_point(data = dfXY,aes(x = x,y=y),alpha=alp)+
    geoChronRPlotTheme()  
  return(scatterplot)
}

#' @export
#' @family plot
#' @family regress
#' @author Nick McKay
#' @title Plot an ensemble of trendlines
#' @description Plot an ensemble of trendlines based on slope and intercept. 
#'
#' @param mb.df A data.frame of slopes (column 1) and intercepts (column 2)
#' @param alp Line transparency
#' @param index.xy index of which observations to use
#' @param x.range range of x values (min and max)
#' @param color color of the lines
#' @param add.to.plot A ggplot object to add these lines to. Default is ggplot() . 
#'
#' @return A ggplot object
plotTrendLinesEns = function(mb.df,
                             x.range,
                             index.xy=1:nrow(mb.df) ,
                             alp=.2 ,
                             color = "red",
                             add.to.plot=ggplot()){
  xvec = c(x.range,NA)
  yall = c()
  xall = c()
  df = data.frame(m=mb.df[index.xy,1],b=mb.df[index.xy,2])
  for(p in 1:length(index.xy)){
    yvec = c(df$m[p]*x.range + df$b[p],NA)
    yall = c(yall,yvec)
    xall = c(xall,xvec)
  }
  dfi = data.frame(x=xall,y=yall)
  
  trendlines = add.to.plot+
    geom_path(data=dfi,aes(x=x,y=y),color = color,alpha=alp)+
    geoChronRPlotTheme()+
    xlim(x.range)
  
  
  return(trendlines)
}

#' @export
#' @family plot
#' @family correlation
#' @author Julien Emile-Geay
#' @author Nick McKay
#' @title Plot the results of an ensemble correlation
#' @description Plots the output of an ensemble correlation analysis.
#' @import ggplot2
#'
#' @param corout output from corEns()
#' @param bins Number of bins in the histogram
#' @param line.labels Labels for the quantiles lines
#' @param add.to.plot A ggplot object to add these lines to. Default is ggplot()
#' @param legend.position Where to put the map legend?
#' @param significance.option Choose how handle significance. Options are:
#'  \itemize{
#'  \item "raw" for uncorrected p-values
#'  \item "eff-n" to adjust the test's sample size to reflect the reduction in degrees of freedom due to autocorrelation
#'  \item "isopersistent" to estimate significance by generating surrogates, or random synthetic timeseries, that emulate the persistence characteristics of the series.
#'  \item "isospectral" A non-parametric alternative which estimates significance by generating surrogates by scrambling the spectral phases of the two datasets, thus preserving their power spectrum while destroying the correlated signal. This is the recommended (and default) option.
#'  }
#' @param f.sig.lab.position x,y (0-1) position of the fraction of significant correlation labels
#' 
#' @param sig.level What significance level to plot?
#' @param use.fdr Use results from False Discovery Rate testing in plot?
#' @param bar.colors What colors to use for the bars, formatted as (insignificant, significant, significant after FDR)
#'
#' @return A ggplot object
plotCorEns = function(corout,
                      bins=40,
                      line.labels = corout$cor.stats$percentiles,
                      add.to.plot=ggplot2::ggplot(),
                      legend.position = c(0.2, 0.8),
                      f.sig.lab.position = c(0.15,0.4),
                      sig.level = 0.05,
                      significance.option = "isospectral",
                      use.fdr = TRUE,
                      bar.colors = c("grey50","Chartreuse4","DarkOrange")){
  
  #pull data frames out of the list
  cor.df <- filter(corout$cor.df,!is.na(r))
  cor.stats <- corout$cor.stats
  
  
  # evaluate preliminary quantities
  rng <- range(cor.df$r)
  bw = (rng[2]-rng[1])/bins
  
  if(significance.option == "raw"){
    p <- cor.df$pRaw
  }else if(grepl("eff",significance.option,ignore.case = T)){#bretherton
    p <- cor.df$pSerial
  }else if(grepl("spect",significance.option,ignore.case = T)){#isospectral
    p <- cor.df$pIsospectral
  }else if(grepl("persis",significance.option,ignore.case = T)){#isopersistent
    p <- cor.df$pIsopersistent
  }else{
    stop("significance.option not recognized. Accepted values are 'isospectral','isopersistent','eff-n', or 'raw'")
  }
  
  #check that p-values exist
  if(is.null(p)){
    stop("It doesn't look like the values you calculated for significance exist. Make sure you had the right options selected in corEns().")
  }
  
  #remove missing values
  goodp <- is.finite(p)
  
  cor.df <- cor.df[goodp,]
  p <- p[goodp]
  
  #is it signficant?
  issig <- p < sig.level
  
  if(use.fdr){
    fdrOut <- fdr(p,qlevel=sig.level,method="original",adjustment.method='mean')
    sig_fdr <- matrix(FALSE,nrow = length(p))
    sig_fdr[fdrOut] <- TRUE
    
    #assign for plotting
    fdrSigPlot <- matrix(NA,nrow(sig_fdr))
    #sig before FDR
    fdrSigPlot[which(issig & !sig_fdr)] <- 1
    #sig after FDR
    fdrSigPlot[which(sig_fdr)] <- 2
    #always insignificant
    fdrSigPlot[which(!issig & !sig_fdr)] <- 0
    sig_frac <- sum(sig_fdr,na.rm = TRUE)/length(sig_fdr)*100
    
  }else{
    sig_frac <- sum(issig,na.rm = TRUE)/length(issig)*100
    
  }
  
  
  sig_lbl = paste0("Fraction significant: ", round(sig_frac,1), "%")
  # Now the plotting begins
  
  #pick a good x scale
  xs <- rng+c(-diff(rng)*.05,diff(rng)*.05)
  plotR <- cor.df$r
  
  h = ggplot() + ggtitle("Correlation Distribution") # initialize plot
  
  if(use.fdr){
    lbf = c(paste("p >=",sig.level),paste("p <",sig.level,"(w/o FDR)"),paste("p <",sig.level,"(with FDR)"))
    
    #artificially introduce at least 1 sig/nonsig for plotting
    if(sum(fdrSigPlot == 2,na.rm = TRUE) == 0){
      fdrSigPlot <- c(fdrSigPlot,2)
      plotR <- c(plotR,3)
      
    }
    if(sum(fdrSigPlot == 1,na.rm = TRUE) == 0){
      fdrSigPlot <- c(fdrSigPlot,1)
      plotR <- c(plotR,3)
      
    }
    if(sum(fdrSigPlot == 0,na.rm = TRUE) == 0){
      fdrSigPlot <- c(fdrSigPlot,0)
      plotR <- c(plotR,3)
      
    }
    
    h <- h+geom_histogram(aes(x=plotR,y=..count..,fill = factor(fdrSigPlot)), position = 'stack', color = "white", binwidth = bw) +
      scale_fill_manual(values=alpha(bar.colors,c(0.8,0.6,0.6)), labels=lbf, guide = guide_legend(title = NULL))
    
  }else{
    bar.colors <- bar.colors[1:2]
    lbf = c(paste("p >=",sig.level),paste("p <",sig.level))
    
    #artificially introduce at least 1 sig/nonsig for plotting
    if(sum(issig,na.rm = TRUE) == 0){
      issig <- c(issig,TRUE)
      plotR <- c(plotR,3)
    }
    if(sum(!issig,na.rm = TRUE) == 0){
      issig <- c(issig,FALSE)
      plotR <- c(plotR,3)
    }
    
    h <- h+geom_histogram(aes(x=plotR,y=..count..,fill = factor(issig)), position = 'stack', color = "white", binwidth = bw) +
      scale_fill_manual(values=alpha(bar.colors,c(0.8,0.6)), labels=lbf, guide = guide_legend(title = NULL))
  }
  
  ranges <- getPlotRanges(h)
  
  x.lims <- xs
  y.lims <- ranges$y.lims
  
  #how many lines?
  if(!is.null(cor.stats)){
    lineType= rep("dashed",times = nrow(cor.stats))
    lineType[cor.stats$percentiles==.5]="solid"
    lineType[cor.stats$percentiles==.975 | cor.stats$percentiles==.025]="dotted"
    
    
    # add vertical lines at the quantiles specified in cor.stats. 
    h = h + geom_vline(data = cor.stats, aes(xintercept = r), color="red", size = 1,
                       linetype=lineType, show.legend = FALSE) +
      ylim(c(y.lims[1],y.lims[2]*1.1)) # expand vertical range
    ymax = max(y.lims)
    # annotate quantile lines. geom_label is too inflexible (no angles) so use geom_text()
    h = h + geom_text(data = cor.stats, mapping = aes(x=r, y=1.05*ymax, label=line.labels), color="red", size=3, angle=45, vjust=+2.0, hjust=0)
  }
  #customize legend
  h = h + geoChronRPlotTheme() +
    theme(legend.position = legend.position,
          legend.title = element_text(size=10, face="bold"),
          legend.text = element_text(size=8),
          legend.key = element_rect(fill = "transparent",
                                    color = "transparent"),
          legend.background = element_rect(fill=alpha('white', 0.3)))+
    coord_cartesian(xlim = xs)+
    xlab("r")+
    annotate("label",x = diff(range(x.lims))*f.sig.lab.position[1]+x.lims[1],
             y=diff(range(y.lims))*f.sig.lab.position[2]+y.lims[1], 
             label = sig_lbl,
             color=bar.colors[length(bar.colors)],
             fill = "white",
             alpha = .3,
             label.size = 0,
             size = 3)
  
  return(h)
}


#' @export
#' @family plot
#' @author Julien Emile-Geay
#' @title Plot the the p-values of an ensemble correlation analysis in a rank-pvalue plot
#' @description Plots the output of an ensemble correlation analysis as a rank-pvalue plot
#' @import ggplot2
#' @param cor.df A data.frame correlation r and p-values. Output from corEns()
#' @param alpha probability threshold
#' @return A ggplot object
plotPvalsEnsFdr = function(cor.df,alpha = 0.05){
  m = dim(cor.df)[1] # number of hypotheses being tested
  rk = seq(m)
  fdr_thresh = rk/m*alpha
  lvl_thresh = rep(alpha,m)
  pvals = sort(cor.df$pRaw)
  pvalsA = sort(cor.df$pSerial)
  # Implement this strategy: https://stackoverflow.com/questions/38962700/ggplot-legend-order-mismatch
  df <- data.frame(pvals, pvalsA, FDR = fdr_thresh, level = lvl_thresh, x = rk)
  mm <- reshape2::melt(df,id.var="x")
  lbl <- c("p-value, IID","p-value, Serial","FDR", bquote(alpha==.(alpha)))
  pvalPlot <- ggplot(data = mm, aes(x,value,color=variable,linetype=variable)) + geom_line()
  pvalPlot <- pvalPlot + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                                       labels = scales::trans_format("log10", scales::math_format(10^.x)),
                                       limits = c(1e-20,1))
  pvalPlot <- pvalPlot + scale_linetype_manual(name="Significance",values=c(1,1,2,3), labels=lbl)
  pvalPlot <- pvalPlot + scale_color_manual(name = "Significance",
                                            values=c("Chocolate1",'Chartreuse4',"black","black"),
                                            labels=lbl)+geoChronRPlotTheme()
  pvalPlot <- pvalPlot +  theme(legend.position = c(0.7, 0.4),
                                legend.title = element_text(size=10, face="bold"),
                                legend.text = element_text(size=8),
                                legend.key = element_rect(fill = "transparent",
                                                          color = "transparent"),
                                legend.background = element_rect(fill=alpha('white', 0.5)))
  # fix labels  
  pvalPlot <- pvalPlot +ylab("p-value") + xlab("rank") 
  
  return(pvalPlot)
}

#' @export
#' @family plot
#' @author Nick McKay
#' @title Plot an ensemble dataset as a histogram
#' @description Plots ensemble data as a histogram
#' @import ggplot2
#'
#' @param ens.data A data.frame of values to plot as a histogram
#' @param bins Number of bins in the histogram
#' @param line.labels Labels for the quantiles lines
#' @param add.to.plot A ggplot object to add these lines to. Default is ggplot()
#' @param quantiles a vecctor of quantiles to add the histogram
#' @param alp transparency (between 0 and 1)
#' @param font.size font size for the labels
#' @param fill fill color of the histogram, following ggplot rules
#' @param add.labels Label the quantiles?
#' @param label.vert.position Vertical position of the quantiles (from 0 to 1)
#'
#' @return A ggplot object
plotHistEns = function(ens.data,
                       quantiles=c(.025, .25, .5, .75, .975),
                       bins=50,
                       add.to.plot=ggplot(),
                       alp=1,
                       fill="grey50",
                       font.size = 10,
                       add.labels = TRUE,
                       line.labels = NA,
                       label.vert.position = .1){
  #plots a histogram of ensemble distribution values, with horizontal bars marking the distributions
  plotData = data.frame("r"=c(ens.data))
  
  
  
  histPlot = add.to.plot+
    geom_histogram(data=plotData,aes(x=r,y=..density..),color="white",bins=bins,fill=fill,alpha=alp)+
    geoChronRPlotTheme()+
    ylab("Probability density")
  if(!all(is.na(quantiles))){
    #make labels better
    lineTypes <- rep(2,times = length(quantiles))
    lineTypes[quantiles==.5] <- 1
    labelPositionY <- geoChronR::getPlotRanges(histPlot)$y.lims[2] * label.vert.position
    quants = quantile(ens.data,quantiles)
    if(all(is.na(line.labels))){
      line.labels <- names(quants)
    }
    quantdf <- data.frame(ll = names(quants),quants = quants)
    histPlot = histPlot + 
      geom_vline(data=quantdf,aes(xintercept = quants),color="red",linetype = lineTypes)+
      xlab(axisLabel(ens.data))
    
    if(add.labels){
      histPlot <- histPlot+geom_label(data = quantdf, aes(x = quants, y=labelPositionY ,label=ll),size = font.size)
    }
    
  }
  return(histPlot)
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

#' create a scree plot for PCA analysis
#'
#' @param pcaout a list of results output from pcaEns()
#' @param null.color color of the line of the null hypothesis results
#' @param null.significance Significance level to of the null to plot (default = 0.05).
#' @inheritDotParams plotTimeseriesEnsRibbons
#' @return a ggplot plot
#' @family pca
#' @family plot
#' @export
plotScreeEns <- function(pcaout,
                         null.color = "red",
                         null.significance = 0.05,
                         ...){
  
  nPCs <- nrow(pcaout$variance)
  
  nullLine <- apply(pcaout$nullVariance,1,quantile,1-null.significance)
  scree <- plotTimeseriesEnsRibbons(X = seq_len(nPCs),
                                    Y = pcaout$variance,
                                    limit.outliers.x = NA,
                                    ...) +
    ggplot2::geom_line(aes(x = seq_len(nPCs),y = nullLine),colour = null.color)+
    ggplot2::scale_x_continuous("Component number",breaks = seq_len(nPCs))+
    ggplot2::scale_y_continuous("Fraction of variance explained",limits=c(NA,NA))+
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(seq_len(nPCs),colour = "black",size = .05,linetype = 2))+
    ggtitle("PCA Scree Plot")
  
  return(scree)
}









#' @export
#' @family plot
#' @family pca
#' @author Nick McKay
#' @title Map ensemble pca loadings and plot PC timeseries
#' @description Map ensemble pca loadings and plot PC timeseries
#' @import ggplot2
#' @import ggmap
#' @importFrom gridExtra grid.arrange
#' @import mapproj
#' @param ens.pc.out results of pcaEns()
#' @param TS Timeseries object \url{http://nickmckay.github.io/LiPD-utilities/r/index.html#what-is-a-time-series} used in the pcaEns() analysis
#' @param map.type "google" or "line"
#' @param f zoom buffer for plotting
#' @param which.pcs vector of PCs to plot. Choose two. c(1,2) is default.
#' @param high.color color for the high end of the scale
#' @param low.color color for the low end of the scale
#' @param color deprecated. Use high.color and low.color instead
#' @param dot.size How big are the dots on the map
#' @param restrict.map.range TRUE or FALSE. Trim the size of the map to the points, for "line" map type
#' @param shape.by.archive TRUE or FALSE. Use archiveType to assign shapes.
#' @param projection Map project. All options on: ?mapproject
#' @param bound.circ For polar projects, draw a boundary circle? TRUE or FALSE
#' @param probs quantiles to calculate and plot in the PC timeseries
#' @param which.leg which map legend to include in the summary plot?
#' @param legend.position Where to put the map legend?
#' @return A gridExtra ggplot object
plotPcaEns = function(ens.pc.out,
                      TS,
                      map.type="line",
                      which.pcs=c(1,2),
                      f=.2,
                      high.color = "red",
                      low.color = "blue",
                      dot.size=5,
                      restrict.map.range=TRUE,
                      shape.by.archive=TRUE,
                      projection="mollweide",
                      bound.circ=TRUE,
                      probs=c(.025, .25, .5, .75, .975),
                      which.leg = 1,
                      legend.position = c(0.5,0.5),
                      color){#now deprecated
  
  #check to see if TS is a tibble
  if(tibble::is_tibble(TS)){#convert back to TS
    TS <- lipdR::untidyTs(TS)
  }
  #Deprecation check
  if(!missing(color)){
    stop("argument `color` is deprecated; please use `high.color` and `low.color` instead.")
  }
  
  
  #get data out of the TS
  lat <- pullTsVariable(TS,"geo_latitude")
  lon <- pullTsVariable(TS,"geo_longitude")
  archive <- pullTsVariable(TS,"archiveType")
  ageUnits <- pullTsVariable(TS,"ageEnsembleUnits")
  
  if(length(unique(ageUnits))>1){
    warning("uh oh, looks like you have multiple units for your age ensemble.")
  }
  
  ageUnits <- ageUnits[1]
  
  #shape by archive!###
  arch.shape=c()
  for(i in 1:length(archive)){
    if(shape.by.archive){
      if (grepl(x=tolower(archive[i]),"lake")){arch.shape[i]="lake"}
      else if (grepl(x=tolower(archive[i]),"marine")){arch.shape[i]="marine"}
      else if (grepl(x=tolower(archive[i]),"speleothem")){arch.shape[i]="speleothem"}
      else if (grepl(x=archive[i],"ice",ignore.case = T)){arch.shape[i]="glacier ice"}
      else {arch.shape[i]="unknown"}
    }else{arch.shape[i]=21} #make them all circles if not shape.by.archive
  }
  
  arch.shape=factor(arch.shape)
  archiveShapes=c(21,22,24,23,25)
  if(!any(grepl(pattern="unknown",arch.shape))){archiveShapes = archiveShapes[-4] }
  if(!any(grepl(pattern="ice",arch.shape))){archiveShapes = archiveShapes[-4] }
  if(!any(grepl(pattern="speleothem",arch.shape))){archiveShapes = archiveShapes[-3] }
  if(!any(grepl(pattern="marine",arch.shape))){archiveShapes = archiveShapes[-2] }
  if(!any(grepl(pattern="lake",arch.shape))){archiveShapes = archiveShapes[-1] }
  
  if(length(archiveShapes) == 0){
    archiveShapes=c(21)
  }
  #end shape by archive
  
  
  
  plotlist=list()
  maplist=list()
  leglist <- list()
  
  #   sorted =  apply(dat.mat[[wm]]$PC$ensemblePCs[,1,],MARGIN = c(2),sort)
  median.pcs = apply(ens.pc.out$PCs,MARGIN = c(1,2),median,na.rm=TRUE)
  loadingSDs = apply(ens.pc.out$loadings,MARGIN = c(1,2),sd,na.rm=TRUE)
  medianLoadings = apply(ens.pc.out$loadings,MARGIN = c(1,2),median,na.rm=TRUE)
  
  #get a base map
  map = baseMap(lon,lat,map.type = map.type,f=f,projection = projection,restrict.map.range = restrict.map.range)
  
  
  
  for (i in 1:length(which.pcs)){
    #figure out dot.size
    sdRange = range(loadingSDs[,which.pcs[i]])
    medianRange = abs(diff(range(medianLoadings[,which.pcs[i]])))
    sdPct = 2*loadingSDs[,which.pcs[i]]/medianRange
    sdDots = sdPct
    
    
    #make a data.frame to plot
    dd=data.frame(lon=lon,lat=lat,medLoad=medianLoadings[,which.pcs[i]],sdDots=sdDots,shape=factor(arch.shape))
    #sort by dot size
    # print(order(sdDots))
    dd = dd[order(sdDots),]
    row.names(dd)=1:nrow(dd)
    
    #assign colors
    scaleColors <- c(low.color,high.color)
    
    #get color range
    crange <- c(-max(abs(medianLoadings[,which.pcs[i]])),max(abs(medianLoadings[,which.pcs[i]])))
    
    
    
    
    maplist[[i]] = map +  geom_point(aes(x=lon,y=lat,fill=medLoad,size=sdDots,shape = shape), data=dd)+theme(legend.box = "horizontal",legend.position=legend.position)
    
    
    testMap <- map +  geom_point(aes(x=lon,y=lat,fill=medLoad,size=sdDots,shape = shape), data=dd)+
      theme(legend.box = "horizontal",legend.position=legend.position) + 
      scale_shape_manual(name = "Archive Type",values = archiveShapes) +
      scale_size(name = "Loading uncertainty",range = c(dot.size,1)) +
      scale_fill_gradient2(name = "Loadings",
                           low = scaleColors[1],
                           high = scaleColors[2],
                           guide = "colorbar",limits = crange)
    
    
    leglist[[i]] <- getLegend(testMap)
    
    
    
    #if(i>1 & !repeatMapLegend){#Don't repeat all the legend components
    if(TRUE){
      maplist[[i]] <- maplist[[i]] +
        scale_shape_manual(values = archiveShapes,guide="none") +
        scale_size(name = "Loading uncertainty",range = c(dot.size,1),guide="none") +
        scale_fill_gradient2(name="Loadings",low=scaleColors[1],high=scaleColors[2],guide="none")
    }else{#plot the map legends
      maplist[[i]] <- maplist[[i]] +
        scale_shape_manual(name = "Archive Type",values = archiveShapes) +
        scale_size(name = "Loading uncertainty",range = c(dot.size,1)) +
        scale_fill_gradient2(name="Loadings",low=scaleColors[1],high=scaleColors[2],guide="colorbar")
    }
    
    
    
    
    
    
    plotlist[[i]] = plotTimeseriesEnsRibbons(X=ens.pc.out$age,Y=ens.pc.out$PCs[,which.pcs[i],],x.bin =ens.pc.out$age,n.bins = 10000 ,probs = probs) 
    medianVarExp = median(ens.pc.out$variance[which.pcs[i],])
    sdVarExp = sd(ens.pc.out$variance[which.pcs[i],])
    varExpStr  = paste(as.character(signif(medianVarExp*100,2)),"+/-",as.character(signif(sdVarExp*100,1)))
    
    plotlist[[i]] = plotlist[[i]]+ggtitle(paste("Variance explained =",varExpStr,"%"))
    
    if(grepl(pattern = "AD",ageUnits) | grepl(pattern = "CE",ageUnits) ){
      plotlist[[i]] = plotlist[[i]] + labs(y=paste0("PC",which.pcs[i]),x="Year (AD)")
    }else{
      plotlist[[i]] = plotlist[[i]] +
        scale_x_reverse()+
        labs(y=paste0("PC",which.pcs[i]),x="Age (yr BP)")
    }
  }
  
  #plot sample depth
  
  bddf = data.frame(sampleDepth = ens.pc.out$meanDataDensity*100,age = ens.pc.out$age)
  
  plot_sample.depth = ggplot(data=bddf)+geom_area(aes(x=age,y=sampleDepth),fill="gray20")+
    ylab("Data coverage (%)")+
    geoChronRPlotTheme()
  
  if(grepl(pattern = "AD",ageUnits) | grepl(pattern = "CE",ageUnits) ){
    plot_sample.depth  <- plot_sample.depth  + labs(x="Year (AD)")
  }else{
    plot_sample.depth  <- plot_sample.depth  +
      scale_x_reverse("Age (yr BP)")
  }
  alllist = vector(mode = "list",length = length(maplist)*2)
  for(aa in 1:(length(maplist)*2)){
    if(aa%%2==1){#if odd
      alllist[[aa]] <- maplist[[ceiling(aa/2)]]
    }else{
      alllist[[aa]] <- plotlist[[aa/2]]
    }
  }
  #alllist = append(maplist,plotlist)
  # tt=1:length(alllist)
  # alllist = alllist[c(tt[tt%%2==1],tt[tt%%2==0])]
  #append on the legend
  alllist[[length(alllist)+1]] <- leglist[[which.leg]]
  #append data density
  alllist[[length(alllist)+1]] <- plot_sample.depth
  
  
  fullPlot = grid.arrange(grobs=alllist,nrow=length(maplist)+1,widths=c(1.5,1.5))
  
  
  
  return(list(lines = plotlist, maps= maplist,summary =fullPlot,sampleDepth = plot_sample.depth,legends = leglist))
  
}

#' @export
#' @family plot
#' @family chron
#' @author Nick McKay
#' @title Plot probability distributions
#' @description Plot or add probability distributions from a paleo or chron model to a plot. 
#' @import ggplot2
#' @inheritParams selectData
#' @param dist.var Name of the distribution variable, will be plotted along the x-axis. Use coord_flip() after running the function if you want vertical distributions. "age" by default. 
#' @param y.var Name of the y-axis variable. "depth" by default. 
#' @param mode chron or paleo 
#' @param paleo.or.chron.num number of the chron or paleo Data object
#' @param model.num number of the model object
#' @param color distribution color (following ggplot rules)
#' @param dist.plot vector of distribution tables to plot
#' @param dist.type "violin" (default), "high" for one-sided distributions towards higher depths, "low" for one-sided distributions towards lower depths
#' @param thick thickness of the line around the distribution
#' @param truncate.dist truncate probability density values below this number. NA (default) means no truncation
#' @param scale.frac controls the vertical span of the probability distribution. Approximately the vertical fraction of the plot that the distribution will cover. 
#' @param add.to.plot A ggplot object to add this plot to. Default is ggplot()
#' @param alp transparency, from 0 to 1
#' @return A ggplot object
plotModelDistributions = function(L,
                                  dist.var = "age",
                                  y.var = "depth",
                                  mode = "chron",
                                  paleo.or.chron.num = 1, 
                                  model.num = 1, 
                                  add.to.plot = ggplot(), 
                                  alp=.5,
                                  color = "purple",
                                  scale.frac = 0.02,
                                  dist.plot = NA,
                                  dist.type = "violin",
                                  thick = 0.1,
                                  truncate.dist = NA){
  
  
  P = L[[paste0(mode,"Data")]]
  if(is.na(paleo.or.chron.num)){
    if(length(P)==1){
      paleo.or.chron.num=1
    }else{
      print(names(P))
      paleo.or.chron.num=as.integer(readline(prompt = "Which paleoData do you want to put this age ensemble in? Select a number "))
    }
  }
  
  #initialize model number
  MT = P[[paleo.or.chron.num]]$model
  if(is.null(MT)){
    stop(paste0("There are no models in ",mode,"Data[[",as.character(paleo.or.chron.num),"]]. This makes it difficult to plot distributions from the model"))
  }
  
  if(is.na(model.num)){
    if(length(MT)==1){
      #only one pmt
      meas.table.num=1
    }else{
      print(paste0(paleo.or.chron,"Data[[", as.character(paleo.num), "]] has ", length(MT), " models"))
      meas.table.num=as.integer(readline(prompt = "Which measurement table do you want to put the ensemble in? Enter an integer "))
    }
  }
  
  
  #pull out distribution object
  dist = MT[[model.num]]$distributionTable
  
  #check it to make sure it's a distribution table
  if(!is.list(dist)){
    stop("This doesn't seem to be a valid distribution table with these settings")
  }
  
  #if not specified, plot all distributions
  if(is.na(dist.plot)){
    dist.plot = 1:length(dist)
  }
  
  #pull out all the yaxis data to get range and scale
  ally = sapply(dist[dist.plot],"[[",y.var)
  
  # get range and scale
  plot.range =range(ally,na.rm = T)
  
  #guess at the scaler...
  this.dist = dist[[dist.plot[[1]]]]
  if(!is.na(truncate.dist)){
    tgood = which(this.dist$probabilityDensity$values > truncate.dist)
    this.dist$probabilityDensity$values = this.dist$probabilityDensity$values[tgood]
    this.dist$age$values = this.dist$age$values[tgood]
  }
  
  
  #loop through individual ages...
  for(y in dist.plot){
    this.dist = dist[[y]]
    if(!is.na(truncate.dist)){
      tgood = which(this.dist$probabilityDensity$values > truncate.dist)
      this.dist$probabilityDensity$values = this.dist$probabilityDensity$values[tgood]
      this.dist$age$values = this.dist$age$values[tgood]
    }
    pd = this.dist$probabilityDensity$values/sum(this.dist$probabilityDensity$values,na.rm=T)
    scaler = scale.frac*abs(diff(plot.range))/max(pd)
    pd = pd * scaler
    this.df = data.frame(x= this.dist[[dist.var]]$values,ymin = this.dist[[y.var]] - pd,ymax = this.dist[[y.var]] + pd )
    if(dist.type == "up" | dist.type == "high"){this.df$ymin =  this.dist[[y.var]]}
    if(dist.type == "down" | dist.type == "low"){this.df$ymax =  this.dist[[y.var]]}
    add.to.plot = add.to.plot + geom_ribbon(data = this.df, aes(x = x,ymin = ymin,ymax = ymax),color = color,fill = color, alpha = alp,size = thick)
  }
  add.to.plot = add.to.plot + geoChronRPlotTheme()
  return(add.to.plot)
}


#' @export
#' @family plot
#' @family chron
#' @author Nick McKay
#' @title Compare chron ensemble with paleoData age-model
#' @description Plots the difference of an chron ensembleTable with the paleoData age
#' @import ggplot2
#' @inheritParams selectData
#' @param ageEnsVar name of the age ensemble variable in the chronData to search for
#' @param age.var name of the age variable in the paleoData to search for
#' @param depth.var name of the depth variable to search for
#' @param paleo.num an integer that corresponds to paleo.numData object (L$paleoData[[?]]) has the measurementTable you want to modify
#' @param paleo.meas.table.num an integer that corresponds to paleo.num measurementTable you want to add the ensemble to?
#' @param chron.num  an integer that corresponds to chron.numData object (L$crhonData[[?]]) has the model you want to get the ensemble from
#' @param model.num an integer that corresponds to chron.num model you want to get the ensemble from?
#' @param ens.table.num an integer that corresponds to chron.num model ensembleTable you want to get the ensemble from?
#' @param max.ensemble.members Maximum number of ensemble members to map
#' @param strict.search Use a strict.search to look for the ageEnsemble and depth variables. TRUE(default) or FALSE   #' @param probs quantiles to calculate and plot
#' @param n.bins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param y.bin vector of bin edges over which to bin.
#' @param color.low Band color of the outer most band.
#' @param color.high Band color of the inner most band.
#' @param alp Transparency of the band plot
#' @param color.line Line color (following ggplot rules)
#' @param line.width Width of the line
#' @param add.to.plot A ggplot object to add this plot to. Default is ggplot() . 
#' @param n.ens.plot Number of ensemble members to plot
#' @param color.ens.line color of the ensemble lines
#' @param alp.ens.line transparency of the lines
#' @param probs quantiles to plot with ribbons
#' @return A ggplot object
plotChronEnsDiff = function(L,
                            ageEnsVar = "ageEnsemble",
                            age.var = "age",
                            depth.var = "depth",
                            paleo.num=NA,
                            paleo.meas.table.num=NA,
                            chron.num=NA,
                            model.num=NA,
                            ens.table.num = NA,
                            max.ensemble.members=NA,
                            strict.search=FALSE,
                            probs=c(0.025,.25,.5,.75,.975),
                            x.bin=NA,
                            y.bin=NA,
                            n.bins=100,
                            color.low="white",
                            color.high="grey70",
                            alp=1,
                            color.line="Black",
                            line.width=1,
                            add.to.plot=ggplot2::ggplot(),
                            n.ens.plot = 5,
                            color.ens.line = "red",
                            alp.ens.line = 0.7){
  
  
  L <- mapAgeEnsembleToPaleoData(L, age.var = ageEnsVar,chron.depth.var = depth.var,paleo.num = paleo.num, chron.num = chron.num, model.num = model.num,paleo.meas.table.num = paleo.meas.table.num, ens.table.num = ens.table.num)
  
  
  #get the paleo and chron Ensemble ages
  pAge <- selectData(L,var.name = age.var,paleo.or.chron.num = paleo.num,meas.table.num = paleo.meas.table.num)
  cAgeEns <- selectData(L,var.name = ageEnsVar,paleo.or.chron = "paleoData",paleo.or.chron.num = paleo.num,meas.table.num = paleo.meas.table.num)
  
  if(is.null(pAge)){
    stop("couldn't find the age/year paleoData")
  }
  if(is.null(cAgeEns)){
    stop("couldn't find the mapped ageEnsemble/yearEnsemble data in paleoData")
  }
  
  #calculate the difference
  ageDiff <- list()
  ageDiff$variableName <- paste0("Delta ",pAge$variableName)
  ageDiff$units <- pAge$units
  ageDiff$values <- pAge$values - cAgeEns$values
  axisLabel(ageDiff)
  
  
  #see if there's depth
  depth <- selectData(L,var.name = depth.var,paleo.or.chron.num = paleo.num,meas.table.num = paleo.meas.table.num)
  
  #if no depth then use age
  if(is.null(depth)){
    depth <- pAge
  }
  
  
  diffPlot <- plotTimeseriesEnsRibbons(X = depth ,Y = ageDiff,,alp = alp,probs = probs,x.bin = x.bin,y.bin = y.bin, n.bins = n.bins, color.low = color.low,color.high = color.high,color.line = color.line,line.width = line.width,add.to.plot = add.to.plot)
  
  #add some traces
  diffPlot <- plotTimeseriesEnsLines(add.to.plot = diffPlot,X = depth ,Y = ageDiff,alp = alp.ens.line,color = color.ens.line,n.ens.plot = n.ens.plot)
  
  return(diffPlot)
  
}




#' @export
#' @family plot
#' @family chron
#' @author Nick McKay
#' @title High-level chron plotting
#' @description Plot a chronology, either from a chron model (preferred) or from a chron measurement table if there is no model
#' @import ggplot2
#' @inheritDotParams plotChronEns
#' @inheritParams selectData
#' @param depth.var variableName to use for depth ("depth" by default)
#' @param age.var variableName to use for age ensemble ("age" by default)
#' @param age.14c.var variableName to use for age ensemble ("age14C" by default)
#' @param chron.number chron.numData object to use (NA by default, will ask if needed)
#' @param meas.num chron.numData model to use (NA by default, will ask if needed)
#' @param dot.size what size dot for the chron plot? Only used if not plotting by plotChronEns() (default = 5)
#' @param legend.position where to put the legend on the chron plot?
#' @return a ggplot object, or NA if there's chronData to plot
plotChron <- function(L,
                      chron.number = NA, 
                      meas.num = NA,
                      depth.var = "depth", 
                      age.var = "age",
                      age.14c.var = "age14C", 
                      dot.size = 5,
                      legend.position = c(0.7,0.3), ...){
  #grab the chronData 
  C = L$chronData
  
  #there must be a chronData to proceed
  if(is.null(C)){
    warning("Must have chronData to proceed. Exiting...")
    return(NA)
  }
  
  #figure out a chron.number
  if(is.na(chron.number)){
    if(length(C)==1){
      chron.number = 1
    }else{
      print(paste0("There are ", as.character(length(C)), " chronData objects. Which do you want to plot?"))
      chron.number=as.integer(readline(prompt = "Which chronData do you want to plot? Enter an integer "))
    }
  }
  
  #is there a model?
  if(!is.null({C[[chron.number]]$model[[1]]$ensembleTable})){#then use plotChronEns!
    chronPlot <- plotChronEns(L,chron.number = chron.number, depth.var = depth.var, age.var = age.var,...)+
      theme(legend.position = "none") 
  }else{#make a simpler plot from the measurementTable
    #look for the measurementTable
    if(is.null(C[[chron.number]]$measurementTable)){
      warning("No chron model, or measurementTable. Exiting...")
      return(NA)
    }
    
    #figure out a measurementTable number
    if(is.na(meas.num)){
      if(length(C[[chron.number]]$measurementTable)==1){
        meas.num = 1
      }else{
        print(paste0("There are ", as.character(length(C[[chron.number]]$measurementTable)), " chron measurement tables. Which do you want to plot?"))
        meas.num=as.integer(readline(prompt = "Which model do you want to plot? Enter an integer "))
      }
    }
    
    #get depth
    depth <- selectData(L,paleo.or.chron = "chronData",paleo.or.chron.num = chron.number, meas.table.num = meas.num, var.name = depth.var)
    
    #get age
    age <- selectData(L,paleo.or.chron = "chronData",paleo.or.chron.num = chron.number, meas.table.num = meas.num, var.name = age.var)
    #get 14Cage
    age14C <- selectData(L,paleo.or.chron = "chronData",paleo.or.chron.num = chron.number, meas.table.num = meas.num, var.name = age.14c.var)
    
    if(!is.null(age) & is.null(age14C)){
      ageDf <- data.frame(age = age$values, ageType = "calibratedAge")
    }else if(is.null(age) & !is.null(age14C)){
      ageDf <- data.frame(age = age14C$values, ageType = "14C Age")
    }else if(!is.null(age) & !is.null(age14C)){
      ageDf14C <- data.frame(age = age14C$values, ageType = "14C Age",stringsAsFactors = FALSE)
      ageDf <- data.frame(age = age$values, ageType = "calibrated age",stringsAsFactors = FALSE)
      naCal <- which(is.na(ageDf$age))
      ageDf[naCal, ] <- ageDf14C[naCal, ] 
    }else{
      stop("couldn't find any age data in chron measurement table")
    }
    
    ageDf$depth <- depth$values
    chronPlot <- ggplot(ageDf)+geom_point(aes(x = age, y = depth, color = ageType), size = dot.size)+
      scale_y_reverse(name = axisLabel(depth))+
      geoChronRPlotTheme() +  theme(legend.position = legend.position) +
      ggtitle(paste0(L$dataSetName,": chronData ", as.character(chron.number), " - measurementTable ", as.character(meas.num)))
    
    if(any(grepl("AD",age$units)) | any(grepl("CE",age$units))){
      chronPlot <- chronPlot + scale_x_continuous(name = axisLabel(age))
    }else{
      chronPlot <- chronPlot + scale_x_reverse(name = axisLabel(age))
    }
  }
  
  return(chronPlot)
  
}

#' @export
#' @family plot
#' @family chron
#' @author Nick McKay
#' @title Plot chron ensemble
#' @description Plot creates an age model plot with all the bells and whistles, including a spread of ensemble members, probability distributions, and a few example ensemble members. 
#' @import ggplot2
#' @inheritParams selectData
#' @param depth.var variableName to use for depth ("depth" by default)
#' @param age.var ariableName to use for age ensemble ("ageEnsemble" by default)
#' @param chron.number chron.numData object to use (NA by default, will ask if needed)
#' @param model.num chron.numData model to use (NA by default, will ask if needed)
#' @param probs quantiles to calculate and plot
#' @param n.bins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param y.bin vector of bin edges over which to bin.
#' @param color.low Band color of the outer most band.
#' @param color.high Band color of the inner most band.
#' @param alp Transparency of the band plot
#' @param color.line Line color (following ggplot rules)
#' @param line.width Width of the line
#' @param add.to.plot A ggplot object to add this plot to. Default is ggplot() . 
#' @param n.ens.plot Number of ensemble members to plot
#' @param color.ens.line color of the ensemble lines
#' @param alp.ens.line transparency of the lines
#' @param dist.color distribution color (following ggplot rules)
#' @param dist.type "violin" (default), "up" for one-sided distributions pointed up, "down" for one-sided distributions pointed down
#' @param dist.thick thickness of the line around the distribution
#' @param dist.alp alpha of the distribution
#' @param truncate.dist truncate probability density values below this number. NA (default) means no truncation
#' @param dist.scale controls the vertical span of the probability distribution. Approximately the vertical fraction of the plot that the distribution will cover. 
#' @param add.paleo.age.depth add a line that shows the paleoData age depth.
#' @param paleo.number paleo.num number for the paleoData age-depth
#' @param meas.num which measurement Table for the paleoData age-depth
#' @param color.line.paleo line color of the paleoData age-depth (following ggplot rules)
#' @param plot.traces Add timeseries lines to the plot (default = TRUE)
#' @param ... Pass parameters to plotTimeseriesEnsRibbons
#' @return A ggplot object
plotChronEns = function(L,
                        age.var = "ageEnsemble",
                        depth.var = "depth",
                        chron.number=NA,
                        model.num = NA,
                        probs=c(0.025,.25,.5,.75,.975),
                        x.bin=NA,
                        y.bin=NA,
                        n.bins=100,
                        color.low="white",
                        color.high="grey70",
                        alp=1,
                        color.line="Black",
                        line.width=1,
                        add.to.plot=ggplot2::ggplot(),
                        n.ens.plot = 5,
                        color.ens.line = "red",
                        alp.ens.line = 0.7,
                        dist.alp = 0.3,
                        dist.type = "violin",
                        dist.color = "purple",
                        dist.thick = 0.1,
                        dist.scale = 0.02,
                        truncate.dist = NA,
                        add.paleo.age.depth = FALSE, 
                        paleo.number = NA, 
                        meas.num = NA,
                        color.line.paleo = "cyan",
                        plot.traces = TRUE,
                        ...){
  
  C = L$chronData
  if(is.na(chron.number)){
    if(length(C)==1){
      chron.number = 1
    }else{
      print(paste0("There are ", as.character(length(C)), " chronData objects. Which do you want to plot?"))
      chron.number=as.integer(readline(prompt = "Which chronData do you want to plot? Enter an integer "))
    }
  }
  
  if(is.na(model.num)){
    if(length(C[[chron.number]]$model)==1){
      model.num = 1
    }else{
      print(paste0("There are ", as.character(length(C[[chron.number]]$model)), " chron models. Which do you want to plot?"))
      model.num=as.integer(readline(prompt = "Which model do you want to plot? Enter an integer "))
    }
  }
  
  #check for ensemble table. For now this is required to plot.
  if(!any(grepl("ensembleTable",names(L$chronData[[chron.number]]$model[[model.num]])))){
    stop("No ensemble table found. At this time, plotChronEns() only works with chronData objects with ensemble tables.")
  }
  
  if(add.paleo.age.depth){#then add a line that shows depth vs age in the paleoTable
    P <- L$paleoData
    if(is.na(paleo.number)){
      if(length(P)==1){
        paleo.number = 1
      }else{
        print(paste0("There are ", as.character(length(P)), " paleoData objects. Which do you want to plot?"))
        paleo.number=as.integer(readline(prompt = "Which chronData do you want to plot? Enter an integer "))
      }
    }
    
    if(is.na(meas.num)){
      if(length(P[[paleo.number]]$measurementTable)==1){
        meas.num = 1
      }else{
        print(paste0("There are ", as.character(length(P[[paleo.number]]$measurementTable)), " paleo models. Which do you want to plot?"))
        meas.num=as.integer(readline(prompt = "Which model do you want to plot? Enter an integer "))
      }
    }
    
    #get the data from the paleo measurement table
    pDepth = selectData(L,var.name = "depth",paleo.or.chron = "paleoData",table.type = "measurement",meas.table.num = meas.num,paleo.or.chron.num = paleo.number)
    pAge = selectData(L,var.name = "age",paleo.or.chron = "paleoData",table.type = "measurement",meas.table.num = meas.num,paleo.or.chron.num = paleo.number)
    
    
  }
  
  
  
  
  #get the data from the chron ensemble table
  depth = selectData(L,var.name = depth.var,paleo.or.chron = "chronData",table.type = "ensemble",model.num = model.num,paleo.or.chron.num = chron.number)
  ageEnsemble = selectData(L,var.name = age.var,paleo.or.chron = "chronData",table.type = "ensemble",model.num = model.num,paleo.or.chron.num = chron.number)
  
  #if there's no depth, just plot by an index
  if(is.null(depth)){
    depth <- list()
    depth$values <- seq_len(nrow(ageEnsemble$values))
    depth$variableName <- "Index"
    depth$units <- "NA"
  }
  
  
  #quick fix to ensemble list bug
  ageEnsemble$values = as.matrix(as.data.frame(ageEnsemble$values))
  
  print("plotting your chron ensemble. This make take a few seconds...")
  
  #Ribbons first
  chronPlot = plotTimeseriesEnsRibbons(X = ageEnsemble,Y = depth,alp = alp,probs = probs,x.bin = x.bin,y.bin = y.bin, n.bins = n.bins, color.low = color.low,color.high = color.high,color.line = color.line,line.width = line.width,add.to.plot = add.to.plot,...)
  
  
  if(plot.traces){
    #A few traces second
    chronPlot = plotTimeseriesEnsLines(X = ageEnsemble,Y = depth,alp = alp.ens.line,color = color.ens.line,add.to.plot = chronPlot,n.ens.plot = n.ens.plot)
  }
  
  #distributions last
  if(is.list(C[[chron.number]]$model[[model.num]]$distributionTable)){#if it exists. Add it.
    chronPlot = plotModelDistributions(L,paleo.or.chron.num = chron.number,model.num = model.num,add.to.plot = chronPlot,alp=dist.alp,color = dist.color,dist.type = dist.type,thick = dist.thick,scale.frac = dist.scale,truncate.dist = truncate.dist)
  }
  
  
  #Compare with the paleoData depth-age ensemble
  if(add.paleo.age.depth){
    chronPlot <- chronPlot+geom_line(aes(x = pAge$values, y = pDepth$value), color = color.line.paleo)
  }
  
  
  
  #Tidy up...
  chronPlot = chronPlot + 
    scale_y_reverse(name = axisLabel(depth)) + 
    ggtitle(paste0(L$dataSetName)) + 
    theme(legend.position = "none")
  geoChronRPlotTheme()
  
  return(chronPlot)
  
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

#' @author Nick McKay
#' @family plot help
#' @title Melt distribution
#' @description Takes a LiPD model distribution and melt it into a single data.frame
#' @param this.dist LiPD "distributionTable" object
#' @param dist.plot vector of distribution tables to plot
#' @return data.frame of melted distribution objects.
#' @export
meltDistributionTable = function(this.dist,dist.plot = 1:length(this.dist)){
  #create large dataframe from dist object
  dist.df = NULL
  nDist = length(dist.plot)
  for(i in dist.plot){
    this.df = list()
    this.dist = dist[[i]]
    #lists first
    ll = which(sapply(this.dist,is.list))
    for(l in ll){
      this.name = names(this.dist)[l] 
      this.df[[this.name]] = this.dist[[l]]$values
    }
    #convert to df
    this.df = as.data.frame(this.df)
    
    ln = which(!sapply(this.dist,is.list))
    for(l in ln){
      this.name = names(this.dist)[l] 
      this.df[this.name] = this.dist[[l]]
    }
    
    if(is.null(dist.df)){
      dist.df = this.df
    }else{
      dist.df = rbind(dist.df,this.df)
    }
    dist.df = rbind(dist.df,rep(NA,ncol(this.df)))
  }
  return(dist.df)
}

#' @export
#' @family plot
#' @family regress
#' @author Nick McKay
#' @title Plot ensemble regression results
#' @description Creates a suite of plots to characterize the results of an ensemble regression.
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#'
#' @param reg.ens output of regressEns()
#' @param alp Transparency of the scatter plot.
#' @param font.size Font size
#' @param quantiles quantiles to calculate and plot
#'
#' @return A list of ggplot objects
#' \itemize{
#' \item YPlot - ribbon plot of the prectictand timeseries over the interval of overlap
#' \item XPlot - ribbon plot of the predictor timeseries over the interval of overlap
#' \item scatterplot - ensemble scatter plot of the predictor and predictand timeseries over the interval of overlap
#' \item mHist - distribution of ensemble regression slopes
#' \item bHist - distribution of ensemble regression intercepts
#' \item modeledYPlot - ribbon plot of values modeled by the ensemble regression, incorporating age uncertainty in both the regression and the predictor timeseries
#' \item summaryPlot - grid.arrange object of all the regression plots
#' }
plotRegressEns = function(reg.ens,
                          alp=0.2,
                          quantiles = c(0.025, .5, .975),
                          font.size = 10){
  regPlot = list()
  #scatter plot
  scatterplot = plotScatterEns(X = reg.ens$binX,Y = reg.ens$binY,alp=alp)
  #add trendlines
  scatterplot = plotTrendLinesEns(mb.df = t(rbind(reg.ens$m,reg.ens$b)),x.range = range(reg.ens$binX,na.rm=TRUE), alp = alp,add.to.plot = scatterplot)
  
  
  scatterplot = scatterplot + xlab(axisLabel(reg.ens$values.x)) + ylab(axisLabel(reg.ens$values.y))
  
  #assign scatter plot to out list
  regPlot$scatterplot = scatterplot + theme(text = element_text(size = font.size))
  
  
  
  #plot histograms of m and b
  mStats = reg.ens$regStats[,1:2]
  names(mStats)[2]="values"
  regPlot$mHist = plotHistEns(reg.ens$m,add.labels = FALSE,quantiles = quantiles,font.size = font.size*.25)+xlab("Slope") + theme(text = element_text(size = font.size))
  bStats = reg.ens$regStats[,c(1,3)]
  names(bStats)[2]="values"
  regPlot$bHist = plotHistEns(reg.ens$b,add.labels = FALSE,quantiles = quantiles,font.size = font.size*.25)+xlab("Intercept") + theme(text = element_text(size = font.size))
  
  binY = reg.ens$binY
  binX = reg.ens$binX
  
  binY[is.nan(binY)]=NA
  binX[is.nan(binX)]=NA
  
  #plot timeseries of regression and target over interval
  regPlot$XPlot = plotTimeseriesEnsRibbons(X = reg.ens$yearX,Y = reg.ens$binX,n.bins = length(reg.ens$yearX))+ggtitle("Calibration predictor")+xlab(axisLabel(reg.ens$time.x))+ylab(axisLabel(reg.ens$values.y)) + theme(text = element_text(size = font.size))
  
  regPlot$YPlot = plotTimeseriesEnsRibbons(X = reg.ens$yearX,Y = reg.ens$binY,color.high = "red",n.bins = length(reg.ens$yearX))+ggtitle("Calibration predictand")+xlab(axisLabel(reg.ens$time.y))+ylab(axisLabel(reg.ens$values.y)) + theme(text = element_text(size = font.size))
  
  
  
  #and plot reconstructions
  if(!is.list(reg.ens$modeledYear)){
    modYear = list()
    modYear$values = reg.ens$modeledYear
    modYear$units = reg.ens$time.x$units
    modYear$variableName = reg.ens$time.x$variableName
  }else{
    modYear = reg.ens$modeledYear
  }
  regPlot$modeledYPlot = plotTimeseriesEnsRibbons(X = modYear,Y=reg.ens$modeled)+
    ggtitle("Calibrated record using ensemble regression") + 
    theme(text = element_text(size = font.size))
  
  
  
  lay = rbind(c(1,1,3,3,4,4),
              c(2,2,3,3,5,5),
              c(6,6,6,6,6,6),
              c(6,6,6,6,6,6))
  
  
  regPlot$summaryPlot <- gridExtra::grid.arrange(grobs = list(regPlot$YPlot,regPlot$XPlot,regPlot$scatterplot,
                                                              regPlot$mHist,regPlot$bHist,regPlot$modeledYPlot),
                                                 layout_matrix=lay)  
  
  return(regPlot)
}

#' @export
#' @family plot
#' @author Nick McKay
#' @title Plot a bunch of timeseries in a vertical stack
#' @description Creates a suite of plots to characterize the results of an ensemble regression.
#' @import ggplot2
#' @importFrom ggridges geom_ridgeline theme_ridges
#' @import dplyr
#' @import RColorBrewer
#' @import grDevices
#' @import scales
#'
#' @param plot.df A tidy data.frame, typically the output of tidyTs()
#' @param time.var Which variable to put on the x-axis. Must be in plot.df. Typically "year", "age", or "depth"
#' @param color.var Which variable to color the timeseries by. The default ("paleoData_TSid") will give each timeseries it's own color. Common other options include "paleoData_variable", "archiveType", or "paleoData_units", but any variable in plot.df should work.
#' @param invert.var Which variable to use to invert the timeseries. This should point to a variable of "positive" and "negative" (searches on "neg"), or a vector of 1s and -1s. (default = NA, which flips nothing)
#' @param fill.alpha Transparency of the shading
#' @param scale.factor Controls how much the timeseries should overlap, with larger numbers overlapping more. (default = 1/3)
#' @param scale.height Controls how large the y-axes will be. 1 is equivalent to end-to-end coverage with no space. (default = 0.75)
#' @param lab.buff Fraction of the x axis to space the tick marks away from the axes bars (default = 0.02)
#' @param lab.size Font size for the ylabels
#' @param line.size thickness of the line (default = 0.5)
#' @param color.ramp Specify the colors to use in the plot arranged along color.var. (default = function(nColors){RColorBrewer::brewer.pal(nColors,"Dark2")})
#' @param lab.space Multiplier on lab.buff for the axis label separation from the y-scale
#'
#' @return A ggplot object of the plot
#' @section Long-form example:
#' \href{http://nickmckay.github.io/GeoChronR/articles/PlotTimeseriesStack.html}{View a full-fledged example of how to use this function.} 
plotTimeseriesStack <- function(plot.df,
                                time.var = "year", 
                                color.var = "paleoData_TSid", 
                                invert.var = NA,
                                fill.alpha = 0.2, 
                                line.size = 0.5,
                                scale.factor = 1/3,
                                scale.height = .75, 
                                lab.buff = 0.02, 
                                lab.size = 3,  
                                lab.space= 2,
                                color.ramp = function(nColors){RColorBrewer::brewer.pal(nColors,"Dark2")}){
  
  
  #force grouping by TSid
  plot.df <- dplyr::group_by(plot.df,paleoData_TSid)
  
  #create the color function
  #start with some error checking...
  if(is.character(color.ramp)){#then use that for the ramp
    #color.fun <- function(nColors,color.ramp){rep(grDevices::rgb(maxColorValue = 255,t(grDevices::col2rgb(color.ramp))),nColors)}
    color.fun <- function(nColors,color.ramp){rep(color.ramp,nColors)}
  }else{
    color.fun <- function(nColors,color.ramp){grDevices::colorRampPalette(color.ramp(nColors))(nColors)}
  }
  
  
  #check the plot.df for required variables
  reqVar <- c("paleoData_values","paleoData_TSid","paleoData_units","paleoData_variableName","dataSetName", "archiveType",time.var)
  
  for(r in 1:length(reqVar)){
    if(!any(reqVar[r] == names(plot.df))){
      stop(paste(reqVar[r],"must be in plot.df"))
    }
  }
  
  #check to see if time.var is an age ensemble
  
  if(NCOL(plot.df[[time.var]]) > 1){
    stop(glue::glue("It looks like your time.var ({time.var}) has more than 1 column ({NCOL(plot.df[[time.var]])}). plotTimeseriesStack() cannot yet handle age ensembles."))
  }
  
  
  plot.df <- plot.df %>%
    dplyr::mutate(scaled = scale(paleoData_values)*scale.factor) %>%
    dplyr::filter(is.finite(scaled))
  
  if(!is.na(invert.var)){# then make some negative
    iv <- plot.df[[invert.var]]
    if(is.character(iv)){
      ivn <- matrix(1,nrow(plot.df))
      ivn[grepl("neg",iv,ignore.case = TRUE)] <- -1
      ivn[grepl("-1",iv,ignore.case = TRUE)] <- -1
      iv <- ivn
    }else if(is.numeric(iv)){
      ivn <- matrix(1,nrow(plot.df))
      ivn[iv<0] <- -1
      iv <- ivn
    }
    if(all(iv == 1 | iv == -1)){
      plot.df$scaled <- plot.df$scaled*iv# flip as needed
      plot.df$iv <- iv
    }else{
      stop("inverting the variables based on invert.var failed. Check help for details.")
    }
  }else{#then all positive
    plot.df$iv <- 1
  }
  
  #arrange the data.frame by TSid factors
  plot.df$paleoData_TSid <- factor(plot.df$paleoData_TSid,levels = unique(plot.df$paleoData_TSid))
  
  
  #copy the color variable into plot.df
  plot.df$cv = plot.df[[color.var]]
  
  plot.df$cv <- factor(plot.df$cv,levels = unique(plot.df$cv))
  
  axisStats <- plot.df %>%
    dplyr::summarize(variableName = unique(paleoData_variableName),
                     units = unique(paleoData_units),
                     dataSetName = unique(dataSetName),
                     archiveType = unique(archiveType), 
                     invert = mean(iv),
                     mean = mean(paleoData_values,na.rm = T),
                     sdhigh = sd(paleoData_values,na.rm = T)/scale.factor*scale.height+mean(paleoData_values,na.rm = T),
                     sdlow = -sd(paleoData_values,na.rm = T)/scale.factor*scale.height+mean(paleoData_values,na.rm = T),
                     color.var = unique(cv)) %>%
    dplyr::mutate(axisLabel = paste0(variableName," (",units,")")) %>%
    dplyr::mutate(axisMin = ifelse(invert == 1,as.character(signif(sdlow,3)),as.character(signif(sdhigh,3))))  %>%
    dplyr::mutate(axisMax = ifelse(invert == 1,as.character(signif(sdhigh,3)),as.character(signif(sdlow,3))))
  
  
  colOrder <- match(unique(plot.df$paleoData_TSid),axisStats$paleoData_TSid)
  
  axisStats <- axisStats[colOrder,]
  
  nlines <- length(unique(plot.df$paleoData_TSid))
  
  if(getRversion() >= 4){
    nColors <- min(length(axisStats$color.var),nlines)
  }else{
    nColors <- min(length(levels(axisStats$color.var)),nlines)
  }
  colVec <- color.fun(nColors,color.ramp)
  axisStats$colors <- colVec[match(axisStats$color.var,levels(axisStats$color.var))]
  
  spag <- ggplot(plot.df, aes(height = scaled, y = paleoData_TSid,color = cv, fill = cv)) +
    geom_ridgeline(aes_string(x = time.var),min_height = -Inf,alpha = fill.alpha,size = line.size)+
    scale_color_manual(name = color.var,values = colVec)+
    scale_fill_manual(name = color.var,values = colVec)+
    theme_ridges(grid = TRUE)+
    theme_bw()
  
  
  ylow <- seq_len(nlines)-scale.height
  yhigh <-  seq_len(nlines)+scale.height
  
  
  my.x.range <- getPlotRanges(spag)$x.lims
  
  xpos <- rep(my.x.range,times = ceiling(nlines/2))[seq_len(nlines)]
  
  #guess position for label
  xrtick <- my.x.range+c(-1 ,1)*abs(diff(my.x.range))*lab.buff*.25
  xposTick <- rep(xrtick,times = ceiling(nlines/2))[seq_len(nlines)]
  
  xrtickLabel <- my.x.range+c(-1 ,1)*abs(diff(my.x.range))*lab.buff
  xposTickLabel <- rep(xrtickLabel,times = ceiling(nlines/2))[seq_len(nlines)]
  
  xrlab <- my.x.range+c(-1 ,1)*abs(diff(my.x.range))*lab.buff*lab.space
  xposLab <- rep(xrlab,times = ceiling(nlines/2))[seq_len(nlines)]
  if(time.var == "year"){
    xlabName <- paste0("Year (",plot.df$yearUnits[1],")")
  }else if(time.var == "age"){
    xlabName <- paste0("Age (",plot.df$ageUnits[1],")")
  }else if(time.var == "depth"){
    xlabName <- paste0("Depth (",plot.df$depth.units[1],")")
  }else{
    xlabName <- "Unknown"
  }
  
  spag <- spag+annotate(geom = "segment", color = axisStats$colors , x = xpos, xend = xpos, y = ylow, yend  = yhigh)+
    annotate(geom = "segment", color = axisStats$colors , x = xpos, xend = xposTick, y = ylow, yend  = ylow)+
    annotate(geom = "segment", color = axisStats$colors , x = xpos, xend = xposTick, y = yhigh, yend  = yhigh)+
    annotate(geom = "text", color = axisStats$colors , x = xposTickLabel, y = ylow, label = axisStats$axisMin,size = lab.size)+
    annotate(geom = "text", color = axisStats$colors , x = xposTickLabel, y = yhigh, label = axisStats$axisMax,size = lab.size)+
    annotate(geom = "text", color = axisStats$colors , x = xposLab, y = seq_len(nlines), label = axisStats$axisLabel,size = lab.size,angle = 90)+
    scale_y_discrete(name = NULL,labels = axisStats$dataSetName, expand = c(0.02,-0.75))+
    scale_x_continuous(name = xlabName, expand = c(0.02,0))
  
  if(length(unique(axisStats$colors))==1){
    spag = spag+theme(legend.position = "none")
  }
  
  return(spag)
}