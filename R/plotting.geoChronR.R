#' @export
#' @family plot
#' @title Define a plot theme for GeoChronR
#' @description Use this to define a theme across geoChronR
geoChronRPlotTheme = ggplot2::theme_bw

#' @export
#' @title Plot ensemble spectra output
#' @description Plot the output of powerSpectrumEns() as a ribbon plot of distributions, plus confidence levels
#' @family plot
#' @family spectra
#' @param spec.ens Output from powerSpectrumEns()
#' @return ggplot object of spectrum plot
plotSpectraEns = function (spec.ens){
  specPlot = plotTimeseriesEnsRibbons(spec.ens$freqs,spec.ens$power)
  specPlot = plotTimeseriesEnsRibbons(spec.ens$freqs,spec.ens$powerSyn,add.to.plot = specPlot,probs = c(.9,.95),colorHigh = "red",alp = .5)
  #{to do} label significant peaks
  
  specPlot = specPlot +xlab("Frequency (1/yr)") +ylab("Power") +scale_x_log10() +scale_y_log10()
  return(specPlot)
}

#' @export
#' @title Find quantiles across an ensemble
#' @family gridding
#' @description Determine quantiles across ensembles of x and/or y, as a function of x, using interpolation
#' @param x n by m matrix where n is the number of observations and m is >= 1
#' @param y n by j matrix where n is the number of observations and j is >= 1 
#' @param nbins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param probs quantiles to calculate
#' @param nens number of ensemble members to derive quantiles for
#' @return list of quantiles and x.bin
#' @author Nick McKay
#' @examples
#' 
#' 
quantile2d = function(x,y,nbins=500,x.bin = NA,probs = c(0.025,0.25,0.5,0.75, 0.975),nens = max(c(ncol(x),ncol(y)))){
  #interpolates then finds
  if(nrow(x)!=nrow(y)){
    stop("x and y must have the same number of rows")
  }
  
  
  #interpolate option...
  sx = sort(c(x))
  if(all(is.na(x.bin))){
    x.bin <- approx(1:length(sx),sx,seq(1,length(sx),length.out = nbins))$y #adjust it along y
  }
  y.int = matrix(NA,ncol = nens,nrow= length(x.bin))
  
  for(int in 1:nens){
    y.int[,int] = approx(x = x[,sample.int(ncol(x),size = 1)] , y = y[,sample.int(ncol(y),size = 1)],xout = x.bin )$y
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
#' @description Calculate the density of samples along a 2-dimensional grid
#' @param x n by m matrix where n is the number of observations and m is >= 1
#' @param y n by j matrix where n is the number of observations and j is >= 1 
#' @param nbins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param y.bin vector of bin edges over which to bin.
#' @param filterFrac Used to beef up sampling for poorly sampled intervals. Interpolates intervals with less than filterFrac coverage.
#' @param interpolate use interpolation? T/F
#' @return A list with a matrix of density, x.bin and y.bin
#' 
bin2d = function(x,y,nbins=100,x.bin=NA,y.bin=NA,filterFrac = NA,interpolate = TRUE){
  if(nrow(x)!=nrow(y)){
    stop("x and y must have the same number of rows")
  }
  
  if(interpolate){
    #interpolate option...
    sx = sort(c(x))
    x.bin <- approx(1:length(sx),sx,seq(1,length(sx),length.out = nbins))$y #adjust it along y
    
    nens = max(c(ncol(x),ncol(y)))
    y.int = matrix(NA,ncol = nens,nrow= length(x.bin))
    for(int in 1:nens){
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
      #x.bin <- seq((min(df[,1],na.rm=TRUE)-range.x/2), (max(df[,1],na.rm=TRUE)+range.x/2), length=nbins)
      x.bin <- unique(approx(1:length(sort(df$x)),sort(df$x),seq(1,length(sort(df$x)),length.out = nbins))$y) #adjust it along y
      #x.bin  = unique(qbins(df$x,nbins))
      #x.bin = unique(quantile(unique(df$x),probs = seq(0,1,length.out = nbins)))
    }
  }
  if(all(is.na(y.bin))){
    if(ncol(y)==1){
      y.bin = sort(unique(y))
    }else{
      #range.y=abs(diff(range(df[,2],na.rm=TRUE)))
      #y.bin <- seq((min(df[,2],na.rm=TRUE)-range.y/2), (max(df[,2],na.rm=TRUE)+range.y/2), length=nbins)
      y.bin <- unique(approx(1:length(sort(df$y)),sort(df$y),seq(1,length(sort(df$y)),length.out = nbins))$y) #adjust it along y
      #y.bin  = unique(qbins(df$y,nbins))
      #y.bin  = unique(quantile(unique(df$y),probs = seq(0,1,length.out = nbins)))
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
  if(!is.na(filterFrac)){
    sumX = apply(freq2D,MARGIN = 1,FUN = sum)
    sumY =  apply(freq2D,MARGIN = 2,FUN = sum)
    freq2D = freq2D[sumX > (length(x.bin)*filterFrac) ,sumY > (length(y.bin)*filterFrac)]
    y.bin = y.bin[sumY > (length(y.bin)*filterFrac)]
    x.bin = x.bin[sumX > (length(x.bin)*filterFrac)]
    
  }
  
  
  density = (freq2D/sum(freq2D))
  
  out = list("density" = density,"x.bin"= x.bin,"y.bin"=y.bin)
  
  return(out)
}

#' @export
#' @family gridding
#' @title Two dimensional kernel density estimation
#' @description Use a kernel density estimator to model the density of samples along a 2-dimensional grid
#' @param x n by m matrix where n is the number of observations and m is >= 1
#' @param y n by j matrix where n is the number of observations and j is >= 1 
#' @param nbins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param y.bin vector of bin edges over which to bin.
#' @return A list with a matrix of density, x.bin and y.bin
kde_2d = function(x,y,nbins=100,x.bin=NA,y.bin=NA){
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
  kde = MASS::kde2d(df$x,df$y,h=1,  n =nbins)
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
#' @import gridExtra
#' @param L A LiPD Object
#' @return A gridArrange of ggplot grobs
#' @examples 
#' myPlot = summaryPlot(L)
#' 
plotSummary = function(L){
  #is this a LiPD file?
  if(is.list(L)){
    if(is.null(L$dataSetName)){
      stop("This is either not a single LiPD object, or it has no dataSetName. plotSummary requires a single LiPD object as input")
    }
  }else{
    stop("plotSummary requires a single LiPD object as input")
  }
  
  map = mapLipd(L,extend.range = 5)
  
  #plot paleoData
  
  print("What should we plot on the X-axis?")
  print("We'll look for age or year...")
  age=selectData(L,varName = "age",always.choose = FALSE,altNames="year")
  
  print("What should we plot on the Y-axis?")
  variable=selectData(L)
  
  paleoPlot = plotLine(age,variable)
  paleoPlot = paleoPlot + labs(title = paste("PaleoData:",variable$variableName))
  
  #do chron.
  if(!is.null(L$chronData[[1]]$measurementTable)){
    #looking for age model data.
    
    print("looking for ages...")
    age2=selectData(L,varName = "age14C",always.choose = FALSE,altNames="year",where = "chronData")
    
    #     print("looking for age uncertainty")
    #     ageUnc = selectData(L,varName = "age14CUncertainty",always.choose = FALSE,altNames=c("uncertainty","error","age"),where = "chronData")
    
    print("looking for depths....")
    depth=selectData(L,varName = "depth",always.choose = FALSE,where = "chronData")
    c.df = data.frame(x=age2$values,y=depth$values)
    chronPlot = plotLine(age2,depth)
    chronPlot = chronPlot+
      geom_point(data=c.df,aes(x=x,y=y),colour="black",size=7)+
      scale_y_reverse()+
      scale_x_reverse()+
      labs(title = paste("ChronData: chronology"))
    
  }else{
    chronPlot = grobTree(rectGrob(gp = gpar(fill = 1,alpha=.1)),textGrob("No chronData"))
  }
  
  lay = rbind(c(1,1,2,2),
              c(3,3,2,2),
              c(3,3,4,4),
              c(3,3,4,4))
  
  
  dataSetText = paste(L$dataSetName,"\n","Archive Type: ",L$archiveType,"\n","Authors: ",L$pub[[1]]$author)
  summaryText = grobTree(rectGrob(gp = gpar(fill = 1,alpha=.1)), textGrob(dataSetText) )
  
  summary = grid.arrange(grobs = list(summaryText,paleoPlot,map,chronPlot),layout_matrix=lay)    
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
#' @examples 
plotLine = function(X,Y,color="black",alp = 1, add.to.plot=ggplot()){
  
  #X and Y and are LiPD variable list, including values, units, names, etc...
  df = data.frame(x = X$values, y = Y$values)
  plot = add.to.plot+ geom_line(data=df,aes(x=x,y=y),colour =color, alpha = alp)+
    ylab(axisLabel(Y))+
    xlab(axisLabel(X))+
    geoChronRPlotTheme()
  
  if(tolower(X$variableName)=="age"){
    plot = plot+scale_x_reverse()
  }
  return(plot)
}


#' @export
#' @family plot
#' @author Nick McKay
#' @title Plot an ensemble timeseries as a set of lines
#' @description Plot an ensemble timeseries as a set of lines. Useful for displaying a handful of ensemble members to characterize individual paths. 
#' @import ggplot2
#' @param X A LiPD variable list to plot, including values, units, names, and more
#' @param Y A LiPD variable list to plot, including values, units, names, and more
#' @param color Line color (following ggplot rules)
#' @param maxPlotN Whats the maximum number of lines to plot?
#' @param alp Line transparency
#' @param add.to.plot A ggplot object to add these lines to. Default is ggplot() . 
#' @return A ggplot object
#' @examples 
plotTimeseriesEnsLines = function(add.to.plot=ggplot(),X,Y,alp=.2,color = "blue",maxPlotN=100){
  #check to see if time and values are "column lists"
  
  oX = X
  oY = Y
  if(is.list(X)){X=as.data.frame(X$values)}
  if(is.list(Y)){Y=as.data.frame(Y$values)}
  
  
  
  X=as.matrix(X)
  Y=as.matrix(Y)
  
  if(nrow(X)!=nrow(Y)){
    stop("X and Y must have the same number of observations")
  }
  
  np = min(maxPlotN,ncol(X)*ncol(Y))
  #sample randomly what to plot
  pX = sample.int(ncol(X),size = np,replace = TRUE)
  pY = sample.int(ncol(Y),size = np,replace = TRUE)
  
  #create data frame of uncertain X, Y data
  Xplot = c(rbind(X[,pX],matrix(NA,ncol=np)))
  Yplot = c(rbind(Y[,pY],matrix(NA,ncol=np)))
  dfXY = data.frame("x"=Xplot,"y"=Yplot)
  
  
  linePlot = add.to.plot+
    geom_path(data=dfXY,aes(x=x,y=y),colour = color,alpha=alp)+
    geoChronRPlotTheme()
  
  #add labels
  linePlot = linePlot+xlab(axisLabel(oX))+ylab(axisLabel(oY))
  
  #reverse the xaxis if the units are BP
  if(any(grepl(pattern = "BP",x = axisLabel(oX))) | (grepl(pattern = "ka",x = axisLabel(oX))) | (grepl(pattern = "B2k",x = axisLabel(oX))) | (grepl(pattern = "kyr",x = axisLabel(oX)))){
    linePlot = linePlot + scale_x_reverse(axisLabel(oX))
  }
  
  
  
  return(linePlot)
  
}
#' @export
#' @family plot
#' @author Nick McKay
#' @title Plot an ensemble timeseries as ribbons of probabilities
#' @description Plot an ensemble timeseries as a set of bands of probability. Useful for displaying the full range of probability across ensemble members.
#' @import ggplot2
#' @param X A LiPD variable list to plot, including values, units, names, and more
#' @param Y A LiPD variable list to plot, including values, units, names, and more
#' @param probs a vector of probabilities to plot as ribbons. It will create bands as ribbons of quantiles moving inward. If there's an odd number, it plots the middle quantile as a line. 
#' @param colorLow Band color of the outer most band.
#' @param colorHigh Band color of the inner most band.
#' @param lineColor Line color (following ggplot rules)
#' @param lineWidth Width of the line
#' @param nbins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param y.bin vector of bin edges over which to bin.
#' @param alp Line transparency
#' @param add.to.plot A ggplot object to add this plot to. Default is ggplot() . 
#' @param export.quantiles If TRUE, teturn the plotted quantiles rather than the plot
#' @return A ggplot object OR list of plotted quantiles, depending on export.quantiles
#' @examples 
plotTimeseriesEnsRibbons = function(add.to.plot=ggplot(),X,Y,alp=1,probs=c(0.025,.25,.5,.75,.975),x.bin=NA,y.bin=NA,nbins=200,colorLow="white",colorHigh="grey70",lineColor="Black",lineWidth=1,export.quantiles = FALSE){
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
    bandPlot=add.to.plot+geom_line(data=df,aes(x=X,y=Y),colour=lineColor)+geoChronRPlotTheme()
    
  }else{
    
    if(nrow(X)!=nrow(Y)){
      stop("X and Y must have the same number of observations")
    }
    
    ###DEPRECATED - old method.
    # binned = bin2d(X,Y,x.bin=x.bin,y.bin = y.bin,nbins=nbins)
    # binned = kde_2d(X,Y,x.bin=x.bin,y.bin = y.bin,nbins=nbins)
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
    
    probMatList = quantile2d(X,Y,nbins = nbins,x.bin = x.bin,probs = probs)
    if(export.quantiles){
      return(probMatList)
    }
    
    probMat = probMatList$quants
    
    probMat=as.data.frame(probMat)
    lineLabels=as.character(probs)
    
    #make labels better
    goodName= c("-2σ","-1σ","Median","1σ","2σ")
    realProb= c(pnorm(-2:2))
    for(i in 1:length(lineLabels)){
      p=which(abs(as.numeric(lineLabels[i])-realProb)<.001)
      if(length(p)==1){
        lineLabels[i]=goodName[p]
      }
    }
    names(probMat) = lineLabels
    
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
    
    #deal with colors
    fillCol=colorRampPalette(c(colorLow,colorHigh))( ncol(bandMat)/2+1 )[-1]
    lineColor="black"
    
    
    for(b in 1:(ncol(bandMat)/2)){
      if(b==1){
        bandPlot = add.to.plot+geoChronRPlotTheme()
      }
      bands=data.frame(x=probMatList$x.bin,ymin = bandMat[,b],ymax = bandMat[,ncol(bandMat)-b+1])
      bandPlot = bandPlot+
        geom_ribbon(data=bands,aes(x=x,ymin=ymin,ymax=ymax),fill=fillCol[b],alpha=alp)
    }
    
    if(!all(is.na(center))){
      bandPlot = bandPlot+
        geom_line(data=center,aes(x=x,y=y),colour=lineColor,size=lineWidth)
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
#' @param alp Line transparency
#' @param maxPlotN Whats the maximum number of lines to plot?
#' @param add.to.plot A ggplot object to add this plot to. Default is ggplot() . 
#' @return A ggplot object
#' @examples 
plotScatterEns = function(X,Y,alp=.2,maxPlotN=1000,add.to.plot = ggplot()){
  X=as.matrix(X)
  Y=as.matrix(Y)
  
  if(nrow(X)!=nrow(Y)){
    stop("X and Y must have the same number of observations")
  }
  
  np = min(maxPlotN,ncol(X)*ncol(Y))
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
#' @param mb.df A data.frame of slopes (column 1) and intercepts (column 2)
#' @param alp Line transparency
#' @param pXY index of which observations to use
#' @param xrange range of x values (min and max)
#' @param add.to.plot A ggplot object to add these lines to. Default is ggplot() . 
#' @return A ggplot object
#' @examples 
plotTrendLinesEns = function(mb.df,xrange,pXY=1:nrow(mb.df) ,alp=.2 ,color = "red",add.to.plot=ggplot()){
  xvec = c(xrange,NA)
  yall = c()
  xall = c()
  df = data.frame(m=mb.df[pXY,1],b=mb.df[pXY,2])
  for(p in 1:length(pXY)){
    yvec = c(df$m[p]*xrange + df$b[p],NA)
    yall = c(yall,yvec)
    xall = c(xall,xvec)
  }
  dfi = data.frame(x=xall,y=yall)
  
  trendlines = add.to.plot+
    geom_path(data=dfi,aes(x=x,y=y),colour = color,alpha=alp)+
    geoChronRPlotTheme()+
    xlim(xrange)
  
  
  return(trendlines)
}

#' @export
#' @family plot
#' @author Julien Emile-Geay
#' @author Nick McKay
#' @title Plot the results of an ensemble correlation
#' @description Plots the output of an ensemble correlation analysis.
#' @import ggplot2
#' @param cor.df A data.frame correlation r and p-values. Output from corEns()
#' @param corStats A data.frame of correlation quantiles. Output from corEns()
#' @param bins Number of bins in the histogram
#' @param lineLabels Labels for the quantiles lines
#' @param add.to.plot A ggplot object to add these lines to. Default is ggplot()
#' @param legendPosition Where to put the map legend?
#' @param significanceOption Choose how handle significance. Options are:
#'  \itemize{
#'  \item "autocor" (default) for serial-autocorrelation corrected p-values
#'  \item "raw" for uncorrected p-values
#'  \item "FDR" for autocorrelation and False-discovery-rate corrected p-values
#'  }
#' @return A ggplot object
#' @examples 
plotCorEns = function(corEns,bins=40,lineLabels = rownames(corStats),add.to.plot=ggplot(),legendPosition = c(0.2, 0.8),significanceOption = "autocor"){
  
  #pull data frames out of the list
  cor.df <- corEns$cor.df
  corStats <- corEns$corStats
  
  
  # evaluate preliminary quantities
  rng <- range(cor.df$r)
  bw = (rng[2]-rng[1])/bins
  
  if(significanceOption == "raw"){
    issig <- cor.df$pRaw<0.05
  }else if(grepl("raw",significanceOption,ignore.case = T)){
    issig <- cor.df$sig_fdr
  }else{#serial autocorrelation
    issig <- cor.df$pSerial<0.05
  }
  
  sig_frac <- sum(issig/dim(cor.df)[1]*100)
  
  sig_lbl = paste0("Fraction significant: ", signif(sig_frac,3), "%")
  # Now the plotting begins
  lbf = c("p ≥ 0.05","p < 0.05")
  
  #artificially introduce at least 1 sig/nonsig for plotting
  if(sum(issig) == 0){
    issig[which(abs(cor.df$pSerial)==max(abs(cor.df$pSerial)))[1]] <- TRUE
  }
  if(sum(issig) == dim(cor.df)[1]){
    issig[which(abs(cor.df$pSerial)==min(abs(cor.df$pSerial)))[1]] <- FALSE
  }
  
  
  h = ggplot() + ggtitle("Correlation Distribution") + # initialize plot
    geom_histogram(data=cor.df,aes(x=r,y=..count..,fill = factor(issig)), position = 'stack', colour = "white", binwidth = bw) +
    scale_fill_manual(values=alpha(c("grey50","Chartreuse4"),c(0.8,0.6)), labels=lbf, guide = guide_legend(title = NULL))
  
  
  #determine initial
  if(compareVersion(as.character(packageVersion("ggplot2")),"2") >=0){ #deal with ggplot versions
    ylims = ggplot_build(h)$layout$panel_scales$y[[1]]$get_limits()
    xlims = ggplot_build(h)$layout$panel_scales$x[[1]]$get_limits()
  }else{
    ylims = ggplot_build(h)$panel$ranges[[1]]$y.range # get y range
    xlims = ggplot_build(h)$panel$ranges[[1]]$x.range # get x range
  }
  
  #how many lines?
  lineType= rep("dashed",times = nrow(corStats))
  lineType[corStats$percentiles==.5]="solid"
  lineType[corStats$percentiles==.975 | corStats$percentiles==.025]="dotted"
  
  
  
  # add vertical lines at the quantiles specified in corStats. 
  h = h + geom_vline(data = corStats, aes(xintercept = values), color="red", size = 1,
                     linetype=lineType, show.legend = FALSE) +
    ylim(c(ylims[1],ylims[2]*1.1)) # expand vertical range
  ymax = max(ylims)
  # annotate quantile lines. geom_label is too inflexible (no angles) so use geom_text()
  h = h + geom_text(data = corStats, mapping = aes(x=values, y=.90*ymax, label=lineLabels), color="red", size=3, angle=45, vjust=+2.0, hjust=0)+
    annotate("text",x = 0.7*xlims[2],y=0.4*ylims[2], label = sig_lbl,color="Chartreuse4")+geoChronRPlotTheme() # add fraction of significant correlations
  #customize legend
  h = h + theme(legend.position = legendPosition,
                legend.title = element_text(size=10, face="bold"),
                legend.text = element_text(size=8),
                legend.key = element_rect(fill = "transparent",
                                          colour = "transparent"),
                legend.background = element_rect(fill=alpha('white', 0.3)))
  
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
#' @examples 
plotPvalsEnsFdr = function(cor.df,alpha = 0.05){
  m=dim(cor.df)[1] # number of hypotheses being tested
  rk = seq(m)
  fdr_thresh = rk/m*alpha
  lvl_thresh = rep(alpha,m)
  pvals = sort(cor.df$pRaw)
  pvalsA = sort(cor.df$pSerial)
  # Implement this strategy: https://stackoverflow.com/questions/38962700/ggplot-legend-order-mismatch
  df <- data.frame(pvals, pvalsA, FDR = fdr_thresh, level = lvl_thresh, x = rk)
  mm <- reshape2::melt(df,id.var="x")
  lbl <- c("p-value, IID","p-value, Serial","FDR", bquote(alpha==.(alpha)))
  pvalPlot <- ggplot(data = mm, aes(x,value,colour=variable,linetype=variable)) + geom_line()
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
                                                          colour = "transparent"),
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
#' @param ensData A data.frame of values to plot as a histogram
#' @param probs quantiles to calculate and plot
#' @param bins Number of bins in the histogram
#' @param lineLabels Labels for the quantiles lines
#' @param add.to.plot A ggplot object to add these lines to. Default is ggplot()
#' @param fill fill color of the histogram, following ggplot rules
#' @return A ggplot object
#' @examples 
plotHistEns = function(ensData,quantiles=c(.025, .25, .5, .75, .975),bins=50,lineLabels = rownames(ensStats),add.to.plot=ggplot(),alp=1,fill="grey50"){
  #plots a histogram of ensemble distribution values, with horizontal bars marking the distributions
  plotData = data.frame("r"=c(ensData))
  
  
  
  histPlot = add.to.plot+
    geom_histogram(data=plotData,aes(x=r,y=..density..),colour="white",bins=bins,fill=fill,alpha=alp)+
    geoChronRPlotTheme()+
    ylab("Probability density")
  if(!all(is.na(quantiles))){
    #make labels better
    
    quants = quantile(ensData,quantiles)
    quantdf = data.frame(ll = names(quants),quants = quants)
    histPlot = histPlot + geom_vline(data=quantdf,aes(xintercept = quants),colour="red") +
      geom_label(data = quantdf, aes(x = quants, y=0,label=ll))+
      xlab(axisLabel(ensData))
    
  }
  return(histPlot)
}


#' @export
#' @title get a ggplot legend object
#' @family plot
#' @family pca
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
#' @family plot
#' @family pca
#' @author Nick McKay
#' @title Map ensemble pca loadings and plot PC timeseries
#' @description Map ensemble pca loadings and plot PC timeseries
#' @import ggplot2
#' @import ggmap
#' @import gridExtra
#' @import mapproj
#' @param ens.PC.out results of pcaEns()
#' @param TS Timeseries object \url{http://nickmckay.github.io/LiPD-utilities/r/index.html#what-is-a-time-series} used in the pcaEns() analysis
#' @param map.type "google" or "line"
#' @param f zoom buffer for plotting
#' @param which.PCs vector of PCs to plot. Choose two. c(1,2) is default.
#' @param color color scale option. See assignColors()
#' @param dotsize How big are the dots on the map
#' @param restrict.map.range TRUE or FALSE. Trim the size of the map to the points, for "line" map type
#' @param shape.by.archive TRUE or FALSE. Use archiveType to assign shapes.
#' @param projection Map project. All options on: ?mapproject
#' @param lineLabels Labels for the quantiles lines
#' @param boundcirc For polar projects, draw a boundary circle? TRUE or FALSE
#' @param probs quantiles to calculate and plot in the PC timeseries
#' @param which.leg which map legend to include in the summary plot?
#' @param legendPosition Where to put the map legend?
#' @return A gridExtra ggplot object
#' @examples 
plotPcaEns = function(ens.PC.out,TS,map.type="line",which.PCs=c(1,2),f=.2,color="temp",dotsize=5,restrict.map.range=TRUE,shape.by.archive=TRUE,projection="mollweide",boundcirc=TRUE,probs=c(.025, .25, .5, .75, .975),which.leg = 1,legendPosition = c(0.5,0.5)){
  #get data out of the TS
  lat = sapply(TS,"[[","geo_latitude")
  lon = sapply(TS,"[[","geo_longitude")
  archive = sapply(TS,"[[","archiveType")
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
  archiveShapes=c(21,22,24,23)
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
  medianPCs = apply(ens.PC.out$PCs,MARGIN = c(1,2),median,na.rm=TRUE)
  loadingSDs = apply(ens.PC.out$loadings,MARGIN = c(1,2),sd,na.rm=TRUE)
  medianLoadings = apply(ens.PC.out$loadings,MARGIN = c(1,2),median,na.rm=TRUE)
  
  #get a base map
  map = baseMap(lon,lat,map.type = map.type,f=f,projection = projection,restrict.map.range = restrict.map.range)
  
 
  
  for (i in 1:length(which.PCs)){
    #figure out dotsize
    sdRange = range(loadingSDs[,which.PCs[i]])
    medianRange = abs(diff(range(medianLoadings[,which.PCs[i]])))
    sdPct = 2*loadingSDs[,which.PCs[i]]/medianRange
    sdDots = sdPct
    
    
    #make a data.frame to plot
    dd=data.frame(lon=lon,lat=lat,medLoad=medianLoadings[,which.PCs[i]],sdDots=sdDots,shape=factor(arch.shape))
    #sort by dot size
    # print(order(sdDots))
    dd = dd[order(sdDots),]
    row.names(dd)=1:nrow(dd)
    
    
    
    #infer colors
    scaleColors = assignColors(color)
    
    
    
    
    maplist[[i]] = map +  geom_point(aes(x=lon,y=lat,fill=medLoad,size=sdDots,shape = shape), data=dd)+theme(legend.box = "horizontal",legend.position=legendPosition)
    
    
    testMap <- map +  geom_point(aes(x=lon,y=lat,fill=medLoad,size=sdDots,shape = shape), data=dd)+
      theme(legend.box = "horizontal",legend.position=legendPosition) + 
      scale_shape_manual(name = "Archive Type",values = archiveShapes) +
      scale_size(name = "Loading uncertainty",range = c(dotsize,1)) +
      scale_fill_gradient2(name="Loadings",low=scaleColors[1],high=scaleColors[2],guide="colourbar")
    
    
    leglist[[i]] <- getLegend(testMap)
    
    
    
    #if(i>1 & !repeatMapLegend){#Don't repeat all the legend components
    if(TRUE){
      maplist[[i]] <- maplist[[i]] +
        scale_shape_manual(values = archiveShapes,guide="none") +
        scale_size(name = "Loading uncertainty",range = c(dotsize,1),guide="none") +
        scale_fill_gradient2(name="Loadings",low=scaleColors[1],high=scaleColors[2],guide="none")
    }else{#plot the map legends
      maplist[[i]] <- maplist[[i]] +
        scale_shape_manual(name = "Archive Type",values = archiveShapes) +
        scale_size(name = "Loading uncertainty",range = c(dotsize,1)) +
        scale_fill_gradient2(name="Loadings",low=scaleColors[1],high=scaleColors[2],guide="colourbar")
    }
    
    
    

    

    plotlist[[i]] = plotTimeseriesEnsRibbons(X=ens.PC.out$age,Y=ens.PC.out$PCs[,which.PCs[i],],x.bin =ens.PC.out$age,nbins = 10000 ,probs = probs) 
    medianVarExp = median(ens.PC.out$variance[which.PCs[i],])
    sdVarExp = sd(ens.PC.out$variance[which.PCs[i],])
    varExpStr  = paste(as.character(signif(medianVarExp*100,2)),"±",as.character(signif(sdVarExp*100,1)))
    
    plotlist[[i]] = plotlist[[i]]+ggtitle(paste("Variance explained =",varExpStr,"%"))
    
    if(grepl(pattern = "AD",ageUnits) | grepl(pattern = "CE",ageUnits) ){
      plotlist[[i]] = plotlist[[i]] + labs(y=paste0("PC",which.PCs[i]),x="Year (AD)")
    }else{
      plotlist[[i]] = plotlist[[i]] +
        scale_x_reverse()+
        labs(y=paste0("PC",which.PCs[i]),x="Age (yr BP)")
    }
  }
    
  #plot sample depth
  
  bddf = data.frame(sampleDepth = ens.PC.out$meanDataDensity*100,age = ens.PC.out$age)
  
  plot_sample.depth = ggplot(data=bddf)+geom_area(aes(x=age,y=sampleDepth),fill="gray20")+
    ylab("Data coverage (%)")+
    geoChronRPlotTheme()
  
  if(grepl(pattern = "AD",ageUnits) | grepl(pattern = "CE",ageUnits) ){
    plot_sample.depth  <- plot_sample.depth  + labs(x="Year (AD)")
  }else{
    plot_sample.depth  <- plot_sample.depth  +
      scale_x_reverse("Age (yr BP)")
  }
  
  
    alllist = append(maplist,plotlist)
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
  #' @param L A LiPD object
  #' @param dist.var Name of the distribution variable, will be plotted along the x-axis. Use coord_flip() after running the function if you want vertical distributions. "age" by default. 
  #' @param y.var Name of the y-axis variable. "depth" by default. 
  #' @param mode chron or paleo 
  #' @param which.data number of the chron or paleo Data object
  #' @param model.num number of the model object
  #' @param color distribution color (following ggplot rules)
  #' @param dist.plot vector of distribution tables to plot
  #' @param distType "violin" (default), "up" for one-sided distributions pointed up, "down" for one-sided distributions pointed down
  #' @param thick thickness of the line around the distribution
  #' @param truncateDist truncate probability density values below this number. NA (default) means no truncation
  #' @param scaleFrac controls the vertical span of the probability distribution. Approximately the vertical fraction of the plot that the distribution will cover. 
  #' @param add.to.plot A ggplot object to add this plot to. Default is ggplot() . 
  #' @return A ggplot object
  #' @examples 
  plotModelDistributions = function(L,dist.var = "age",y.var = "depth",mode = "chron",which.data = 1, model.num = 1, add.to.plot = ggplot(), alp=.5,color = "purple",scaleFrac = 0.02,dist.plot = NA,distType = "violin",thick = 0.1,truncateDist = NA){
    
    
    P = L[[paste0(mode,"Data")]]
    if(is.na(which.data)){
      if(length(P)==1){
        which.data=1
      }else{
        print(names(P))
        which.data=as.integer(readline(prompt = "Which paleoData do you want to put this age ensemble in? Select a number "))
      }
    }
    
    #initialize model number
    MT = P[[which.data]]$model
    if(is.null(MT)){
      stop(paste0("There are no models in ",mode,"Data[[",as.character(which.data),"]]. This makes it difficult to plot distributions from the model"))
    }
    
    if(is.na(model.num)){
      if(length(MT)==1){
        #only one pmt
        which.mt=1
      }else{
        print(paste0(where,"Data[[", as.character(which.paleo), "]] has ", length(MT), " models"))
        which.mt=as.integer(readline(prompt = "Which measurement table do you want to put the ensemble in? Enter an integer "))
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
    if(!is.na(truncateDist)){
      tgood = which(this.dist$probabilityDensity$values > truncateDist)
      this.dist$probabilityDensity$values = this.dist$probabilityDensity$values[tgood]
      this.dist$age$values = this.dist$age$values[tgood]
    }
    
    
    pd = this.dist$probabilityDensity$values/sum(this.dist$probabilityDensity$values,na.rm=T)
    scaler = scaleFrac*abs(diff(plot.range))/max(pd)
    
    
    #loop through individual ages...
    for(y in dist.plot){
      this.dist = dist[[y]]
      if(!is.na(truncateDist)){
        tgood = which(this.dist$probabilityDensity$values > truncateDist)
        this.dist$probabilityDensity$values = this.dist$probabilityDensity$values[tgood]
        this.dist$age$values = this.dist$age$values[tgood]
      }
      pd = this.dist$probabilityDensity$values/sum(this.dist$probabilityDensity$values,na.rm=T) * scaler
      this.df = data.frame(x= this.dist[[dist.var]]$values,ymin = this.dist[[y.var]] - pd,ymax = this.dist[[y.var]] + pd )
      if(distType == "up"){this.df$ymin =  this.dist[[y.var]]}
      if(distType == "down"){this.df$ymax =  this.dist[[y.var]]}
      add.to.plot = add.to.plot + geom_ribbon(data = this.df, aes(x = x,ymin = ymin,ymax = ymax),colour = color,fill = color, alpha = alp,size = thick)
    }
    add.to.plot = add.to.plot + geoChronRPlotTheme()
    return(add.to.plot)
  }
  
  #' @export
  #' @family plot
  #' @family chron
  #' @author Nick McKay
  #' @title Plot chronologies
  #' @description Plot creates an age model plot with all the bells and whistles, including a spread of ensemble members, probability distributions, and a few example ensemble members. 
  #' @import ggplot2
  #' @param L A LiPD object
  #' @param dist.var Name of the distribution variable, will be plotted along the x-axis. Use coord_flip() after running the function if you want vertical distributions. "age" by default. 
  #' @param y.var Name of the y-axis variable. "depth" by default. 
  #' @param probs quantiles to calculate and plot
  #' @param nbins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
  #' @param x.bin vector of bin edges over which to bin.
  #' @param y.bin vector of bin edges over which to bin.
  #' @param bandColorLow Band color of the outer most band.
  #' @param bandColorHigh Band color of the inner most band.
  #' @param bandAlpha Transparency of the band plot
  #' @param lineColor Line color (following ggplot rules)
  #' @param lineWidth Width of the line
  #' @param add.to.plot A ggplot object to add this plot to. Default is ggplot() . 
  #' @param nEnsLines Number of ensemble members to plot
  #' @param ensLineColor color of the ensemble lines
  #' @param ensLineAlp transparency of the lines
  #' @param distColor distribution color (following ggplot rules)
  #' @param distType "violin" (default), "up" for one-sided distributions pointed up, "down" for one-sided distributions pointed down
  #' @param distThick thickness of the line around the distribution
  #' @param truncateDist truncate probability density values below this number. NA (default) means no truncation
  #' @param distScale controls the vertical span of the probability distribution. Approximately the vertical fraction of the plot that the distribution will cover. 
  #' @return A ggplot object
  #' @examples 
  plotChron = function(L,ageVar = "ageEnsemble",depthVar = "depth",chron.number=NA,model.num = NA,probs=c(0.025,.25,.5,.75,.975),x.bin=NA,y.bin=NA,nbins=100,bandColorLow="white",bandColorHigh="grey70",bandAlp=1,lineColor="Black",lineWidth=1,add.to.plot=ggplot2::ggplot(),nEnsLines = 5, ensLineColor = "red",ensLineAlp = 0.7,distAlp = 0.3,distType = "violin",distColor = "purple",distThick = 0.1,distScale = 0.02,truncateDist = NA){
    
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
      stop("No ensemble table found. At this time, plotChron() only works with chronData objects with ensemble tables.")
    }
    
    #get the data from the chron ensemble table
    depth = selectData(L,varName = depthVar,where = "chronData",tableType = "ensemble",model.num = model.num,which.data = chron.number)
    ageEnsemble = selectData(L,varName = ageVar,where = "chronData",tableType = "ensemble",model.num = model.num,which.data = chron.number)
    
    #quick fix to ensemble list bug
    ageEnsemble$values = as.matrix(as.data.frame(ageEnsemble$values))
    
    #Ribbons first
    chronPlot = plotTimeseriesEnsRibbons(X = ageEnsemble,Y = depth,alp = bandAlp,probs = probs,x.bin = x.bin,y.bin = y.bin, nbins = nbins, colorLow = bandColorLow,colorHigh = bandColorHigh,lineColor = lineColor,lineWidth = lineWidth,add.to.plot = add.to.plot)
    
    #distributions second...
    if(is.list(C[[chron.number]]$model[[model.num]]$distributionTable)){#if it exists. Add it.
      chronPlot = plotModelDistributions(L,which.data = chron.number,model.num = model.num,add.to.plot = chronPlot,alp=distAlp,color = distColor,distType = distType,thick = distThick,scaleFrac = distScale,truncateDist = truncateDist)
    }
    
    #A few traces last...
    chronPlot = plotTimeseriesEnsLines(X = ageEnsemble,Y = depth,alp = ensLineAlp,color = ensLineColor,add.to.plot = chronPlot,maxPlotN = nEnsLines)
    
    
    #Tidy up...
    chronPlot = chronPlot + scale_y_reverse(name = axisLabel(ageEnsemble)) + xlab(axisLabel(depth)) + ggtitle(paste0(L$dataSetName))
    
    return(chronPlot)
    
  }
  
  #' @export
  #' @family plot
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
  #' @import gridExtra
  #' @param regEnsList output of regressEns()
  #' @param alp Transparency of the scatter plot.
  #' @param quantiles quantiles to calculate and plot
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
  #' @examples 
  plotRegressEns = function(regEnsList,alp=0.2,quantiles = c(0.025, .5, .975)){
    regPlot = list()
    #scatter plot
    scatterplot = plotScatterEns(regEnsList$binX,regEnsList$binY,alp=alp)
    #add trendlines
    scatterplot = plotTrendLinesEns(mb.df = t(rbind(regEnsList$m,regEnsList$b)),xrange = range(regEnsList$binX,na.rm=TRUE), alp = alp,add.to.plot = scatterplot)
    
    
    scatterplot = scatterplot + xlab(axisLabel(regEnsList$valuesX)) + ylab(axisLabel(regEnsList$valuesY))
    
    #assign scatter plot to out list
    regPlot$scatterplot = scatterplot
    
    
    
    #plot histograms of m and b
    mStats = regEnsList$regStats[,1:2]
    names(mStats)[2]="values"
    regPlot$mHist = plotHistEns(regEnsList$m,quantiles = quantiles)+xlab("Slope")
    bStats = regEnsList$regStats[,c(1,3)]
    names(bStats)[2]="values"
    regPlot$bHist = plotHistEns(regEnsList$b,quantiles = quantiles)+xlab("Intercept")
    
    binY = regEnsList$binY
    binX = regEnsList$binX
    
    binY[is.nan(binY)]=NA
    binX[is.nan(binX)]=NA
    
    #plot timeseries of regression and target over interval
    regPlot$XPlot = plotTimeseriesEnsRibbons(regEnsList$yearX,regEnsList$binX,nbins = length(regEnsList$yearX))+ggtitle("Calibration interval predictor")+xlab(axisLabel(regEnsList$timeX))+ylab(axisLabel(regEnsList$valuesX))
    regPlot$YPlot = plotTimeseriesEnsRibbons(regEnsList$yearX,regEnsList$binY,colorHigh = "red",nbins = length(regEnsList$yearX))+ggtitle("Calibration interval predictand")+xlab(axisLabel(regEnsList$timeY))+ylab(axisLabel(regEnsList$valuesY))
    
    
    
    #and plot reconstructions
    if(!is.list(regEnsList$modeledYear)){
      modYear = list()
      modYear$values = regEnsList$modeledYear
      modYear$units = regEnsList$timeX$units
      modYear$variableName = regEnsList$timeX$variableName
    }else{
      modYear = regEnsList$modeledYear
    }
    regPlot$modeledYPlot = plotTimeseriesEnsRibbons(X = modYear,Y=regEnsList$modeled)+ggtitle("Calibrated record using ensemble regression")
    
    
    
    lay = rbind(c(1,1,3,3,4,4),
                c(2,2,3,3,5,5),
                c(6,6,6,6,6,6),
                c(6,6,6,6,6,6))
    
    
    regPlot$summaryPlot = gridExtra::grid.arrange(grobs = list(regPlot$YPlot,regPlot$XPlot,regPlot$scatterplot,
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
  #' @import ggridges
  #' @import dplyr
  #' @import RColorBrewer
  #' @import grDevices
  #' @import scales
  #' @param plot.df A tidy data.frame, typically the output of tidyTs()
  #' @param timeVar Which variable to put on the x-axis. Must be in plot.df. Typically "year", "age", or "depth"
  #' @param colorVar Which variable to color the timeseries by. The default ("paleoData_TSid") will give each timeseries it's own color. Common other options include "paleoData_variable", "archiveType", or "paleoData_units", but any variable in plot.df should work.
  #' @param fillAlpha Transparency of the shading
  #' @param scaleFactor Controls how much the timeseries should overlap, with larger numbers overlapping more. (default = 1/3)
  #' @param scaleHeight Controls how large the y-axes will be. 1 is equivalent to end-to-end coverage with no space. (default = 0.75)
  #' @param labBuff Fraction of the x axis to space the tick marks away from the axes bars (default = 0.02)
  #' @param labSize Font size for the ylabels
  #' @param labSpace Multiplier on labBuff for the axis label separation from the y-scale
  #' @param colorFun A function that defines what colorscale to use. If you want constant colors, you can just enter a string (e.g., "black"). (default = grDevices::colorRampPalette(RColorBrewer::brewer.pal(nColors,"Dark2")))
  #' @return A ggplot object of the plot 
  plotTimeseriesStack <- function(plot.df,timeVar = "year", colorVar = "paleoData_TSid", fillAlpha = 0.2,scaleFactor = 1/3,scaleHeight = .75, labBuff = 0.02, labSize = 3,  labSpace= 2,colorRamp = function(nColors){RColorBrewer::brewer.pal(nColors,"Dark2")}){
 
    
    #force grouping by TSid
    plot.df <- dplyr::group_by(plot.df,paleoData_TSid)
    
    #create the color function
    #start with some error checking...
    if(is.character(colorRamp)){#then use that for the ramp
      #colorFun <- function(nColors,colorRamp){rep(grDevices::rgb(maxColorValue = 255,t(grDevices::col2rgb(colorRamp))),nColors)}
      colorFun <- function(nColors,colorRamp){rep(colorRamp,nColors)}
    }else{
      colorFun <- function(nColors,colorRamp){grDevices::colorRampPalette(colorRamp(nColors))(nColors)}
    }
    
    
    #check the plot.df for required variables
    reqVar <- c("paleoData_values","paleoData_TSid","paleoData_units","paleoData_variableName","dataSetName", "archiveType",timeVar)
    
    for(r in 1:length(reqVar)){
      if(!any(reqVar[r] == names(plot.df))){
        stop(paste(reqVar[r],"must be in plot.df"))
      }
    }
    plot.df <- plot.df %>%
      dplyr::mutate(scaled = scale(paleoData_values)*scaleFactor) %>%
      dplyr::filter(is.finite(scaled))
    
    #arrange the data.frame by TSid factors
    plot.df$paleoData_TSid <- factor(plot.df$paleoData_TSid,levels = unique(plot.df$paleoData_TSid))
    
    
    #copy the color variable into plot.df
    plot.df$cv = plot.df[[colorVar]]
    
    plot.df$cv <- factor(plot.df$cv,levels = unique(plot.df$cv))
    
    axisStats <- plot.df %>%
      dplyr::summarize(variableName = unique(paleoData_variableName),
                units = unique(paleoData_units),
                dataSetName = unique(dataSetName),
                archiveType = unique(archiveType),
                mean = mean(paleoData_values,na.rm = T),
                sdhigh = sd(paleoData_values,na.rm = T)/scaleFactor*scaleHeight+mean(paleoData_values,na.rm = T),
                sdlow = -sd(paleoData_values,na.rm = T)/scaleFactor*scaleHeight+mean(paleoData_values,na.rm = T),
                colorVar = unique(cv)) %>%
      dplyr::mutate(axisLabel = paste0(variableName," (",units,")")) %>%
      dplyr::mutate(axisMin = as.character(signif(sdlow,3))) %>%
      dplyr::mutate(axisMax = as.character(signif(sdhigh,3)))
    
    colOrder <- match(unique(plot.df$paleoData_TSid),axisStats$paleoData_TSid)
    
    axisStats <- axisStats[colOrder,]
    
    nlines <- length(unique(plot.df$paleoData_TSid))
    
    nColors <- min(length(levels(axisStats$colorVar)),nlines)
    
    colVec <- colorFun(nColors,colorRamp)
    axisStats$colors <- colVec[match(axisStats$colorVar,levels(axisStats$colorVar))]
    
    spag <- ggplot(plot.df, aes(height = scaled, y = paleoData_TSid,color = cv, fill = cv)) +
      geom_ridgeline(aes_string(x = timeVar),min_height = -Inf,alpha = fillAlpha)+
      scale_color_manual(name = colorVar,values = colVec)+
      scale_fill_manual(name = colorVar,values = colVec)+
      theme_ridges(grid = TRUE)+
      theme_bw()
    
    
    ylow <- seq_len(nlines)-scaleHeight
    yhigh <-  seq_len(nlines)+scaleHeight
    
    
    my.xrange <- ggplot_build(spag)$layout$panel_scales$x[[1]]$range$range
    
    xpos <- rep(my.xrange,times = ceiling(nlines/2))[seq_len(nlines)]
    
    #guess position for label
    xrtick <- my.xrange+c(-1 ,1)*abs(diff(my.xrange))*labBuff*.25
    xposTick <- rep(xrtick,times = ceiling(nlines/2))[seq_len(nlines)]
    
    xrtickLabel <- my.xrange+c(-1 ,1)*abs(diff(my.xrange))*labBuff
    xposTickLabel <- rep(xrtickLabel,times = ceiling(nlines/2))[seq_len(nlines)]
    
    xrlab <- my.xrange+c(-1 ,1)*abs(diff(my.xrange))*labBuff*labSpace
    xposLab <- rep(xrlab,times = ceiling(nlines/2))[seq_len(nlines)]
    if(timeVar == "year"){
      xlabName <- paste0("Year (",plot.df$yearUnits[1],")")
    }else if(timeVar == "age"){
      xlabName <- paste0("Age (",plot.df$ageUnits[1],")")
    }else if(timeVar == "depth"){
      xlabName <- paste0("Depth (",plot.df$depthUnits[1],")")
    }else{
      xlabName <- "¯\\_(ツ)_/¯"
    }
    spag <- spag+annotate(geom = "segment", colour = axisStats$colors , x = xpos, xend = xpos, y = ylow, yend  = yhigh)+
      annotate(geom = "segment", colour = axisStats$colors , x = xpos, xend = xposTick, y = ylow, yend  = ylow)+
      annotate(geom = "segment", colour = axisStats$colors , x = xpos, xend = xposTick, y = yhigh, yend  = yhigh)+
      annotate(geom = "text", colour = axisStats$colors , x = xposTickLabel, y = ylow, label = axisStats$axisMin,size = labSize)+
      annotate(geom = "text", colour = axisStats$colors , x = xposTickLabel, y = yhigh, label = axisStats$axisMax,size = labSize)+
      annotate(geom = "text", colour = axisStats$colors , x = xposLab, y = seq_len(nlines), label = axisStats$axisLabel,size = labSize,angle = 90)+
      scale_y_discrete(name = NULL,labels = axisStats$dataSetName, expand = c(0.02,-0.75))+
      scale_x_continuous(name = xlabName, expand = c(0.02,0))
    
    if(length(unique(axisStats$colors))==1){
      spag = spag+theme(legend.position = "none")
    }
    
    return(spag)
  }
  
  
  