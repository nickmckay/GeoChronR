# Analysis plots: correlation, regression, histogram, and PCA results.


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
  cor.df <- corout$cor.df[!is.na(corout$cor.df$r), ]
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
    
    h <- h+geom_histogram(aes(x=plotR,y=after_stat(count),fill = factor(fdrSigPlot)), position = 'stack', color = "white", binwidth = bw) +
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
    
    h <- h+geom_histogram(aes(x=plotR,y=after_stat(count),fill = factor(issig)), position = 'stack', color = "white", binwidth = bw) +
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
    h = h + geom_vline(data = cor.stats, aes(xintercept = r), color="red", linewidth = 1,
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
  mm <- tidyr::pivot_longer(df,cols = -x, values_to = "value",names_to = "variable")
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
    geom_histogram(data=plotData,aes(x=r,y=after_stat(density)),color="white",bins=bins,fill=fill,alpha=alp)+
    geoChronRPlotTheme()+
    ylab("Probability density")
  if(!all(is.na(quantiles))){
    #make labels better
    lineTypes <- rep(2,times = length(quantiles))
    lineTypes[quantiles==.5] <- 1
    labelPositionY <- getPlotRanges(histPlot)$y.lims[2] * label.vert.position
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
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = "black",linewidth = .05,linetype = 2))+
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
