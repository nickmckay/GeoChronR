#' @export
plot_spectra.ens = function (spec.ens){
  
  specPlot = plot_timeseries.ribbons(spec.ens$freqs,spec.ens$power)
  specPlot = plot_timeseries.ribbons(spec.ens$freqs,spec.ens$powerSyn,add.to.plot = specPlot,probs = c(.9,.95),colorHigh = "red",alp = .5)
  #to do label significant peaks
  
  #add pvalues?
  
  specPlot = specPlot +xlab("Frequency (1/yr)") +ylab("Power") +scale_x_log10() +scale_y_log10()
  return(specPlot)
}
#' @export
bin_2d = function(x,y,nbins=100,x.bin=NA,y.bin=NA){
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
  
  if(is.na(x.bin)){
    range.x=abs(diff(range(df[,1],na.rm=TRUE)))
    #x.bin <- seq((min(df[,1],na.rm=TRUE)-range.x/2), (max(df[,1],na.rm=TRUE)+range.x/2), length=nbins)
    x.bin <- approx(1:length(sort(df$x)),sort(df$x),seq(1,length(sort(df$x)),length.out = nbins))$y #adjust it along y
  }
  if(is.na(y.bin)){
    range.y=abs(diff(range(df[,2],na.rm=TRUE)))
    #y.bin <- seq((min(df[,2],na.rm=TRUE)-range.y/2), (max(df[,2],na.rm=TRUE)+range.y/2), length=nbins)
    y.bin <- approx(1:length(sort(df$y)),sort(df$y),seq(1,length(sort(df$y)),length.out = nbins))$y #adjust it along y
    
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
  
  density = (freq2D/sum(freq2D))/(abs(mean(diff(x.bin)))*abs(mean(diff(y.bin))))
  out = list("density" = density,"x.bin"= x.bin,"y.bin"=y.bin)
  
  return(out)
}

#' @export
#show a map, timeseries, and age model diagram..
summary_plot = function(L){
  library(grid)
  library(gridExtra)
  map = map.lipd(L,extend.range = 5)
  
  #plot paleoData
  
  print("What should we plot on the X-axis?")
  print("We'll look for age or year...")
  age=select.data(L,varName = "age",always.choose = FALSE,altNames="year")
  
  print("What should we plot on the Y-axis?")
  variable=select.data(L)
  
  paleoPlot = plot_line(age,variable)
  paleoPlot = paleoPlot + labs(title = paste("PaleoData:",variable$variableName))
  
  #do chron.
  if(!is.null(L$chronData[[1]]$chronMeasurementTable)){
    #looking for age model data.
    
    print("looking for ages...")
    age2=select.data(L,varName = "age14C",always.choose = FALSE,altNames="year",where = "chronData")
    
    #     print("looking for age uncertainty")
    #     ageUnc = select.data(L,varName = "age14CUncertainty",always.choose = FALSE,altNames=c("uncertainty","error","age"),where = "chronData")
    
    print("looking for depths....")
    depth=select.data(L,varName = "depth",always.choose = FALSE,where = "chronData")
    c.df = data.frame(x=age2$values,y=depth$values)
    chronPlot = plot_line(age2,depth)
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

#' @export
plot_line = function(X,Y,color="black",add.to.plot=ggplot()){
  
  #X and Y and are lipd variables, including values, units, names, etc...
  df = data.frame(x = X$values, y = Y$values)
  plot = add.to.plot+ geom_line(data=df,aes(x=x,y=y),colour =color)+
    ylab(paste0(Y$variableName," (",Y$units,")"))+
    xlab(paste0(X$variableName," (",X$units,")"))+
    theme_bw()
  if(tolower(X$variableName)=="age"){
    plot = plot+scale_x_reverse()
  }
  return(plot)
  
  
}


#' @export
plot_timeseries.lines = function(X,Y,alp=.2,color = "blue",maxPlotN=1000,add.to.plot=ggplot()){
  #check to see if time and values are "column lists"
  if(is.list(X)){X=X$values}
  if(is.list(Y)){Y=Y$values}
  
  
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
  
  
  library(ggplot2)
  linePlot = add.to.plot+
    geom_path(data=dfXY,aes(x=x,y=y),colour = color,alpha=alp)+
    theme_bw()
  
  return(linePlot)
  
}
#' @export
plot_timeseries.ribbons = function(X,Y,alp=1,probs=pnorm(-2:2),x.bin=NA,y.bin=NA,nbins=1000,colorLow="white",colorHigh="grey70",lineColor="Black",lineWidth=1,add.to.plot=ggplot()){
  #check to see if time and values are "column lists"
  if(is.list(X)){X=X$values}
  if(is.list(Y)){Y=Y$values}
  
  
  
  
  X=as.matrix(X)
  Y=as.matrix(Y)
  
  #check to make sure that at least one is a matrix
  if(ncol(X)==1 & ncol(Y)==1){
    #then just plot a line
    df = data.frame(x=X,y=Y)
    bandPlot=ggplot(data=df)+geom_line(aes(x=X,y=Y),colour=lineColor)+theme_bw()
    
  }else{
    
    if(nrow(X)!=nrow(Y)){
      stop("X and Y must have the same number of observations")
    }
    #  good =which(!is.)
    
    
    binned = bin_2d(X,Y,x.bin=x.bin,y.bin = y.bin,nbins=nbins)
    
    #find cum sum probabilities  
    colSums = apply(binned$density,2,sum)
    sumMat= t(matrix(colSums, nrow=length(colSums),ncol=nrow(binned$density)))
    bmcs = apply(binned$density/sumMat,2,cumsum)
    
    good.cols = which(!apply(is.na(bmcs),2,all))
    
    probMat = matrix(data = NA,nrow=length(good.cols),ncol=length(probs))
    for(p in 1:length(probs)){
      probMat[,p]=apply(bmcs[,good.cols],MARGIN=2,function(x) approx(x,binned$y.bin,probs[p])$y)
    }
    
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
      center = data.frame(x=binned$x.bin[good.cols],y=probMat[,ncol(probMat)/2+1])
      
      bandMat =  probMat[,-(ncol(probMat)/2+1)]
    }else{
      center =NA
      bandMat =  probMat
    }
    
    #deal with colors
    fillCol=colorRampPalette(c(colorLow,colorHigh))( ncol(bandMat)/2+1 )[-1]
    lineColor="black"
    
    
    library(ggplot2)
    for(b in 1:(ncol(bandMat)/2)){
      if(b==1){
        bandPlot = add.to.plot+theme_bw()
      }
      bands=data.frame(x=binned$x.bin[good.cols],ymin = bandMat[,b],ymax = bandMat[,ncol(bandMat)-b+1])
      bandPlot = bandPlot+
        geom_ribbon(data=bands,aes(x=x,ymin=ymin,ymax=ymax),fill=fillCol[b],alpha=alp)
    }
    
    if(!is.na(center)){
      bandPlot = bandPlot+
        geom_line(data=center,aes(x=x,y=y),colour=lineColor,size=lineWidth)
    }
    
  }
  
  return(bandPlot)
  
  
  
  
  
  
  
}
#' @export
plot_scatter.ens = function(X,Y,alp=.2,maxPlotN=1000){
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
  
  library(ggplot2)
  scatterplot = ggplot(data=dfXY)+
    geom_point(aes(x = x,y=y),alpha=alp)+
    theme_bw()
  
  return(list("plot" = scatterplot,"pX"=pX,"pY"=pY))
}
#' @export
plot_trendlines.ens = function(mb.df,xrange,pXY=1:nrow(mb.df) ,alp=.2 ,color = "red",add.to.plot=ggplot()){
  #mb.df = dataframe of slopes (column 1) and intercepts (column 2)
  #xrange = range of x values (min and max)
  #pXY = index of which observations to use
  #create a path for the fit lines
  #add.to.plot if you want to add this to an existing plot, put that object here.
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
  
  library(ggplot2)
  trendlines = add.to.plot+
    geom_path(data=dfi,aes(x=x,y=y),colour = color,alpha=alp)+
    theme_bw()+
    xlim(xrange)
  
  
  return(trendlines)
}

#' @export
plot_corr.ens = function(cor.df,corStats,bins=40,lineLabels = rownames(corStats),add.to.plot=ggplot()){
  # evaluate preliminary quantities
  rng <- range(cor.df$r)
  bw = (rng[2]-rng[1])/bins
  
  cs = colSums(cor.df)
  sig_frac = cs["sig_fdr"][[1]]/dim(cor.df)[1]*100
  sig_lbl = paste0("Fraction significant: ", sig_frac, "%")
  # Now the plotting begins
  lbf = c("All correlations","p < 0.05 + FDR")
  h = ggplot() + ggtitle("Correlation Distribution") + # initialize plot
    geom_histogram(data=cor.df,aes(x=r,y=..count..,fill = factor(sig_fdr)), position = 'identity', colour = "white", binwidth = bw) +
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
    annotate("text",x = 0.7*xlims[2],y=0.4*ylims[2], label = sig_lbl,color="Chartreuse4")+theme_bw() # add fraction of significant correlations
  #customize legend
  h = h + theme(legend.position = c(0.2, 0.8),
                legend.title = element_text(size=10, face="bold"),
                legend.text = element_text(size=8),
                legend.key = element_rect(fill = "transparent",
                                          colour = "transparent"),
                legend.background = element_rect(fill=alpha('white', 0.3)))

  return(h)
}

plot_pvals.ens = function(cor.df,alpha = 0.05){
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
                                labels=lbl)+theme_bw()
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

#  THIS SHOULD BE DEPRECATED - kept for backwards compatibility

#' @export
plot_hist.ens = function(ensData,ensStats=NA,bins=50,lineLabels = rownames(ensStats),add.to.plot=ggplot(),alp=1,fill="grey50"){
  #plots a histogram of ensemble distribution values, with horizontal bars marking the distributions
  ensData = data.frame("r"=c(ensData))
  library(ggplot2)
  
  
  
  histPlot = add.to.plot+
    geom_histogram(data=ensData,aes(x=r,y=..density..),colour="white",bins=bins,fill=fill,alpha=alp)+
    theme_bw()+
    ylab("Probability density")
  if(!all(is.na(ensStats))){
    #make labels better
    goodName= c("-2σ","-1σ","Median","1σ","2σ")
    realProb= c(pnorm(-2:2))
    for(i in 1:length(lineLabels)){
      p=which(abs(as.numeric(lineLabels[i])-realProb)<.001)
      if(length(p)==1){
        lineLabels[i]=goodName[p]
      }
    }
    ensStats$ll=lineLabels
    histPlot = histPlot + geom_vline(data=ensStats,aes(xintercept = values),colour="red") +
      geom_label(data = ensStats, aes(x = values, y=0,label=ll))
    
  }
  return(histPlot)
}

#' @export
plot_ensemble.PCA <- function(ens.PC.out,TS,map.type="line",which.PCs=c(1,2),f=.6,color="temp",dotsize=5,restrict.map.range=TRUE,shape.by.archive=TRUE,data.file=NA,projection="mollweide",boundcirc=TRUE,probs=pnorm(-2:2)){
  library(ggmap)
  library(ggplot2)
  library(gridExtra)
  #map.type can be "google" or "polar"
  
  #get data out of the TS
  lat = sapply(TS,"[[","geo_latitude")
  lon = sapply(TS,"[[","geo_longitude")
  archive = sapply(TS,"[[","archiveType")
  
  
  #shape by archive!###
  arch.shape=c()
  for(i in 1:length(archive)){
    if(shape.by.archive){
      if (grepl(tolower(archive[i]),"lake")){arch.shape[i]="lake"}
      else if (grepl(tolower(archive[i]),"marine sediments")){arch.shape[i]="marine"}
      else if (grepl(tolower(archive[i]),"speleothem")){arch.shape[i]="speleothem"}
      #else if (grepl(tolower(archive[i]),"peat")){arch.shape[i]=18}
      else {arch.shape[i]="unknown"}
    }else{arch.shape[i]=21} #make them all circles if not shape.by.archive
  }
  
  arch.shape=factor(arch.shape)
  archiveShapes=c(21,22,24)
  if(!any(grepl(pattern="speleothem",arch.shape))){archiveShapes = archiveShapes[-3] }
  if(!any(grepl(pattern="marine",arch.shape))){archiveShapes = archiveShapes[-2] }
  if(!any(grepl(pattern="lake",arch.shape))){archiveShapes = archiveShapes[-1] }
  
  #end shape by archive
  
  
  
  plotlist=list()
  maplist=list()
  
  
  
  #   sorted =  apply(dat.mat[[wm]]$PC$ensemblePCs[,1,],MARGIN = c(2),sort)
  medianPCs = apply(ens.PC.out$PCs,MARGIN = c(1,2),median,na.rm=TRUE)
  loadingSDs = apply(ens.PC.out$loadings,MARGIN = c(1,2),sd,na.rm=TRUE)
  medianLoadings = apply(ens.PC.out$loadings,MARGIN = c(1,2),median,na.rm=TRUE)
  
  #get a base map
  map = base.map(lon,lat,map.type = map.type,f=f,projection = projection,restrict.map.range = restrict.map.range)
  
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
    scaleColors = assign.high.low.colors(color)
    
    
    maplist[[i]] = map +  geom_point(aes(x=lon,y=lat,fill=medLoad,size=sdDots,shape = shape), data=dd) +
      scale_shape_manual(values = archiveShapes) +
      scale_size(range = c(dotsize,1)) +
      scale_fill_gradient2(name="Loadings",low=scaleColors[1],high=scaleColors[2],guide="colourbar")
    
    
    
    
    bddf = data.frame(sampleDepth = ens.PC.out$meanDataDensity,age = ens.PC.out$age)
    #TODO and sample depth plot  
    plot_sample.depth = ggplot(data=bddf)+geom_bar(aes(x=age,y=sampleDepth),fill="gray20",stat="identity")+
      ylab("fractional mean sample depth")+
      xlab("age (yr BP)")+
      theme_bw()+
      scale_x_reverse()
    
    plotlist[[i]] = plot_timeseries.ribbons(X=ens.PC.out$age,Y=ens.PC.out$PCs[,which.PCs[i],],x.bin =ens.PC.out$age,nbins = 10000 ,probs = probs) 
    medianVarExp = median(ens.PC.out$variance[which.PCs[i],])
    sdVarExp = sd(ens.PC.out$variance[which.PCs[i],])
    varExpStr  = paste(as.character(signif(medianVarExp*100,2)),"±",as.character(signif(sdVarExp*100,1)))
    plotlist[[i]] = plotlist[[i]]+ggtitle(paste("Variance explained =",varExpStr,"%"))+
      scale_x_reverse()+
      labs(y=paste0("PC",which.PCs[i]),x="Age (yr BP)")
    
  }
  
  df2=data.frame(age=ens.PC.out$age,sampleDensity = ens.PC.out$meanDataDensity)
  backDensity = ggplot()+geom_bar(data=df2,aes(x=age,y=sampleDensity),stat = 'identity')
  alllist = append(maplist,plotlist)
  tt=1:length(alllist)
  alllist = alllist[c(tt[tt%%2==1],tt[tt%%2==0])]
  
  summaryPlot = grid.arrange(grobs=alllist,ncol=2,widths=c(1.5,1.5))
  
  return(list(lines = plotlist, maps= maplist,summary =summaryPlot,sampleDepth = plot_sample.depth))
  
}
