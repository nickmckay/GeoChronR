bin_2d = function(x,y,nbins=100,x.bin=NA,y.bin=NA){
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

map.lipd = function(L,color="red",size=8,shape = 16,zoom=4){
 library(ggmap)
  dfp = data.frame(lon = L$geo$longitude,lat = L$geo$latitude)
  basemap = get_googlemap(center = c(dfp$lon,dfp$lat),zoom=zoom)
  map = ggmap(ggmap = basemap)  + geom_point(data=dfp,aes(x=lon,y=lat),colour = color,size=size,shape = shape) 
  if(!is.null( L$geo$siteName)){
    dfp$sitename = L$geo$siteName
    map=map+geom_label(data=dfp,aes(x=lon,y=lat,label=sitename),nudge_y=-1)
  }
  return(map)
}

map.lipds = function(D,color= sapply(D,"[[","archiveType"),size=8,shape = 16 ){
  library(ggmap)
  dfp = data.frame(lon = sapply(D,function(x) x$geo$longitude),lat = sapply(D,function(x) x$geo$latitude),color,shape)
  dfp = dfp[!is.na(dfp$lat) & !is.na(dfp$lon),]
  rLat=range(dfp$lat)
  rLon=range(dfp$lon)
  cent=c(mean(rLon),mean(rLat))
  zoom = calc_zoom(rLon,rLat)
  basemap = get_googlemap(center = cent,zoom=zoom,source = "stamen")

  

  map = ggmap(ggmap = basemap)  + geom_point(data=dfp,aes(x=lon,y=lat,colour = color),shape = shape,size=7)
  return(map)
}


#show a map, timeseries, and age model diagram..
summary.plot = function(L){
  library(grid)
  library(gridExtra)
  map = map.lipd(L)
  
  #plot paleoData
  
  print("What should we plot on the X-axis?")
  print("We'll look for age or year...")
  age=select.data(L,varName = "age",always.choose = FALSE,altNames="year")
  
  print("What should we plot on the Y-axis?")
  variable=select.data(L)
  
  paleoPlot = plot.line(age,variable)
  paleoPlot = paleoPlot + labs(title = paste("PaleoData:",variable$variableName))
  
  #do chron.
  if(any(names(L)=="chronData")){
    #looking for age model data.
    
    print("looking for ages...")
    age2=select.data(L,varName = "age14C",always.choose = FALSE,altNames="year",where = "chronData")
    
#     print("looking for age uncertainty")
#     ageUnc = select.data(L,varName = "age14CUncertainty",always.choose = FALSE,altNames=c("uncertainty","error","age"),where = "chronData")
    
    print("looking for depths....")
    depth=select.data(L,varName = "depth",always.choose = FALSE,where = "chronData")
    c.df = data.frame(x=age2$values,y=depth$values)
   chronPlot = plot.line(age2,depth)
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

#X and Y and are LiPD variables, including values, units, names, etc...
plot.line = function(X,Y,color="black",add.to.plot=ggplot()){
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



plot.timeseries.lines = function(X,Y,alp=.2,color = "blue",maxPlotN=1000,add.to.plot=ggplot()){
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
plot.timeseries.ribbons = function(X,Y,alp=.2,probs=pnorm(-2:2),x.bin=NA,y.bin=NA,nbins=1000,colorLow="white",colorHigh="grey70",lineColor="Black",lineWidth=1){
  #check to see if time and values are "column lists"
  if(is.list(X)){X=X$values}
  if(is.list(Y)){Y=Y$values}

  X=as.matrix(X)
  Y=as.matrix(Y)

  if(nrow(X)!=nrow(Y)){
    stop("X and Y must have the same number of observations")
  }
  
  
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
    bandPlot = ggplot()+theme_bw()
  }
  bands=data.frame(x=binned$x.bin[good.cols],ymin = bandMat[,b],ymax = bandMat[,ncol(bandMat)-b+1])
  bandPlot = bandPlot+
    geom_ribbon(data=bands,aes(x=x,ymin=ymin,ymax=ymax),fill=fillCol[b])
}

  if(!is.na(center)){
    bandPlot = bandPlot+
      geom_line(data=center,aes(x=x,y=y),colour=lineColor,size=lineWidth)
  }
  
  

return(bandPlot)




  
  
  
}

plot.scatter.ens = function(X,Y,alp=.2,maxPlotN=1000){
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

plot.trendlines.ens = function(mb.df,xrange,pXY=1:nrow(mb.df) ,alp=.2 ,color = "red",add.to.plot=ggplot()){
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



plot.hist.ens = function(ensData,ensStats=NA,bins=50,lineLabels = rownames(ensStats),add.to.plot=ggplot(),alp=1,fill="grey50"){
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
