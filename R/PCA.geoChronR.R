

PCA.ens <-  function(bin.list,method='ppca',plotPCs=c(1,2),weights=NA,map.type="google",PCAtype="corr",nPCs=max(plotPCs),plot.opt=TRUE,pdf.opt=FALSE,pdfname=NA,plotf=.6,color="temp",dotsize=5,port.CompleteObs=TRUE,site.nums=NA,restrict.map.range=TRUE,nEns=1000){
  #the input for this function is the list output of build.matrices(), and should include one more data matrices
  #of evenly spaced data, where the first (leftmost) column is age, and the rest are data. The list should also include sitenames, latitude and longitude
  #the option type controls whether the analysis is done on a correlation (default) or covariance matrix ("cov")
#the function uses the pcaMethods library to do the heavy lifting
library("matrixStats")
library("pcaMethods")
  time = bin.list[[1]]$time

nD = length(bin.list) #how many sites?
nEnsSite = sapply(bin.list,function(x) ncol(x$matrix)) #how many ensembles at each site


#dat.mat[[1]]$record.density <- rowSums(!is.na(dat.mat[[1]]$matrix))/NCOL(dat.mat[[1]]$matrix)
#dat.mat <- tmat2

#setup output
loads = array(data = NA,dim = c(nD,nPCs,nEns))
PCs = array(data = NA,dim = c(length(time),nPCs,nEns))
varExp = array(data = NA,dim = c(nPCs,nEns))

pb=txtProgressBar(min=1,max=nEns,style = 3)
for(n in 1:nEns){#for each ensemble member
  #build a matrix from the list
  PCAMAT=matrix(NA,nrow=length(time),ncol = nD)
  for(j in 1:nD){
    PCAMAT[,j] = bin.list[[j]]$matrix[,sample.int(nEnsSite[j],size=1)]
  }
  PCAMAT[is.nan(PCAMAT)]=NA
  
  if(any(!is.na(weights))){
    normmat=scale(PCAMAT)
    wmat=scale(normmat,scale=1/weights)
    PCAMAT=wmat
  }
  
  #remove means, and scale if correlation matrix
  if(PCAtype=="corr"){
    pca.out=pca(PCAMAT,method,center=TRUE,scale="vector",nPcs=nPCs)
  }else if(PCAtype=="cov"){
    pca.out=pca(PCAMAT,method,center=TRUE,scale="none",nPcs=nPCs)
  }
  
  loads[,,n]=loadings(pca.out)
  PCs[,,n]=scores(pca.out)
  
  
  #reorient PCs such that the mean loadings are positive
  meanloadings <- colMeans(loads[,,n])
  for (npc in 1:NCOL(PCs)){
    if(meanloadings[npc] < 0){
      loads[,npc,n]=-1*loads[,npc,n]
      PCs[,npc,n]=-1*PCs[,npc,n]
    }}
  
  
  
  #variance explained  
  cumvariance=R2cum(pca.out)
  variance=cumvariance
  for (v in 2:length(variance)){
    variance[v]=cumvariance[v]-cumvariance[v-1]
  }
  varExp[,n]=variance
  setTxtProgressBar(pb,n)
}
close(pb)


if(any(!is.na(weights))){
  ens.PC.out <- list(loads/weights,PCs,varExp,time)
  ens.PC.out$weightedPCmat=PCAMAT
}else{
  ens.PC.out <- list(loads,PCs,varExp,time)
}
names(ens.PC.out) <- c("loadings","PCs","variance","age")
return(ens.PC.out)

}


plotEnsemblePCA <- function(ens.PC.out,TS,map.type="lines",which.PCs=c(1,2),pdfopt=FALSE,pdfname=NA,f=.6,legend=FALSE,color="temp",dotsize=5,site.nums=NA,number.sites=FALSE,restrict.map.range=TRUE,shape.by.archive=TRUE,data.file=NA,projection="mollweide",boundcirc=TRUE){
  
  
  
  #map.type can be "google" or "polar"

  #get data out of the TS
  lat = sapply(TS,"[[","geo_latitude")
  lon = sapply(TS,"[[","geo_longitude")
  archive = sapply(TS,"[[","archiveType")
  
  
  
  #outmat <- PCA(allmat)
  #dat.mat=outmat; wm=1; which.PCs=c(1,2,3); pdfopt=FALSE
  
  #shape by archive!###
  arch.shape=c()
  for(i in 1:length(archive)){
    if(shape.by.archive){
      if (grepl(tolower(archive[i]),"lake")){arch.shape[i]="lake"}
      else if (grepl(tolower(archive[i]),"marine sediments")){arch.shape[i]="marine"}
      else if (grepl(tolower(archive[i]),"speleothem")){arch.shape[i]="speleothem"}
      #else if (grepl(tolower(archive[i]),"peat")){arch.shape[i]=18}
      else {arch.shape[i]="unknown"}
    }else{arch.shape[i]=15} #make them all circles if not shape.by.archive
  }

  arch.shape=factor(arch.shape)
archiveShapes=c(16,15,17)
if(!any(arch.shape=="speleothem")){archiveShapes = archiveShapes[-3] }
if(!any(arch.shape=="marine")){archiveShapes = archiveShapes[-2] }
if(!any(arch.shape=="lake")){archiveShapes = archiveShapes[-1] }

#end shape by archive



  plotlist=list()
  maplist=list()
  PCnames=names(dat.mat[[wm]]$PC$loadings)
  medianLoadings = apply(dat.mat[[wm]]$PC$ensembleLoadings,MARGIN = c(1,2),median,na.rm=TRUE)

#   sorted =  apply(dat.mat[[wm]]$PC$ensemblePCs[,1,],MARGIN = c(2),sort)
    medianPCs = apply(ens.PC.out$PCs,MARGIN = c(1,2),median,na.rm=TRUE)
   loadingSDs = apply(ens.PC.out$loadings,MARGIN = c(1,2),sd,na.rm=TRUE)
   medianLoadings = apply(ens.PC.out$loadings,MARGIN = c(1,2),median,na.rm=TRUE)
   
#



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
    
    #get a base map
    map2 = base.map(dd$lon,dd$lat,map.type = map.type,f=f,projection = projection,restrict.map.range = restrict.map.range,boundcirc = TRUE)
    
    #infer colors
    scaleColors = assign.high.low.colors(color)
    
  
      maplist[[i]] = map2 +  geom_point(aes(x=lon,y=lat,size=sdDots, shape = shape), data=dd,colour="black")+ 
        geom_point(aes(x=lon,y=lat,colour=medLoad,size=sdDots,shape = shape), data=dd) +
        #scale_shape_manual(values = archiveShapes) +
        scale_size(range = c(dotsize,1)) +
        scale_colour_gradient2(name="Loadings",low=scaleColors[1],high=scaleColors[2],guide="colourbar")

  }
   
   
   
  plotlist[[i]] = plot.timeseries.lines(X=ens.PC.out$age,Y=ens.PC.out$PCs[,which.PCs[i],]) 
   
   
  PCs <- dat.mat[[wm]]$PC$PCs
  PCs[dat.mat[[wm]]$record.density < 1,]=NA



  df2=data.frame(age=dat.mat[[wm]]$PC$AGE,PC=dat.mat[[wm]]$record.density)
  df2$panel <- "nrecs"

  for (i in 1:length(which.PCs)){
    df1=data.frame(age=dat.mat[[wm]]$PC$AGE,PC=medianPCs[,which.PCs[i]],PC100=PCs)
    df1$panel <- "PC"

    medianVarExp = median(dat.mat[[wm]]$PC$ensembleVariance[which.PCs[i],])
    sdVarExp = sd(dat.mat[[wm]]$PC$ensembleVariance[which.PCs[i],])
    print(medianVarExp)
    print(sdVarExp)
    varExpStr  = paste(as.character(signif(medianVarExp*100,2)),"±",as.character(signif(sdVarExp*100,1)))


#    sorted =  (t(apply(((scale(dat.mat[[wm]]$PC$ensemblePCs[,which.PCs[i],]))),MARGIN = 1,sort)))
    sorted =  (t(apply((((dat.mat[[wm]]$PC$ensemblePCs[,which.PCs[i],]))),MARGIN = 1,sort)))

    PChi1 = (sorted[ , floor(ncol(sorted)*pnorm(1))])
    PChi2 = (sorted[ , floor(ncol(sorted)*pnorm(2))])
    PClo1 = (sorted[ , ceiling(ncol(sorted)*pnorm(-1))])
    PClo2 = (sorted[ , ceiling(ncol(sorted)*pnorm(-2))])
    PCmed = (sorted[ , round(ncol(sorted)*pnorm(0))])
    PCunc=data.frame(hi1=PChi1,hi2=PChi2,lo1=PClo1,lo2=PClo2,age=dat.mat[[wm]]$PC$AGE,PCmed=PCmed)
    PCmat = as.matrix(PCunc[,c(1:4,6)])
    PCmat=scale(PCmat,center = matrix(mean(PCunc$PCmed),5),scale=matrix(sd(PCunc$PCmed),5))
    PCunc[,c(1:4,6)]=PCmat

    if(!is.na(data.file)){
      write.csv(PCunc,file = paste0(data.file,as.character(which.PCs[i]),".csv"))

    }


    PCunc$panel = "PC"
    pp <- ggplot(data = PCunc) + theme_bw()
    pp <- pp + facet_grid(panel ~ ., scale = "free_y", space = "free_y")
    pp <- pp+ geom_ribbon(aes(x=age,ymin=lo2,ymax=hi2),fill="gray90")+
   geom_ribbon(aes(x=age,ymin=lo1,ymax=hi1),fill="gray")+

      geom_line(aes(x=age,y=PCmed))+
     # layer(data=PCunc,mapping=aes(x=age,y=PCmed),geom = "line",stat="identity",position = "identity")+
      scale_x_reverse(breaks=seq(0,max(df1$age),by=2000))+ggtitle(paste("Variance explained =",varExpStr,"%"))
    pp <- pp + layer(data = df2, mapping=aes(x=age,y=PC), geom = c( "area"), position = "identity",stat="identity")

    #KLUDGE FOR ggplot2 stat_identity bug
    for(k in 1:length(pp$layers)){
      pp$layers[[k]]$geom_params=list(na.rm=FALSE)
    }

    plotlist[[i]] <-  pp
  }
  save(pp,file="~/Dropbox/vplR/pp.mat")
  alllist=append(maplist,plotlist)
  if(pdfopt){
    if(is.na(pdfname)){
      pdfname=paste0("PCA_plots-",format(Sys.time(), "%Y-%m-%d-%H-%M"),".pdf")
    }else
      pdfname=pdfname
  }else{
    pdfname=NA
  }

  if(map.type=="line"){
    multiplot(plotlist=alllist,cols=2,colwidth=c(1.8,1.5),pdfname=pdfname)
  }else{
    multiplot(plotlist=alllist,cols=2,colwidth=c(1.2,1.5),pdfname=pdfname)
  }
}


