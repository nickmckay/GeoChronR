iterativeBrokenStick <- function(age,values,useKinit=20,maxCP=6,confidence.level=.90,plot.opt=FALSE){
  #pt is a single, paleoTimeseries object
  
  if(is.list(age)){
    ao = age
    age = as.matrix(age$values)
  }
  if(is.list(values)){
    vo = values
    values = as.matrix(values$values)
  }
  
  dati<-data.frame(x=age,y=values)
  
  #replace any Nans with NAs
  dati[dati=="NaN"]=NA
  
  #force into gaussian
  dati$y=gaussianize(dati$y,jitter=FALSE)#note, jitter allows for gaussianization with ties. It's also stochastic, needs to be run in ensemble mode
  
  
  
  out.lm<-lm(y~x,data=dati)
  
  #determine use K, must have at least 3 points per seg
  #length(dati$x)/3
  useK <- useKinit
  psi.est=NA
  count=0
  
  while(TRUE){ #iteratively reduce changepoints by testing for significant changes in slope
    count=count+1
    print(count)
    if(count>useKinit+10){
      break
    }
    if(useK<=0){
      no.cp=TRUE
      break
    }
    if(any(is.na(psi.est))){
      
      o<-try(segmented::segmented.lm(out.lm,seg.Z=~x,psi=list(x=NA),control=segmented::seg.control(it.max=1e3,stop.if.error=FALSE,n.boot=0,K=useK)))
    }else{
      
      o<-try(segmented::segmented(out.lm,seg.Z=~x,psi=psi.est,segmented::seg.control(it.max=1e3,stop.if.error=TRUE,n.boot=20)))
    }
    
    if(all(grepl("lm",class(o))) | all(grepl("try-error",class(o)))){
      if(useK<=1){
        no.cp=TRUE
        break
      }else{
        psi.est=NA
        useK=useK-1}
    }else{
      so <- segmented::slope(o,conf.level=confidence.level)
      so.max <- so$x[,5]
      so.min <- so$x[,4]
      goodBP <- c()
      for(i in 1:length(o$psi[,2])){
        if(so.max[i+1] < so.min[i] | so.max[i] < so.min[i+1]){
          goodBP[i] <- TRUE
        }else{
          goodBP[i] <- FALSE
        }}
      
      
      if(length(which(goodBP)) <= maxCP & length(which(goodBP)) > 0){
        if(length(o$psi[,2])==length(which(goodBP))){
          no.cp = FALSE
          break
        }else{
          psi.est <- o$psi[goodBP,2]
        }
      }else{
        useK=useK-2
        psi.est <- NA
      }
    }
  }
  
  if(!no.cp){
    ncp=length(o$psi[,2])
  }else{
    ncp=0
  }
  
  if(!no.cp){
    #estimate values at changepoints
    cp.y=predict(o,data.frame(x=o$psi[,2]))
    
    cp <- o$psi[,2]
    cp.se <- o$psi[,3]
    slope.change <- abs(diff(segmented::slope(o)$x[,1]))
    
    
    #sort by value at changepoint
    si = sort(cp.y,decreasing=TRUE,index.return=TRUE)$ix
    cp <- cp[si]
    cp.se <- cp.se[si]
    
    cp.y <- cp.y[si]
    
    print(cp)
    if(length(cp)==0){
      cp=NA
      cp.se=NA
      cp.y = NA
    }
  }else{
    cp=NA
    cp.se=NA  
    cp.y = NA
    
  }
  
  if(plot.opt){
    #x11()
    plot(dati,xlab="Age (BP)",ylab = "Values")
    #title(names(pts)[ts])
    if(!no.cp){
      plot(o,add=TRUE,col="red",se=TRUE)
      segmented::lines.segmented(o,conf.level = confidence.level,col="red")
      abline(v=cp,col="blue")
    }
  }
  
return(list(cp=cp,cp.se=cp.se,cp.y = cp.y,slope.change = slope.change))
}
