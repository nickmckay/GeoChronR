#make a base map, with some options. 
base.map = function(lon,lat,map.type="google",f=.3,restrict.map.range=TRUE,projection="mercator",boundcirc=FALSE,global=FALSE){
  library("ggplot2")
  library("ggmap")
  library("mapproj")
  
  try(load("bound.box.Rdata"),silent = TRUE)
if(!exists("bb")){bb=1000}
bbnew <- make_bbox(lon,lat,f=f)
if(all(bbnew==bb)){
  lnp=TRUE
  #load the file you already downloaded
}else{
  lnp=FALSE
  bb=bbnew
  if(bb[4] > 89){
    diff <- bb[4]-89
    bb[2] <- bb[2]-(diff*2)
    bb[4] <- bb[4]-(diff*2)
  }
  save(bb,file="bound.box.Rdata")
}

if(map.type=="google"){
  if(lnp){
    try(load("newmap.Rdata"),silent = TRUE)
  }
  if(!exists("newmap")){
    newmap <- get_map(location=bb,maptype="terrain",source="google")
    save(newmap,file="newmap.RData")
  }
  baseMap = ggmap(newmap,maprange=TRUE) 
}else if(map.type=="line"){
  
  res=1
  low.lat <- min(lat)-5
  if(restrict.map.range){
    x_lim = bb[c(1,3)]
    y_lim = bb[c(2,4)]
  }else{
    x_lim = c(-190,190)
    y_lim = c(min(bb[c(2,4)]),90)
  }
  x_cell_lim <- x_lim + c(1, -1) * res/2
  y_cell_lim <- y_lim+ c(1, -1) * res/2
  if(global){
    dum = map(plot = FALSE)
  }else{
  dum = map(xlim = x_lim, ylim = y_lim, plot = FALSE,wrap=TRUE)
  }
  #dum = map(xlim = x_cell_lim, ylim = y_cell_lim, plot = FALSE)
  ant_ggplot = data.frame(dum[c("x","y")])
 badLines = which(ant_ggplot$x < min(x_lim) | ant_ggplot$x > max(x_lim) | ant_ggplot$y < min(y_lim) | ant_ggplot$y > max(y_lim))
 ant_ggplot[badLines,]=NA
  
  
  bbdf=data.frame(bb)
  
  baseMap <- ggplot() +  geom_path(aes(x = x, y = y), data = ant_ggplot)
  if(restrict.map.range){
    baseMap  = baseMap  +  geom_rect(aes(xmax=bb[3]-.1,xmin=bb[1]+.1,ymax=bb[4]-.1,ymin=bb[2]+.1),fill=NA, colour="black",data=bbdf)
  }else if(boundcirc){
    boundcirc = data.frame(x = seq(-180,180),y=rep(bb[2]+.1,length.out=length(seq(-180,180))))
    baseMap  = baseMap  + geom_path(aes(x = x, y = y), data = boundcirc)
  }
  baseMap  = baseMap  +   
    theme(panel.background = element_rect(fill="white"), axis.ticks = element_blank(), axis.text.y = element_blank(),axis.text.x = element_blank(),
                                axis.title.x = element_blank(), axis.title.y = element_blank(),
                                panel.border = element_blank())+
    coord_map(projection)
}

return(baseMap )


}



assign.high.low.colors = function(colors){
  
  if(length(colors==1)){
    #make point map
    if(grepl(pattern = "temp",colors)){
      lowcolor="blue"
      hicolor="red"
    }else if(grepl(pattern = "precip",colors)){
      lowcolor="saddlebrown"
      hicolor="blue"
    }else if(grepl(pattern = "drought",colors)){
      hicolor="saddlebrown"
      lowcolor="green"
    }else{
      warning("dont recognize color scale, useing blue-red")
      lowcolor="blue"
      hicolor="red"
    }
    colors = c(lowcolor,hicolor)
  }
  
  return(colors)
}
