#' @export
mapLipd = function(L,color="red",size=8,shape = 16,zoom=4,map.type="google",extend.range=10){
  library(ggmap)
  dfp = data.frame(lon = L$geo$longitude,lat = L$geo$latitude)
  basemap = baseMap(dfp$lon,dfp$lat,extend.range=extend.range,map.type = map.type)
  map = basemap + geom_point(data=dfp,aes(x=lon,y=lat),colour = color,size=size,shape = shape) 
  if(!is.null( L$geo$siteName)){
    dfp$sitename = L$geo$siteName
    map=map+geom_label(data=dfp,aes(x=lon,y=lat,label=sitename),nudge_y=-1)
  }
  return(map)
}


#' @export
mapLipds = function(D,shape= 21,size=8,fill = sapply(D,"[[","archiveType") ,map.type="google",f=.3,restrict.map.range=TRUE,boundcirc=FALSE,global=FALSE){
  library(ggmap)
  dfp = data.frame(lon = sapply(D,function(x) x$geo$longitude),lat = sapply(D,function(x) x$geo$latitude),fill,shape)
  dfp = dfp[!is.na(dfp$lat) & !is.na(dfp$lon),]
  basemap = baseMap(dfp$lon,dfp$lat,map.type = map.type,f=f,restrict.map.range = restrict.map.range,boundcirc = boundcirc,global=global)
  map = basemap  + geom_point(data=dfp,aes(x=lon,y=lat,fill = fill),shape = shape,size=7)
  return(map)
}


#make a base map, with some options. 
#' @export
baseMap = function(lon,lat,map.type="google",f=.3,restrict.map.range=TRUE,projection="mercator",boundcirc=FALSE,global=FALSE,extend.range=10){
  library("ggplot2")
  library("ggmap")
  library("mapproj")
  
  if(length(lat)==1 & length(lon)==1){
    lon = lon + c(-extend.range,extend.range)
    lat = lat + c(-extend.range,extend.range)
}
  
  
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
  #save(bb,file="bound.box.Rdata")
}

if(map.type=="google"){
  #if(lnp){
  #  newmap = try(load("newmap.Rdata"),silent = TRUE)
  #  if(grepl(class(newmap),pattern="error")){newmap <- get_map(location=bb,maptype="terrain",source="google")}
  #}else{
    newmap <- get_map(location=bb,maptype="terrain",source="google")
  #}
#  save(newmap,file="newmap.RData")
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
    wbb=which(min(abs(bb[c(2,4)]))==abs(bb[c(2,4)]))
    wbb=c(2,4)[wbb]
    boundcirc = data.frame(x = seq(-180,180),y=rep(bb[wbb]+.1,length.out=length(seq(-180,180))))
    baseMap  = baseMap  + geom_path(aes(x = x, y = y), data = boundcirc)
  }
  baseMap  = baseMap  +   
    theme(panel.background = element_rect(fill="white"), axis.ticks = element_blank(), axis.text.y = element_blank(),axis.text.x = element_blank(),
                                axis.title.x = element_blank(), axis.title.y = element_blank(),
                                panel.border = element_blank())+
    coord_map(projection)
}else{
  stop(paste("Dont recognize map.type =",map.type))
}

return(baseMap )


}


#' @export
assignColors = function(colors){
  
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
