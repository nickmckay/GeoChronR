#' @export
#' @import ggplot2
#' @family mapping
#' @title Map a LiPD object
#' @author Nick McKay
#' @description Create a google or line map of the location of the LiPD object
#' @param L Single LiPD object
#' @param color Color of the location marker
#' @param size Size of the location marker
#' @param shape Shape of the location marker
#' @param map.type "google" or "line" 
#' @param extend.range increase the span of the map by this much (lat/long degrees)
#' @return ggmap object
mapLipd = function(L,color="red",size=8,shape = 16,map.type="google",extend.range=10){
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
#' @import ggplot2
#' @family mapping
#' @title Map a list of LiPD objects
#' @author Nick McKay
#' @description Create a google or line map of the location of a list of LiPD objects
#' @param D A list of LiPD objects
#' @param color Color of the location marker
#' @param size Size of the location marker
#' @param shape Shape of the location marker
#' @param map.type "google" or "line" 
#' @param f buffer for the map range
#' @param restrict.map.range TRUE or FALSE. Trim the size of the map to the points, for "line" map type
#' @param boundcirc Draw a boundary circle around a polar projection. TRUE or FALSE(default).
#' @param global Should the scope of the map be global? TRUE or FALSE(default).
#' @param shape.by.archive TRUE or FALSE. Use archiveType to assign shapes.
#' @param projection Map project. All options on: ?mapproject
#' @return ggmap object
mapLipds = function(D,shape= 21,size=8,color = sapply(D,"[[","archiveType") ,map.type="google",f=.3,restrict.map.range=TRUE,boundcirc=FALSE,global=FALSE,projection = "mercator"){
  dfp = data.frame(lon = sapply(D,function(x) x$geo$longitude),lat = sapply(D,function(x) x$geo$latitude),color,shape)
  dfp = dfp[!is.na(dfp$lat) & !is.na(dfp$lon),]
  basemap = baseMap(dfp$lon,dfp$lat,map.type = map.type,f=f,restrict.map.range = restrict.map.range,boundcirc = boundcirc,global=global,projection = projection )
  map = basemap  + geom_point(data=dfp,aes(x=lon,y=lat,fill = color),shape = shape,size=7)
  return(map)
}

#' @family mapping
#' @export
#' @title Make a base map
#' @author Nick McKay
#' @description Create a google or line map of the location of a list of LiPD objects
#' @import ggmap
#' @import ggplot2
#' @import mapproj
#' @import maps
#' @param lat latitude(s) range to map
#' @param lon longitude(s) range to map
#' @param map.type "google" or "line" 
#' @param f buffer for the map range
#' @param restrict.map.range TRUE or FALSE. Trim the size of the map to the points, for "line" map type
#' @param boundcirc Draw a boundary circle around a polar projection. TRUE or FALSE(default).
#' @param global Should the scope of the map be global? TRUE or FALSE(default).
#' @param projection Map project. All options on: ?mapproject
#' @param extend.range increase the span of the map by this much (lat/long degrees)
#' @return ggmap base map 
baseMap = function(lon,lat,map.type="google",f=.1,restrict.map.range=TRUE,projection="mercator",boundcirc=FALSE,global=FALSE,extend.range=10){
  if(length(lat)==1 & length(lon)==1){
    lon = lon + c(-extend.range,extend.range)
    lat = lat + c(-extend.range,extend.range)
  }
  
  #boundcirc and restrict.map.range can't both be true
  if(boundcirc & restrict.map.range){
    warning("boundcirc and restrict.map.range can't both be true - setting restrict.map.range to FALSE")
    restrict.map.range=FALSE
  }
  
  if(map.type=="google"){
    try(load("bound.box.Rdata"),silent = TRUE)
    if(!exists("bb")){bb=1000}
  }else{
    bb=1000
  }
  
  bbnew <- ggmap::make_bbox(lon,lat,f=f)
  if(all(bbnew==bb)){
    lnp=TRUE
    #load the file you already downloaded
  }else{
    lnp=FALSE
    bb=bbnew
    # if(map.type=="google"){
    # if(bb[4] > 89){
    #   diff <- bb[4]-89
    #   bb[2] <- bb[2]-(diff*2)
    #   bb[4] <- bb[4]-(diff*2)
    # }
    if(bb[4] > 90){
      bb[4]=90
    }
    #save(bb,file="bound.box.Rdata")
  }
  
  if(map.type=="google"){
  #if(lnp){
  #  newmap = try(load("newmap.Rdata"),silent = TRUE)
    #  if(grepl(class(newmap),pattern="error")){newmap <- get_map(location=bb,maptype="terrain",source="google")}
    #}else{
    newmap <- get_map(location=bb,maptype="terrain",source="stamen")
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
      dum = maps::map(plot = FALSE)
    }else{
      dum = maps::map(xlim = x_lim, ylim = y_lim, plot = FALSE,wrap=TRUE)
    }
    
    #dum = map(xlim = x_cell_lim, ylim = y_cell_lim, plot = FALSE)
    ant_ggplot = data.frame(dum[c("x","y")])
    if(min(x_lim)<=-180 & max(x_lim)>=180){#don't restrict lon
      badLines = which(ant_ggplot$y <= min(y_lim) | ant_ggplot$y >= max(y_lim))
      
    }else{
    badLines = which(ant_ggplot$x <= min(x_lim) | ant_ggplot$x >= max(x_lim) | ant_ggplot$y <= min(y_lim) | ant_ggplot$y >= max(y_lim))
    }
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
            panel.border = element_blank())
    
    if(global){
      baseMap = baseMap+ coord_map(projection,xlim = c(-180,180))
    }else{
      baseMap = baseMap+ coord_map(projection)
    }
  }else{
    stop(paste("Dont recognize map.type =",map.type))
  }
  
  return(baseMap )
  
  
}

#' @family mapping
#' @export
#' @title Assign colors for map color scale
#' @author Nick McKay
#' @description Quick look up for color scale pairs for mapping
#' @param colors string to define color scale. Options are "temp", "precip" and "drought"
#' @return a pair of high/low colors
assignColors = function(colors="temp"){
  
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
