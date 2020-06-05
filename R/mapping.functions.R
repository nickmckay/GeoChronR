
#
#' TS overview plot
#'
#' @param TS 
#' @param sortVar What variable in the TS should the map and histogram be organized by? (default = "archiveType)
#' @param ageRange a two element vector that includes the range over which you want to count (default = NA, which calculates the full range in the TS)
#' @param ageVar What age variable should be used (default = "age)
#' @param step What step size should be used along the age axis? (default = NA, which will calculate 0.5% of the ageRange)
#' @param ... variables to pass to mapTs()
#' @inheritParams mapTs
#' @import gridExtra
#' 
#' @return a gridExtra ggplot output
#' @export
summaryPlotTs <- function(TS,sortVar = "archiveType", ageRange = NA, ageVar = "age", step = NA,... ){


#make a map
tsMap <- mapTs(TS,color = sortVar,...)

#make a time availability plot
ta <- plotTimeAvailabilityTs(TS,ageRange = ageRange, ageVar = ageVar, groupVar = sortVar, step = step)+theme(legend.position = "none")

#define the layout
lay <- rbind(c(1,1,1),
             c(1,1,1),
             c(1,1,1),
             c(2,2,2),
             c(2,2,2))

out <- gridExtra::grid.arrange(grobs = list(tsMap,ta),
                        layout_matrix=lay)  

return(out)
}

#' Create a Time availability plot. 
#'
#' @param TS a lipd TS object. This should probably be filtered to show just records you're interested in, as all we counted
#' @param ageRange a two element vector that includes the range over which you want to count (default = NA, which calculates the full range in the TS)
#' @param ageVar What age variable should be used (default = "age)
#' @param groupVar What variable Should be used to Group the counts? (default = "archiveType)
#' @param step What step size should be used along the age axis? (default = NA, which will calculate 0.5% of the ageRange)
#' @import ggplot2 tidyr lipdR purrr
#' @return a ggplot object
#' @export
plotTimeAvailabilityTs <- function(TS,
                                 ageRange = NA,
                                 ageVar = "age",
                                 groupVar = "archiveType",
                                 step = NA){ 
  
  
  #estimate age range
if(is.na(ageRange)){
  ageRange <- c(min(purrr::map_dbl(TS,function(x) min(x[[ageVar]],na.rm = TRUE))),
                max(purrr::map_dbl(TS,function(x) max(x[[ageVar]],na.rm = TRUE))))
  
}

#estimate step
if(is.na(step)){
step <- round(.005 * abs(diff(ageRange)))
}


gv <- lipdR::pullTsVariable(TS,groupVar)
gv[is.na(gv)] <- "missing metadata"

ugv <- sort(unique(gv))


y1 <- min(ageRange)
y2 <- max(ageRange)

yvec <- seq(floor(y1),
            ceiling(y2),
            by = step)

tvf <- function(ts,ageVar,yvec){
  return(yvec >= min(ts[[ageVar]],na.rm = T) & yvec <= max(ts[[ageVar]],na.rm = T))
}

tfmat <- as.matrix(purrr::map_dfc(TS,tvf,ageVar,yvec))

hgroups <- length(ugv)

if(hgroups == 0){
  stop("no variables in groupVar")
}

countMat <- matrix(0,nrow = nrow(tfmat),ncol = hgroups)

for(i in 1:hgroups){
  countMat[,i] <- rowSums(tfmat[ ,ugv[i]==gv])
}

dcm <- data.frame(cbind(yvec,countMat))
names(dcm) <- c("yvec",ugv)

longCount <- tidyr::pivot_longer(dcm,-yvec,names_to = "group")

densityPlot <- ggplot2::ggplot(longCount)+ggplot2::geom_area(ggplot2::aes(x = yvec,y = value, fill = group ))+
  ggplot2::labs(fill = groupVar)  +
  ggplot2::ylab("count")+
  geoChronRPlotTheme()+
  ggtitle("Data availability")

if(grepl("age",ageVar,ignore.case = T)){
  densityPlot <- densityPlot+scale_x_reverse(ageVar)
}else{
  densityPlot <- densityPlot+scale_x_continuous(ageVar)
}

return(densityPlot)
}

#' @export
#' @import ggplot2
#' @family mapping
#' @title Map a TS object
#' @author Nick McKay
#' @importFrom lipdR pullTsVariable
#' @description Create a stamen or line map of the location of a list of LiPD objects
#' @param TS A list of LiPD objects
#' @param color variable by which to color
#' @param size Size of the location marker
#' @param shape Shape of the location marker
#' @inheritParams baseMap
#' @return ggmap object
mapTs <- function(TS,
                    color = "archiveType",
                    size=6,
                    shape= 21,
                    ...){
  
  
  lat <- pullTsVariable(TS,"geo_latitude")
  lon <- pullTsVariable(TS,"geo_longitude")
  
  #get the color variable if possible
  cvar <- try(pullTsVariable(TS,color))
  if(class(cvar) == "try-error"){
    cvar <- color
  }
  
  dfp <- data.frame(lon = lon,lat = lat, color = cvar)
  
  #omit NAs
  dfp <- dfp[!is.na(dfp$lat) & !is.na(dfp$lon),]

  #create the baseMap  
  basemap <-  baseMap(dfp$lon,dfp$lat,...)
  
  #add the dots
  map <-  basemap  + 
    geom_point(data=dfp,aes(x=lon,y=lat, fill = color),color = "black",shape = shape,size=size) + 
labs(fill = color)  

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
#' @inheritParams baseMap
#' @return ggmap object
mapLipd <- function(D,
                    color = NA,
                    size=8,
                    shape= 21,
                    labelSite = TRUE,
                    ...){
  if(any(names(D) == "paleoData")){#single lipd file
    if(is.na(color)){
      color <- "red"
    }
    L <- D
    dfp <- data.frame(lon = L$geo$longitude,lat = L$geo$latitude)
    basemap <-  baseMap(dfp$lon,dfp$lat,...)
    map <-  basemap + geom_point(data=dfp,aes(x=lon,y=lat),colour = color,fill = color,size=size,shape = shape) 
    if(!is.null( L$geo$siteName) & labelSite){
      dfp$sitename <-  L$geo$siteName
      map <- map+geom_label(data=dfp,aes(x=lon,y=lat,label=sitename),nudge_y=-1)
    }
    
  }else{ #multiple lipd datasets
    if(is.na(color)){
      af <- function(L){
        if("archiveType" %in% names(L)){
          return(as.character(L$archiveType))
        }else{
          return("missing")
        }
      }
      color <- purrr::map_chr(D,af)
      cname <- "archiveType"
    }else{
      cname <- ""
    }
    
    
    #create a data frame of coordinates, colors and shape
    dfp <-  data.frame(lon = sapply(D,function(x) x$geo$longitude),lat = sapply(D,function(x) x$geo$latitude),color,shape)
    
    #omit NAs
    dfp <- dfp[!is.na(dfp$lat) & !is.na(dfp$lon),]
    
    #create the baseMap
    basemap <- baseMap(dfp$lon,dfp$lat,...)
    map <-  basemap  + geom_point(data=dfp,aes(x=lon,y=lat,fill = color),shape = shape,size=size) + labs(fill = cname)  
    
  }
  return(map)
}

#' @family mapping
#' @export
#' @title Make a base map
#' @author Nick McKay
#' @description Create a stamen or line map of the location of a list of LiPD objects
#' @import ggmap
#' @import ggplot2
#' @import mapproj
#' @import maps
#' @param lat latitude(s) range to map
#' @param lon longitude(s) range to map
#' @param map.type "stamen" or "line" 
#' @param f buffer for the map range
#' @param restrict.map.range TRUE or FALSE. Trim the size of the map to the points, for "line" map type
#' @param boundcirc Draw a boundary circle around a polar projection. TRUE or FALSE(default).
#' @param global Should the scope of the map be global? TRUE or FALSE(default).
#' @param projection Map project. All options on: ?mapproject
#' @param extend.range increase the span of the map by this much (lat/long degrees)
#' @return ggmap base map 
baseMap = function(lon,
                   lat,
                   map.type="line",
                   f=.1,
                   restrict.map.range=TRUE,
                   projection="mollweide",
                   boundcirc=FALSE,
                   global=FALSE,
                   extend.range=10){
  
  #if there's only one location, extend the range. 
  if(length(lat)==1 & length(lon)==1){
    lon = lon + c(-extend.range,extend.range)
    lat = lat + c(-extend.range,extend.range)
  }
  
  #boundcirc and restrict.map.range can't both be true
  if(boundcirc & restrict.map.range){
    warning("boundcirc and restrict.map.range can't both be true - setting restrict.map.range to FALSE")
    restrict.map.range=FALSE
  }
  
  if(global & restrict.map.range){
    warning("global and restrict.map.range can't both be true - setting restrict.map.range to FALSE")
    restrict.map.range=FALSE
  }
  
  if(map.type == "google"){
    warning("google maps are no longer supported, using `stamen` instead")
    map.type <- "stamen"
    
  }
  
  if(map.type=="stamen"){
    try(load(file.path(tempdir(),"bound.box.Rdata")),silent = TRUE)
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
    if(bb[4] > 90){
      bb[4]=90
    }
    save(bb,file = file.path(tempdir(),"bound.box.Rdata"))
  }
  
  if(map.type=="stamen"){
    #if global, set bbnew to global
    if(global){
      bb <- c(-180,-70,180,70)
      names(bb) <- c("left","bottom","right","top")
      lnp <- FALSE
      warning("Stamen globally doesn't work well at high latitudes. Truncating at 70.")
    }
    if(lnp){
      try(load(file.path(tempdir(),"newmap.Rdata")),silent = TRUE)
      if(!exists(newmap)){
        newmap <- get_map(location=bb,maptype="terrain",source="stamen")
        save(newmap,file=file.path(tempdir(),"newmap.Rdata"))
        
      }
    }else{
      newmap <- get_map(location=bb,maptype="terrain",source="stamen")
      save(newmap,file=file.path(tempdir(),"newmap.Rdata"))
      
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
      y_lim = c(-91,91)
    }
    
    x_cell_lim <- x_lim + c(1, -1) * res/2
    y_cell_lim <- y_lim + c(1, -1) * res/2
    
    
    
    if(global){
      dum = maps::map(plot = FALSE)
      x_lim = c(-190,190)
      y_lim = c(-91,91)
      
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
    
    }else if(boundcirc){ #this is only for polar projections
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
      baseMap = baseMap+ 
        coord_map(projection,xlim = c(-180,180))+
        geom_rect(aes(xmax=180.1,xmin=-180.1,ymax=90.1,ymin=-90.1),fill=NA, colour="black")
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
