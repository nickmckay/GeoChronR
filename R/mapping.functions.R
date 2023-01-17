
#
#' TS overview plot
#'
#' @inheritParams binTs
#' @param sort.var What variable in the TS should the map and histogram be organized by? (default = "archiveType)
#' @param age.range a two element vector that includes the range over which you want to count (default = NA, which calculates the full range in the TS)
#' @param age.var What age variable should be used (default = "age)
#' @param step What step size should be used along the age axis? (default = NA, which will calculate 0.5\% of the age.range)
#' @inheritDotParams mapTs
#' @importFrom gridExtra grid.arrange
#' @return a gridExtra ggplot output
#' @export
#' @family plot
plotSummaryTs <- function(TS,sort.var = "archiveType", age.range = NA, age.var = "age", step = NA,... ){
  
  #check to see if TS is a tibble
  if(tibble::is_tibble(TS)){#convert back to TS
    TS <- lipdR::untidyTs(TS)
  }
  
  #make a map
  tsMap <- mapTs(TS,color = sort.var,...)
  
  #make a time availability plot
  ta <- plotTimeAvailabilityTs(TS,age.range = age.range, age.var = age.var, group.var = sort.var, step = step)+theme(legend.position = "none")
  
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

#' 
#' @title Create a Time availability plot
#' @param TS a lipd TS object. This should probably be filtered to show just records you're interested in, as all we counted
#' @param age.range a two element vector that includes the range over which you want to count (default = NA, which calculates the full range in the TS)
#' @param age.var What age variable should be used (default = "age")
#' @param group.var What variable Should be used to Group the counts? (default = "archiveType")
#' @param step What step size should be used along the age axis? (default = NA, which will calculate 0.5\% of the age.range)
#' @import ggplot2 tidyr
#' @importFrom purrr map_dbl map_dfc map_chr
#' @return a ggplot object
#' @export
plotTimeAvailabilityTs <- function(TS,
                                   age.range = NA,
                                   age.var = "age",
                                   group.var = "archiveType",
                                   step = NA){ 
  
  #check to see if TS is a tibble
  if(tibble::is_tibble(TS)){#convert back to TS
    TS <- lipdR::untidyTs(TS)
  }
  #estimate age range
  if(any(is.na(age.range))){
    age.range <- c(min(purrr::map_dbl(TS,function(x) min(x[[age.var]],na.rm = TRUE))),
                   max(purrr::map_dbl(TS,function(x) max(x[[age.var]],na.rm = TRUE))))
    
  }
  
  #estimate step
  if(all(is.na(step))){
    step <- round(.005 * abs(diff(age.range)))
  }
  
  gv <- pullTsVariable(TS,group.var)
  
  gv[is.na(gv)] <- "missing metadata"
  ugv <- sort(unique(gv))
  
  
  y1 <- min(age.range)
  y2 <- max(age.range)
  
  yvec <- seq(floor(y1),
              ceiling(y2),
              by = step)
  
  tvf <- function(ts,age.var,yvec){
    return(yvec >= min(ts[[age.var]],na.rm = T) & yvec <= max(ts[[age.var]],na.rm = T))
  }
  
  names(TS) <- paste0("col",seq_along(TS))#get rid of name warning.
  tfmat <- as.matrix(purrr::map_dfc(TS,tvf,age.var,yvec))
  
  hgroups <- length(ugv)
  
  if(hgroups == 0){
    stop("no variables in group.var")
  }
  
  countMat <- matrix(0,nrow = nrow(tfmat),ncol = hgroups)
  
  for(i in 1:hgroups){
    ind <- which(ugv[i]==gv)
    if(length(ind)==1){
      countMat[,i] <- as.numeric(tfmat[ ,ind])
    }else{
      countMat[,i] <- rowSums(tfmat[ ,ind])
    }
  }
  
  dcm <- data.frame(cbind(yvec,countMat))
  names(dcm) <- c("yvec",ugv)
  
  longCount <- tidyr::pivot_longer(dcm,-yvec,names_to = "group")
  
  densityPlot <- ggplot2::ggplot(longCount)+ggplot2::geom_area(ggplot2::aes(x = yvec,y = value, fill = group ))+
    ggplot2::labs(fill = group.var)  +
    ggplot2::ylab("count")+
    geoChronRPlotTheme()+
    ggtitle("Data availability")
  
  if(grepl("age",age.var,ignore.case = T)){
    densityPlot <- densityPlot+scale_x_reverse(age.var)
  }else{
    densityPlot <- densityPlot+scale_x_continuous(age.var)
  }
  
  return(densityPlot)
}

#' @export
#' @import ggplot2 dplyr
#' @family mapping
#' @title Map a TS object
#' @author Nick McKay
#' @description Create a stamen or line map of the location of a list of LiPD objects
#' @importFrom tibble is_tibble
#' @param TS A list of LiPD objects
#' @param color variable (in TS) by which to color, or a string specifying a static ggplot color
#' @param size Size of the location marker
#' @param lat.range Latitudes to use to create baseMap range (default = NA, which determines from TS)
#' @param lon.range Longitudes to use to create baseMap range (default = NA, which determines from TS)
#' @param shape variable (in TS) by which to adjust shape, or an integer specifying a static ggplot shape
#'
#' @inheritDotParams baseMap
#' @return ggmap object
#' @section Long-form example:
#' \href{http://nickmckay.github.io/GeoChronR/articles/TsFilteringAndMapping.html}{View a full-fledged example of how to use this function.}
mapTs <- function(TS,
                  color = "archiveType",
                  size=6,
                  shape= 16,
                  lat.range = NA,
                  lon.range = NA,
                  ...){
  
  
  #check to see if TS is a tibble
  if(tibble::is_tibble(TS)){#
    lat <- TS$geo_latitude
    lon <- TS$geo_longitude
    
    cvar <- TS[[color]]
    if(is.null(cvar)){
      man.color = TRUE
      cvar <- color
    }else{
      man.color = FALSE
    }
    
    shapevar <- TS[[shape]]
    if(is.null(shapevar) | is.numeric(shape)){
      man.shape = TRUE
      shapevar <- shape
    }else{
      man.shape = FALSE
    }
  
  }else{
  lat <- pullTsVariable(TS,"geo_latitude")
  lon <- pullTsVariable(TS,"geo_longitude")
  
  #get the color variable if possible
  cvar <- try(pullTsVariable(TS,color),silent = TRUE)
  if(class(cvar) == "try-error"){
    man.color = TRUE
    cvar <- color
  }else{
    man.color = FALSE
  }
  
  #get the color variable if possible
  shapevar <- try(pullTsVariable(TS,shape),silent = TRUE)
  if(class(shapevar) == "try-error"){
    man.shape = TRUE
    shapevar <- shape
  }else{
    man.shape = FALSE
  }
  
}
  
  dfp <- dplyr::distinct(data.frame(lon = lon,lat = lat, color = cvar, shape = shapevar))
  
  #omit NAs
  dfp <- dfp[!is.na(dfp$lat) & !is.na(dfp$lon),]
  
  #which coords to use for basemap?
  if(any(is.na(lon.range))){
    blon <- dfp$lon
  }else{
    blon <- lon.range
    dfp <- dplyr::filter(dfp,between(lon,min(blon),max(blon)))
    
  }
  if(any(is.na(lat.range))){
    blat <- dfp$lat
  }else{
    blat <- lat.range
    dfp <- dplyr::filter(dfp,between(lat,min(blat),max(blat)))
  }
  
  
  #create the baseMap  
  basemap <-  baseMap(lon = blon,lat = blat,...)
  
  #add the dots
  if(!man.color & man.shape){
    map <-  basemap  + 
      geom_point(data=dfp,aes(x=lon,y=lat, color = color),shape = shape,size = size) + 
      labs(color = color)  
  }else if(man.color & !man.shape){
    map <-  basemap  + 
      geom_point(data=dfp,aes(x=lon,y=lat, shape = shape),color = "black",fill = cvar,size = size) + 
      labs(shape = shape)  
  }else if(man.color & man.shape){
    map <-  basemap  + 
      geom_point(data=dfp,aes(x=lon,y=lat), shape = shapevar,color = "black",fill = cvar,size = size) 
  }else if(!man.color & !man.shape){
    map <-  basemap  + 
      geom_point(data=dfp,aes(x=lon,y=lat, shape = shape,color = color),size = size) + 
      labs(color = color,shape = shape) 
  }
  
  return(map)
}



#' @export
#' @import ggplot2
#' @family mapping
#' @title Map a list of LiPD objects
#' @author Nick McKay
#' @description Create a google or line map of the location of a list of LiPD objects
#'
#' @param D A list of LiPD objects
#' @param color Color of the location marker
#' @param size Size of the location marker
#' @param label.site Label the sites on the map (T/f)
#' @param ... 
#' @param shape Shape of the location marker
#' @inheritDotParams baseMap
#' @return ggmap object
mapLipd <- function(D,
                    color = NA,
                    size=8,
                    shape= 21,
                    label.site = TRUE,
                    ...){
  if(any(names(D) == "paleoData")){#single lipd file
    if(any(is.na(color))){
      color <- "red"
    }
    L <- D
    dfp <- data.frame(lon = L$geo$longitude,lat = L$geo$latitude)
    basemap <-  baseMap(dfp$lon,dfp$lat,...)
    map <-  basemap + geom_point(data=dfp,aes(x=lon,y=lat),colour = color,fill = color,size=size,shape = shape) 
    if(!is.null( L$geo$siteName) & label.site){
      dfp$sitename <-  L$geo$siteName
      map <- map+geom_label(data=dfp,aes(x=lon,y=lat,label=sitename),nudge_y=-5)
    }
    
  }else{ #multiple lipd datasets
    if(any(is.na(color))){
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
#' @importFrom maps map
#' @param lat latitude(s) range to map
#' @param lon longitude(s) range to map
#' @param map.type "stamen" or "line" 
#' @param f buffer for the map range
#' @param restrict.map.range TRUE or FALSE. Trim the size of the map to the points, for "line" map type
#' @param bound.circ Draw a boundary circle around a polar projection. TRUE or FALSE(default).
#' @param global Should the scope of the map be global? TRUE or FALSE(default).
#' @param projection Map project. All options on: ?mapproject
#' @param extend.range increase the span of the map by this much (lat/long degrees)
#' @inheritDotParams maps::map
#' @return ggmap base map 
baseMap = function(lon,
                   lat,
                   map.type="line",
                   f=0,
                   restrict.map.range=TRUE,
                   projection="mollweide",
                   bound.circ=FALSE,
                   global=FALSE,
                   extend.range=1,
                   ...){
  
  #if there's only one location, extend the range. 
  if(length(lat)==1 & length(lon)==1){
    extend.range <- extend.range*5
    #extend the range more
    lon = lon + c(-extend.range,extend.range)
    lat = lat + c(-extend.range,extend.range)
  }
  

  bbnew <- ggmap::make_bbox(lon,lat,f=f)
  
  #check to make sure no too big.
  bbmax <- c(-179,-89,179,89)
  bbdiff <- bbnew-bbmax
  bbdiff[3:4] <- bbmax[3:4]-bbnew[3:4]
  
  
  if(!global & any(bbdiff < 0)){
    global <- TRUE
    restrict.map.range <- FALSE
  #  bound.circ <- FALSE
    warning("Defined area too large for restrict.map.range = TRUE\n To force map restriction pass additional arguments to maps::map() in the function call. You probably want at least xlim, ylim, and wrap = TRUE")
  }
  
  
  #bound.circ and restrict.map.range can't both be true
  if(bound.circ & restrict.map.range){
    warning("bound.circ and restrict.map.range can't both be true - setting restrict.map.range to FALSE")
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
      suppressWarnings(try(load(file.path(tempdir(),"newmap.Rdata")),silent = TRUE))
      if(!exists("newmap")){
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
      x_lim = bb[c(1,3)] + c(-extend.range,extend.range)
      y_lim = bb[c(2,4)] + c(-extend.range,extend.range)
    }else if(global){
      x_lim = c(-190,190)
      y_lim = c(-91,91)
    }else if(bound.circ){
      x_lim = c(-190,190)
      y_lim = bb[c(2,4)] + c(-extend.range,extend.range)
    }
    
    
    x_cell_lim <- x_lim + c(1, -1) * res/2
    y_cell_lim <- y_lim + c(1, -1) * res/2
    
    
    
    if(global){
      dum = maps::map(plot = FALSE,...)
      x_lim = c(-190,190)
      y_lim = c(-91,91)
      
    }else{
      dum = try(maps::map(xlim = x_lim, ylim = y_lim, plot = FALSE,wrap=TRUE,...),silent = TRUE)
      if(class(dum) == "try-error"){
        stop("It looks like the region defined by your coordinates might be too small to draw with a line map.\n You should either specify a larger region with `extend.range` or change the map type.")
      }
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
      baseMap  = baseMap  +  geom_rect(aes(xmax=x_lim[2]-.1,xmin=x_lim[1]+.1,ymax=y_lim[2]-.1,ymin=y_lim[1]+.1),fill=NA, colour="black",data=bbdf)
      
    }else if(bound.circ){ #this is only for polar projections
      wbb=which(min(abs(y_lim))==abs(y_lim))
      my <- y_lim[wbb]
      bound.circ = data.frame(x = seq(-180,180),
                              y=rep(my+.1,length.out=length(seq(-180,180))))
      baseMap  = baseMap  + geom_path(aes(x = x, y = y), data = bound.circ)
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
