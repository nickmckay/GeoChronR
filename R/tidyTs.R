# #create tidy data.frame from TS
# library(tidyr)
# library(dplyr)
# library(tibble)
# library(purrr)
# 
# 
# #to do = handle ensembles!
# 
# for(i in 1:length(TS)){
#   
#   
#   ti <- TS[[i]]
# 
#   #find which entries are vectors. Year and value should be. There could be more.
#   al <- sapply(ti,length)
#   
#   #going to assume that we only want the longest ones here
#   long <- which(al==max(al))
#   
#   if(!any(names(long)=="paleoData_values")){
#     stop(paste0(as.character(i),": paleoData_values didn't show up as being the longest vector"))
#   }
#   
#   if(!(any(names(long)=="year") | any(names(long)=="age") | any(names(long)=="depth") )){
#     stop(paste0(as.character(i),": There must be an 'age', 'year', or 'depth' column that's the same length as paleoData_values"))
#   }
#   
#   sdf <- as.tibble(ti[long])
#   
#   #handle ts variables that are longer than 1, but not the full length by concatenating 
#   
#   med <- ti[which(al<max(al) & al>1)]
#   collapsed <- sapply(med, paste,collapse = ", ")
#   ti[which(al<max(al) & al>1)] <- collapsed
#   
#   #check length again
#   al2 <- sapply(ti,length)
#   
#   #replicate the metadata to each observation row
#   short <- which(al2==1)
#   mdf <- as.data.frame(ti[short])
#   meta.df <- map_df(seq_len(nrow(sdf)), ~mdf)
#   
#   #combine them together
#   tdf <- bind_cols(sdf,meta.df)
#   if(i == 1){
#     tidyData <- tdf
#   }else{
#     tidyData <- bind_rows(tidyData,tdf)
#   }
# }
# 
# 

# 
# library(ggplot2)
# library(ggridges)
# #test plotting code
# 
# scaleHeight <- .75
# labBuff = 0.02
# labSize = 3
# labSpace = 2
# xlabName <- "Year (AD)"
# colorVar <- "paleoData_variableName"
# #colorFun <- colorRampPalette(brewer.pal(nColors,"Dark2"))
# colorFun <- function(x){rep("black",x)}
# 
# #tidy the TS structure
# tidyData <- tidyTs(arcTS)
# 
# #filter for plotting
# plot.df <- filter(tidyData,between(year,1600,2000)) %>% #only years from 1700 to 2000
#   filter(interpretation1_variable == "T") %>% #only variables sensitive temperature
#   filter(between(geo_longitude,-90,90)) %>%  #only in the Atlantic Arctic
#   group_by(paleoData_TSid) %>% #grou by column
#    arrange(geo_longitude) #and sort by longitude
# 
# #plot a stack of all timeseries, coloring them by the variableName
# plotTimeseriesStack(plot.df,colorVar = "paleoData_variableName")
# 
# plotTimeseriesStack <- function(plot.df,timeVar = "year", colorVar = "paleoData_TSid", fillAlpha = 0.2,scaleFactor = 1/3,scaleHeight = .75, labBuff = 0.02, labSize = 3,  labSpace= 2,colorFun = grDevices::colorRampPalette( RColorBrewer::brewer.pal(nColors,"Dark2"))){
# 
#   #start with some error checking...
#   if(is.character(colorFun)){#then just assign all colors to that string
#     colorFun <- function(x){rep(colorFun,x)}
#   }
# 
#   #check the plot.df for required variables
#   reqVar <- c("paleoData_values","paleoData_TSid","paleoData_units","paleoData_variableName","dataSetName", "archiveType",timeVar)
# 
#   for(r in 1:length(reqVar)){
#     if(!any(reqVar[r] == names(plot.df))){
#       stop(paste(reqVar[r],"must be in plot.df"))
#     }
#   }
#   plot.df <- plot.df %>%
#   mutate(scaled = scale(paleoData_values)*scaleFactor) %>%
#     filter(is.finite(scaled))
# 
#   #arrange the data.frame by TSid factors
#   plot.df$paleoData_TSid <- factor(plot.df$paleoData_TSid,levels = unique(plot.df$paleoData_TSid))
# 
# 
#   #copy the color variable into plot.df
# plot.df$cv = plot.df[[colorVar]]
# 
# plot.df$cv <- factor(plot.df$cv,levels = unique(plot.df$cv))
# 
# axisStats <- plot.df %>%
#   summarize(variableName = unique(paleoData_variableName),
#             units = unique(paleoData_units),
#             dataSetName = unique(dataSetName),
#             archiveType = unique(archiveType),
#             mean = mean(paleoData_values,na.rm = T),
#             sdhigh = sd(paleoData_values,na.rm = T)/scaleFactor*scaleHeight+mean(paleoData_values,na.rm = T),
#             sdlow = -sd(paleoData_values,na.rm = T)/scaleFactor*scaleHeight+mean(paleoData_values,na.rm = T),
#             colorVar = unique(cv)) %>%
#   mutate(axisLabel = paste0(variableName," (",units,")")) %>%
#   mutate(axisMin = as.character(signif(sdlow,3))) %>%
#   mutate(axisMax = as.character(signif(sdhigh,3)))
# 
# colOrder <- match(unique(plot.df$paleoData_TSid),axisStats$paleoData_TSid)
# 
# axisStats <- axisStats[colOrder,]
# 
# nlines <- length(unique(plot.df$paleoData_TSid))
# 
# nColors <- min(length(levels(axisStats$colorVar)),nlines)
# 
# colVec <- colorFun(nColors)
# 
# axisStats$colors <- colVec[match(axisStats$colorVar,levels(axisStats$colorVar))]
# 
# spag <- ggplot(plot.df, aes(x = year, height = scaled, y = paleoData_TSid,color = cv, fill = cv)) +
#   geom_ridgeline(min_height = -Inf,alpha = fillAlpha)+
#   scale_color_manual(name = colorVar,values = colVec)+
#   scale_fill_manual(name = colorVar,values = colVec)+
#   theme_ridges(grid = TRUE)+
#   theme_bw()
# 
# 
# ylow <- seq_len(nlines)-scaleHeight
# yhigh <-  seq_len(nlines)+scaleHeight
# 
# 
# my.xrange <- ggplot_build(spag)$layout$panel_scales$x[[1]]$range$range
# 
# xpos <- rep(my.xrange,times = ceiling(nlines/2))[seq_len(nlines)]
# 
# #guess position for label
# xrtick <- my.xrange+c(-1 ,1)*abs(diff(my.xrange))*labBuff*.25
# xposTick <- rep(xrtick,times = ceiling(nlines/2))[seq_len(nlines)]
# 
# xrtickLabel <- my.xrange+c(-1 ,1)*abs(diff(my.xrange))*labBuff
# xposTickLabel <- rep(xrtickLabel,times = ceiling(nlines/2))[seq_len(nlines)]
# 
# xrlab <- my.xrange+c(-1 ,1)*abs(diff(my.xrange))*labBuff*labSpace
# xposLab <- rep(xrlab,times = ceiling(nlines/2))[seq_len(nlines)]
# 
# spag <- spag+annotate(geom = "segment", colour = axisStats$colors , x = xpos, xend = xpos, y = ylow, yend  = yhigh)+
#   annotate(geom = "segment", colour = axisStats$colors , x = xpos, xend = xposTick, y = ylow, yend  = ylow)+
#   annotate(geom = "segment", colour = axisStats$colors , x = xpos, xend = xposTick, y = yhigh, yend  = yhigh)+
#   annotate(geom = "text", colour = axisStats$colors , x = xposTickLabel, y = ylow, label = axisStats$axisMin,size = labSize)+
#   annotate(geom = "text", colour = axisStats$colors , x = xposTickLabel, y = yhigh, label = axisStats$axisMax,size = labSize)+
#   annotate(geom = "text", colour = axisStats$colors , x = xposLab, y = seq_len(nlines), label = axisStats$axisLabel,size = labSize,angle = 90)+
#   scale_y_discrete(name = NULL,labels = axisStats$dataSetName, expand = c(0.02,-0.75))+
#   scale_x_continuous(name = xlabName, expand = c(0.02,0))
# 
# return(spag)
# }
#  annotate(geom = "rect", colour = "yellow", fill = "yellow", xmin = 1910, xmax = 1930, ymin = 0, ymax = nlines+1,alpha = 0.2)

