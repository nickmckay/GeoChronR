# Chronology plots: age models, ensembles, distributions, and dataset summaries.


#' @export
#' @family plot
#' @author Nick McKay
#' @title Plot a summary figure
#' @description shows a map, timeseries, and age model diagram, and basic simple metadata
#' @import ggplot2
#' @import grid
#' @importFrom gridExtra grid.arrange
#' @inheritDotParams plotChronEns
#' @inheritParams selectData
#' @param paleo.age.var variableName to use for x axis of the paleo plot ("age" by default)
#' @param paleo.data.var variableName to use for the y axis of the paleo plot (NA by default, which lets you choose)
#' @param chron.number chron.numData object to use (NA by default, will ask if needed)
#' @param paleo.meas.num paleo.num measurement table to use (NA by default, will ask if needed)
#' @param chron.meas.num chron.num measurement table to use (NA by default, will ask if needed)
#' @param chron.depth.var variableName to use for chron depth ("depth" by default)
#' @param chron.age.var variableName to use for chron calibrated age ("age" by default)
#' @param chron.age.14c.var variableName to use for chron 14C age ("age" by default)
#' @param dot.size what size dot for the chron plot? Only used if not plotting by plotChronEns() (default = 5)
#' @param summary.font.size Font size for the summary
#' @param text.width Width of the text panel
#' @param legend.position location of the legend
#' @return A gridArrange of ggplot grobs
#' @examples 
#' \dontrun{
#' myPlot = summaryPlot(L)
#' }
plotSummary = function(L,
                       paleo.age.var = "age",
                       paleo.data.var = NA,
                       chron.number = NA, 
                       paleo.meas.num = NA, 
                       chron.meas.num = NA, 
                       chron.depth.var = "depth", 
                       chron.age.var = "age", 
                       chron.age.14c.var = "age14C",
                       dot.size = 5, 
                       summary.font.size = 6, 
                       text.width = 400/summary.font.size, 
                       legend.position = c(0.7,0.3),
                       ...){
  
  #is this a LiPD file?
  if(is.list(L)){
    if(is.null(L$dataSetName)){
      stop("This is either not a single LiPD object, or it has no dataSetName. plotSummary requires a single LiPD object as input")
    }
  }else{
    stop("plotSummary requires a single LiPD object as input")
  }
  
  map <- mapLipd(L,extend.range = 4,map.type = "line")
  
  #plot paleoData
  
  if(any(is.na(paleo.age.var))){
    print("What should we plot on the X-axis?")
    print("We'll look for age or year...")
    age=selectData(L,var.name = "age",alt.names = "year",meas.table.num = paleo.meas.num)
  }else{
    age=selectData(L,var.name = paleo.age.var, meas.table.num = paleo.meas.num)
  }
  
  if(any(is.na(paleo.data.var))){
    print("What should we plot on the Y-axis?")
    variable=selectData(L,meas.table.num = paleo.meas.num)
  }else{
    variable=selectData(L,var.name = paleo.data.var,meas.table.num = paleo.meas.num)
  }
  
  paleoPlot = plotLine(X = age,Y = variable)
  paleoPlot = paleoPlot + ggtitle(paste("PaleoData:",variable$variableName))
  
  #do chron.
  chronPlot <- plotChron(L,chron.number = chron.number, meas.num = chron.meas.num, depth.var = chron.depth.var, age.var = chron.age.var,age.14c.var = chron.age.14c.var, dot.size = dot.size,legend.position = legend.position, ...)
  
  if(!is.list(chronPlot)){
    if(any(is.na(chronPlot))){
      chronPlot = grid::grobTree(grid::rectGrob(gp = grid::gpar(fill = 1,alpha=.1)),grid::textGrob("No chronData"))
    }
  }
  
  
  
  lay = rbind(c(1,1,2,2),
              c(3,3,2,2),
              c(3,3,4,4),
              c(3,3,4,4))
  
  
  if(!is.null(L$pub[[1]]$citation)){
    citation <- L$pub[[1]]$citation
  }else{
    authors <- L$pub[[1]]$author
    if(is.list(authors)){
      authors <-  unlist(authors)
    }
    year <- L$pub[[1]]$year
    if(is.null(year)){
      year <-  L$pub[[1]]$pubYear
    }
    title <- L$pub[[1]]$title
    journal <- L$pub[[1]]$journal
    volume <- L$pub[[1]]$volume
    pages <- L$pub[[1]]$pages
    
    citation <- paste0(authors," (",as.character(year),"). ",title,". ",journal, " ", as.character(volume),", ",pages,".")
  }
  citation <- paste(strwrap( citation, width = text.width, simplify = FALSE)[[1]],collapse = "\n          ")
  dataSetText = paste("DataSetName:",L$dataSetName,"\nArchive Type: ",L$archiveType,"\nCitation:",citation)
  summaryText = grid::grobTree(grid::rectGrob(gp = grid::gpar(fill = 1,alpha=.1)), grid::textGrob(x = unit(0.03, "npc"), y = unit(0.5, "npc"),dataSetText,just = "left",check.overlap = FALSE,gp = grid::gpar(fontfamily = "mono",fontsize = summary.font.size)))
  
  summary = gridExtra::grid.arrange(grobs = list(summaryText,paleoPlot,map,chronPlot),layout_matrix=lay)    
  return(summary)
  
}

#' @export
#' @family plot
#' @family chron
#' @author Nick McKay
#' @title Plot probability distributions
#' @description Plot or add probability distributions from a paleo or chron model to a plot. 
#' @import ggplot2
#' @inheritParams selectData
#' @param dist.var Name of the distribution variable, will be plotted along the x-axis. Use coord_flip() after running the function if you want vertical distributions. "age" by default. 
#' @param y.var Name of the y-axis variable. "depth" by default. 
#' @param mode chron or paleo 
#' @param paleo.or.chron.num number of the chron or paleo Data object
#' @param model.num number of the model object
#' @param color distribution color (following ggplot rules)
#' @param dist.plot vector of distribution tables to plot
#' @param dist.type "violin" (default), "high" for one-sided distributions towards higher depths, "low" for one-sided distributions towards lower depths
#' @param thick thickness of the line around the distribution
#' @param truncate.dist truncate probability density values below this number. NA (default) means no truncation
#' @param scale.frac controls the vertical span of the probability distribution. Approximately the vertical fraction of the plot that the distribution will cover. 
#' @param add.to.plot A ggplot object to add this plot to. Default is ggplot()
#' @param alp transparency, from 0 to 1
#' @return A ggplot object
plotModelDistributions = function(L,
                                  dist.var = "age",
                                  y.var = "depth",
                                  mode = "chron",
                                  paleo.or.chron.num = 1, 
                                  model.num = 1, 
                                  add.to.plot = ggplot(), 
                                  alp=.5,
                                  color = "purple",
                                  scale.frac = 0.02,
                                  dist.plot = NA,
                                  dist.type = "violin",
                                  thick = 0.1,
                                  truncate.dist = NA){
  
  
  P = L[[paste0(mode,"Data")]]
  if(any(is.na(paleo.or.chron.num))){
    if(length(P)==1){
      paleo.or.chron.num=1
    }else{
      print(names(P))
      paleo.or.chron.num=as.integer(readline(prompt = "Which paleoData do you want to put this age ensemble in? Select a number "))
    }
  }
  
  #initialize model number
  MT = P[[paleo.or.chron.num]]$model
  if(is.null(MT)){
    stop(paste0("There are no models in ",mode,"Data[[",as.character(paleo.or.chron.num),"]]. This makes it difficult to plot distributions from the model"))
  }
  
  if(any(is.na(model.num))){
    if(length(MT)==1){
      #only one pmt
      meas.table.num=1
    }else{
      print(paste0(paleo.or.chron,"Data[[", as.character(paleo.num), "]] has ", length(MT), " models"))
      meas.table.num=as.integer(readline(prompt = "Which measurement table do you want to put the ensemble in? Enter an integer "))
    }
  }
  
  
  #pull out distribution object
  dist = MT[[model.num]]$distributionTable
  
  #check it to make sure it's a distribution table
  if(!is.list(dist)){
    stop("This doesn't seem to be a valid distribution table with these settings")
  }
  
  #if not specified, plot all distributions
  if(any(is.na(dist.plot))){
    dist.plot = 1:length(dist)
  }
  
  #pull out all the yaxis data to get range and scale
  ally = sapply(dist[dist.plot],"[[",y.var)
  
  # get range and scale
  plot.range =range(ally,na.rm = T)
  
  #guess at the scaler...
  this.dist = dist[[dist.plot[[1]]]]
  if(!any(is.na(truncate.dist))){
    tgood = which(this.dist$probabilityDensity$values > truncate.dist)
    this.dist$probabilityDensity$values = this.dist$probabilityDensity$values[tgood]
    this.dist$age$values = this.dist$age$values[tgood]
  }
  
  
  #loop through individual ages...
  for(y in dist.plot){
    this.dist = dist[[y]]
    if(!is.na(truncate.dist)){
      tgood = which(this.dist$probabilityDensity$values > truncate.dist)
      this.dist$probabilityDensity$values = this.dist$probabilityDensity$values[tgood]
      this.dist$age$values = this.dist$age$values[tgood]
    }
    pd = this.dist$probabilityDensity$values/sum(this.dist$probabilityDensity$values,na.rm=T)
    scaler = scale.frac*abs(diff(plot.range))/max(pd)
    pd = pd * scaler
    this.df = data.frame(x= this.dist[[dist.var]]$values,ymin = this.dist[[y.var]] - pd,ymax = this.dist[[y.var]] + pd )
    if(dist.type == "up" | dist.type == "high"){this.df$ymin =  this.dist[[y.var]]}
    if(dist.type == "down" | dist.type == "low"){this.df$ymax =  this.dist[[y.var]]}
    add.to.plot = add.to.plot + geom_ribbon(data = this.df, aes(x = x,ymin = ymin,ymax = ymax),color = color,fill = color, alpha = alp,linewidth = thick)
  }
  add.to.plot = add.to.plot + geoChronRPlotTheme()
  return(add.to.plot)
}


#' @export
#' @family plot
#' @family chron
#' @author Nick McKay
#' @title Compare chron ensemble with paleoData age-model
#' @description Plots the difference of an chron ensembleTable with the paleoData age
#' @import ggplot2
#' @inheritParams selectData
#' @param ageEnsVar name of the age ensemble variable in the chronData to search for
#' @param age.var name of the age variable in the paleoData to search for
#' @param depth.var name of the depth variable to search for
#' @param paleo.num an integer that corresponds to paleo.numData object (L$paleoData[[?]]) has the measurementTable you want to modify
#' @param paleo.meas.table.num an integer that corresponds to paleo.num measurementTable you want to add the ensemble to?
#' @param chron.num  an integer that corresponds to chron.numData object (L$crhonData[[?]]) has the model you want to get the ensemble from
#' @param model.num an integer that corresponds to chron.num model you want to get the ensemble from?
#' @param ens.table.num an integer that corresponds to chron.num model ensembleTable you want to get the ensemble from?
#' @param max.ensemble.members Maximum number of ensemble members to map
#' @param strict.search Use a strict.search to look for the ageEnsemble and depth variables. TRUE(default) or FALSE   #' @param probs quantiles to calculate and plot
#' @param n.bins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param y.bin vector of bin edges over which to bin.
#' @param color.low Band color of the outer most band.
#' @param color.high Band color of the inner most band.
#' @param alp Transparency of the band plot
#' @param color.line Line color (following ggplot rules)
#' @param line.width Width of the line
#' @param add.to.plot A ggplot object to add this plot to. Default is ggplot() . 
#' @param n.ens.plot Number of ensemble members to plot
#' @param color.ens.line color of the ensemble lines
#' @param alp.ens.line transparency of the lines
#' @param probs quantiles to plot with ribbons
#' @return A ggplot object
plotChronEnsDiff = function(L,
                            ageEnsVar = "ageEnsemble",
                            age.var = "age",
                            depth.var = "depth",
                            paleo.num=NA,
                            paleo.meas.table.num=NA,
                            chron.num=NA,
                            model.num=NA,
                            ens.table.num = NA,
                            max.ensemble.members=NA,
                            strict.search=FALSE,
                            probs=c(0.025,.25,.5,.75,.975),
                            x.bin=NA,
                            y.bin=NA,
                            n.bins=100,
                            color.low="white",
                            color.high="grey70",
                            alp=1,
                            color.line="Black",
                            line.width=1,
                            add.to.plot=ggplot2::ggplot(),
                            n.ens.plot = 5,
                            color.ens.line = "red",
                            alp.ens.line = 0.7){
  
  
  L <- mapAgeEnsembleToPaleoData(L, age.var = ageEnsVar,chron.depth.var = depth.var,paleo.num = paleo.num, chron.num = chron.num, model.num = model.num,paleo.meas.table.num = paleo.meas.table.num, ens.table.num = ens.table.num)
  
  
  #get the paleo and chron Ensemble ages
  pAge <- selectData(L,var.name = age.var,paleo.or.chron.num = paleo.num,meas.table.num = paleo.meas.table.num)
  cAgeEns <- selectData(L,var.name = ageEnsVar,paleo.or.chron = "paleoData",paleo.or.chron.num = paleo.num,meas.table.num = paleo.meas.table.num)
  
  if(is.null(pAge)){
    stop("couldn't find the age/year paleoData")
  }
  if(is.null(cAgeEns)){
    stop("couldn't find the mapped ageEnsemble/yearEnsemble data in paleoData")
  }
  
  #calculate the difference
  ageDiff <- list()
  ageDiff$variableName <- paste0("Delta ",pAge$variableName)
  ageDiff$units <- pAge$units
  ageDiff$values <- pAge$values - cAgeEns$values
  axisLabel(ageDiff)
  
  
  #see if there's depth
  depth <- selectData(L,var.name = depth.var,paleo.or.chron.num = paleo.num,meas.table.num = paleo.meas.table.num)
  
  #if no depth then use age
  if(is.null(depth)){
    depth <- pAge
  }
  
  
  diffPlot <- plotTimeseriesEnsRibbons(X = depth ,Y = ageDiff,,alp = alp,probs = probs,x.bin = x.bin,y.bin = y.bin, n.bins = n.bins, color.low = color.low,color.high = color.high,color.line = color.line,line.width = line.width,add.to.plot = add.to.plot)
  
  #add some traces
  diffPlot <- plotTimeseriesEnsLines(add.to.plot = diffPlot,X = depth ,Y = ageDiff,alp = alp.ens.line,color = color.ens.line,n.ens.plot = n.ens.plot)
  
  return(diffPlot)
  
}




#' @export
#' @family plot
#' @family chron
#' @author Nick McKay
#' @title High-level chron plotting
#' @description Plot a chronology, either from a chron model (preferred) or from a chron measurement table if there is no model
#' @import ggplot2
#' @inheritDotParams plotChronEns
#' @inheritParams selectData
#' @param depth.var variableName to use for depth ("depth" by default)
#' @param age.var variableName to use for age ensemble ("age" by default)
#' @param age.14c.var variableName to use for age ensemble ("age14C" by default)
#' @param chron.number chron.numData object to use (NA by default, will ask if needed)
#' @param meas.num chron.numData model to use (NA by default, will ask if needed)
#' @param dot.size what size dot for the chron plot? Only used if not plotting by plotChronEns() (default = 5)
#' @param legend.position where to put the legend on the chron plot?
#' @return a ggplot object, or NA if there's chronData to plot
plotChron <- function(L,
                      chron.number = NA, 
                      meas.num = NA,
                      depth.var = "depth", 
                      age.var = "age",
                      age.14c.var = "age14C", 
                      dot.size = 5,
                      legend.position = c(0.7,0.3), ...){
  #grab the chronData 
  C = L$chronData
  
  #there must be a chronData to proceed
  if(is.null(C)){
    warning("Must have chronData to proceed. Exiting...")
    return(NA)
  }
  
  #figure out a chron.number
  if(is.na(chron.number)){
    if(length(C)==1){
      chron.number = 1
    }else{
      print(paste0("There are ", as.character(length(C)), " chronData objects. Which do you want to plot?"))
      chron.number=as.integer(readline(prompt = "Which chronData do you want to plot? Enter an integer "))
    }
  }
  
  #is there a model?
  if(!is.null({C[[chron.number]]$model[[1]]$ensembleTable})){#then use plotChronEns!
    chronPlot <- plotChronEns(L,chron.number = chron.number, depth.var = depth.var, age.var = age.var,...)+
      theme(legend.position = "none") 
  }else{#make a simpler plot from the measurementTable
    #look for the measurementTable
    if(is.null(C[[chron.number]]$measurementTable)){
      warning("No chron model, or measurementTable. Exiting...")
      return(NA)
    }
    
    #figure out a measurementTable number
    if(is.na(meas.num)){
      if(length(C[[chron.number]]$measurementTable)==1){
        meas.num = 1
      }else{
        print(paste0("There are ", as.character(length(C[[chron.number]]$measurementTable)), " chron measurement tables. Which do you want to plot?"))
        meas.num=as.integer(readline(prompt = "Which model do you want to plot? Enter an integer "))
      }
    }
    
    #get depth
    depth <- selectData(L,paleo.or.chron = "chronData",paleo.or.chron.num = chron.number, meas.table.num = meas.num, var.name = depth.var)
    
    #get age
    age <- selectData(L,paleo.or.chron = "chronData",paleo.or.chron.num = chron.number, meas.table.num = meas.num, var.name = age.var)
    #get 14Cage
    age14C <- selectData(L,paleo.or.chron = "chronData",paleo.or.chron.num = chron.number, meas.table.num = meas.num, var.name = age.14c.var)
    
    if(!is.null(age) & is.null(age14C)){
      ageDf <- data.frame(age = age$values, ageType = "calibratedAge")
    }else if(is.null(age) & !is.null(age14C)){
      ageDf <- data.frame(age = age14C$values, ageType = "14C Age")
    }else if(!is.null(age) & !is.null(age14C)){
      ageDf14C <- data.frame(age = age14C$values, ageType = "14C Age",stringsAsFactors = FALSE)
      ageDf <- data.frame(age = age$values, ageType = "calibrated age",stringsAsFactors = FALSE)
      naCal <- which(is.na(ageDf$age))
      ageDf[naCal, ] <- ageDf14C[naCal, ] 
    }else{
      stop("couldn't find any age data in chron measurement table")
    }
    
    ageDf$depth <- depth$values
    chronPlot <- ggplot(ageDf)+geom_point(aes(x = age, y = depth, color = ageType), size = dot.size)+
      scale_y_reverse(name = axisLabel(depth))+
      geoChronRPlotTheme() +  theme(legend.position = legend.position) +
      ggtitle(paste0(L$dataSetName,": chronData ", as.character(chron.number), " - measurementTable ", as.character(meas.num)))
    
    if(any(grepl("AD",age$units)) | any(grepl("CE",age$units))){
      chronPlot <- chronPlot + scale_x_continuous(name = axisLabel(age))
    }else{
      chronPlot <- chronPlot + scale_x_reverse(name = axisLabel(age))
    }
  }
  
  return(chronPlot)
  
}

#' @export
#' @family plot
#' @family chron
#' @author Nick McKay
#' @title Plot chron ensemble
#' @description Plot creates an age model plot with all the bells and whistles, including a spread of ensemble members, probability distributions, and a few example ensemble members. 
#' @import ggplot2
#' @inheritParams selectData
#' @param depth.var variableName to use for depth ("depth" by default)
#' @param age.var ariableName to use for age ensemble ("ageEnsemble" by default)
#' @param chron.number chron.numData object to use (NA by default, will ask if needed)
#' @param model.num chron.numData model to use (NA by default, will ask if needed)
#' @param probs quantiles to calculate and plot
#' @param n.bins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param y.bin vector of bin edges over which to bin.
#' @param color.low Band color of the outer most band.
#' @param color.high Band color of the inner most band.
#' @param alp Transparency of the band plot
#' @param color.line Line color (following ggplot rules)
#' @param line.width Width of the line
#' @param add.to.plot A ggplot object to add this plot to. Default is ggplot() . 
#' @param n.ens.plot Number of ensemble members to plot
#' @param color.ens.line color of the ensemble lines
#' @param alp.ens.line transparency of the lines
#' @param dist.color distribution color (following ggplot rules)
#' @param dist.type "violin" (default), "up" for one-sided distributions pointed up, "down" for one-sided distributions pointed down
#' @param dist.thick thickness of the line around the distribution
#' @param dist.alp alpha of the distribution
#' @param truncate.dist truncate probability density values below this number. NA (default) means no truncation
#' @param dist.scale controls the vertical span of the probability distribution. Approximately the vertical fraction of the plot that the distribution will cover. 
#' @param add.paleo.age.depth add a line that shows the paleoData age depth.
#' @param paleo.number paleo.num number for the paleoData age-depth
#' @param meas.num which measurement Table for the paleoData age-depth
#' @param color.line.paleo line color of the paleoData age-depth (following ggplot rules)
#' @param plot.traces Add timeseries lines to the plot (default = TRUE)
#' @return A ggplot object
plotChronEns = function(L,
                        age.var = "ageEnsemble",
                        depth.var = "depth",
                        chron.number=NA,
                        model.num = NA,
                        probs=c(0.025,.25,.5,.75,.975),
                        x.bin=NA,
                        y.bin=NA,
                        n.bins=100,
                        color.low="white",
                        color.high="grey70",
                        alp=1,
                        color.line="Black",
                        line.width=1,
                        add.to.plot=ggplot2::ggplot(),
                        n.ens.plot = 5,
                        color.ens.line = "red",
                        alp.ens.line = 0.7,
                        dist.alp = 0.3,
                        dist.type = "violin",
                        dist.color = "purple",
                        dist.thick = 0.1,
                        dist.scale = 0.02,
                        truncate.dist = NA,
                        add.paleo.age.depth = FALSE, 
                        paleo.number = NA, 
                        meas.num = NA,
                        color.line.paleo = "cyan",
                        plot.traces = TRUE){
  
  C = L$chronData
  if(any(is.na(chron.number))){
    if(length(C)==1){
      chron.number = 1
    }else{
      print(paste0("There are ", as.character(length(C)), " chronData objects. Which do you want to plot?"))
      chron.number=as.integer(readline(prompt = "Which chronData do you want to plot? Enter an integer "))
    }
  }
  
  if(any(is.na(model.num))){
    if(length(C[[chron.number]]$model)==1){
      model.num = 1
    }else{
      print(paste0("There are ", as.character(length(C[[chron.number]]$model)), " chron models. Which do you want to plot?"))
      model.num=as.integer(readline(prompt = "Which model do you want to plot? Enter an integer "))
    }
  }
  
  #check for ensemble table. For now this is required to plot.
  if(!any(grepl("ensembleTable",names(L$chronData[[chron.number]]$model[[model.num]])))){
    stop("No ensemble table found. At this time, plotChronEns() only works with chronData objects with ensemble tables.")
  }
  
  if(add.paleo.age.depth){#then add a line that shows depth vs age in the paleoTable
    P <- L$paleoData
    if(any(is.na(paleo.number))){
      if(length(P)==1){
        paleo.number = 1
      }else{
        print(paste0("There are ", as.character(length(P)), " paleoData objects. Which do you want to plot?"))
        paleo.number=as.integer(readline(prompt = "Which chronData do you want to plot? Enter an integer "))
      }
    }
    
    if(any(is.na(meas.num))){
      if(length(P[[paleo.number]]$measurementTable)==1){
        meas.num = 1
      }else{
        print(paste0("There are ", as.character(length(P[[paleo.number]]$measurementTable)), " paleo models. Which do you want to plot?"))
        meas.num=as.integer(readline(prompt = "Which model do you want to plot? Enter an integer "))
      }
    }
    
    #get the data from the paleo measurement table
    pDepth = selectData(L,var.name = "depth",paleo.or.chron = "paleoData",table.type = "measurement",meas.table.num = meas.num,paleo.or.chron.num = paleo.number)
    pAge = selectData(L,var.name = "age",paleo.or.chron = "paleoData",table.type = "measurement",meas.table.num = meas.num,paleo.or.chron.num = paleo.number)
    
    
  }
  
  
  
  
  #get the data from the chron ensemble table
  depth = selectData(L,var.name = depth.var,paleo.or.chron = "chronData",table.type = "ensemble",model.num = model.num,paleo.or.chron.num = chron.number)
  ageEnsemble = selectData(L,var.name = age.var,paleo.or.chron = "chronData",table.type = "ensemble",model.num = model.num,paleo.or.chron.num = chron.number)
  
  #if there's no depth, just plot by an index
  if(is.null(depth)){
    depth <- list()
    depth$values <- seq_len(nrow(ageEnsemble$values))
    depth$variableName <- "Index"
    depth$units <- "NA"
  }
  
  
  #quick fix to ensemble list bug
  ageEnsemble$values = as.matrix(as.data.frame(ageEnsemble$values))
  
  print("plotting your chron ensemble. This make take a few seconds...")
  
  #Ribbons first
  chronPlot = plotTimeseriesEnsRibbons(X = ageEnsemble,Y = depth,alp = alp,probs = probs,x.bin = x.bin,y.bin = y.bin, n.bins = n.bins, color.low = color.low,color.high = color.high,color.line = color.line,line.width = line.width,add.to.plot = add.to.plot)
  
  
  if(plot.traces){
  #A few traces second
  chronPlot = plotTimeseriesEnsLines(X = ageEnsemble,Y = depth,alp = alp.ens.line,color = color.ens.line,add.to.plot = chronPlot,n.ens.plot = n.ens.plot)
  }
  
  #distributions last
  if(is.list(C[[chron.number]]$model[[model.num]]$distributionTable)){#if it exists. Add it.
    chronPlot = plotModelDistributions(L,paleo.or.chron.num = chron.number,model.num = model.num,add.to.plot = chronPlot,alp=dist.alp,color = dist.color,dist.type = dist.type,thick = dist.thick,scale.frac = dist.scale,truncate.dist = truncate.dist)
  }
  
  
  #Compare with the paleoData depth-age ensemble
  if(add.paleo.age.depth){
    chronPlot <- chronPlot+geom_line(aes(x = pAge$values, y = pDepth$value), color = color.line.paleo)
  }
  
  
  
  #Tidy up...
  chronPlot = chronPlot + 
    scale_y_reverse(name = axisLabel(depth)) + 
    ggtitle(paste0(L$dataSetName)) + 
    theme(legend.position = "none")
  geoChronRPlotTheme()
  
  return(chronPlot)
  
}

#' @author Nick McKay
#' @family plot help
#' @title Melt distribution
#' @description Takes a LiPD model distribution and melt it into a single data.frame
#' @param this.dist LiPD "distributionTable" object
#' @param dist.plot vector of distribution tables to plot
#' @return data.frame of melted distribution objects.
#' @export
meltDistributionTable = function(this.dist,dist.plot = 1:length(this.dist)){
  #create large dataframe from dist object
  dist.df = NULL
  nDist = length(dist.plot)
  for(i in dist.plot){
    this.df = list()
    this.dist = dist[[i]]
    #lists first
    ll = which(sapply(this.dist,is.list))
    for(l in ll){
      this.name = names(this.dist)[l] 
      this.df[[this.name]] = this.dist[[l]]$values
    }
    #convert to df
    this.df = as.data.frame(this.df)
    
    ln = which(!sapply(this.dist,is.list))
    for(l in ln){
      this.name = names(this.dist)[l] 
      this.df[this.name] = this.dist[[l]]
    }
    
    if(is.null(dist.df)){
      dist.df = this.df
    }else{
      dist.df = rbind(dist.df,this.df)
    }
    dist.df = rbind(dist.df,rep(NA,ncol(this.df)))
  }
  return(dist.df)
}
