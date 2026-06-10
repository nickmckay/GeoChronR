# Spectral analysis plots: ensemble and single spectra, period annotation.


#' @export
#' @title Plot ensemble spectra output
#' @description Plot the output of `computeSpectraEns` as a ribbon plot of distributions, with specified confidence levels
#' @details `plotSpectraEns` re-uses `plotTimeseriesRibbons` and therefore the same graphical conventions. Spectra are plotted on a log-log scale, with the x-axis labeled by periods instead of frequencies, for improved intelligibility. 
#' @family plot
#' @inheritParams plotTimeseriesEnsRibbons
#' @param spec.ens list or dataframe containing frequency (freq) and power (pwr); typically output of computeSpectraEns
#' @param cl.df list or dataframe containing confidence limits (90, 95 and 99\%) as well as frequency (freq)
#' @param x.lims 2-element vector defining the range of periods (x-axis)
#' @param x.ticks n-element vector of the periods labeled
#' @param y.lims 2-element vector defining the range of spectral power (y-axis)
#' @param color.cl color of the lines representing the confidence limits (90, 95, 99\%)
#' @return a ggplot object
#' @author Julien Emile-Geay
#' @import ggplot2
#' @importFrom tidyr pivot_longer
plotSpectraEns = function (spec.ens,
                           cl.df = NULL,
                           x.lims = NULL,
                           x.ticks = c(10, 20, 50, 100, 200, 500, 1000),
                           y.lims = NULL,
                           color.low="white",
                           color.high="grey70",
                           color.line="Black",
                           color.cl="red",
                           alp=0.5){
  
  freq = spec.ens$freqs 
  if (is.matrix(freq)){freq <- rowMedians(freq)}
  
  period <- 1 / freq 
  
  if (is.null(x.lims)) {
    x.lims = c(min(period), max(period))
  } else {
    f.low = 1 / x.lims[2]
    f.high = 1 / x.lims[1]
  }
  freq_range = which(freq >= f.low & freq <= f.high)
  
  if (is.null(y.lims)) {
    m <- floor(log10(min(spec.ens$power[freq_range,])))
    M <- ceiling(log10(max(spec.ens$power[freq_range,])))
  }
  else {
    m <- log10(y.lims[1])
    M <- log10(y.lims[2])
  }
  
  specPlot = plotTimeseriesEnsRibbons(X = period,
                                      Y = spec.ens$power,
                                      color.low = color.low,
                                      color.high = color.high,
                                      color.line = color.line,
                                      alp = alp) +
    scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  limits = c(10^m,10^M)) + 
    scale_x_continuous(breaks=x.ticks, minor_breaks = NULL, trans=reverselog10_trans(), limits = rev(x.lims)) +
    xlab("Period") + ylab("PSD")
  
  if (!is.null(cl.df)) {# if data about confidence limit are provided, plot them
    cl.df.rs = tidyr::pivot_longer(cl.df,cols = -freq,values_to = "value",names_to = "variable") # reshape to facilitate one-line plotting call
    specPlot <- specPlot + geom_line(data=cl.df.rs,aes(x=1/freq,y=value,linetype=variable),color=color.cl)
  }
  
  #if(any(!is.na(spec.ens$powerSyn))){
  #  specPlot = plotTimeseriesEnsRibbons(X = spec.ens$freqs, Y = spec.ens$powerSyn,add.to.plot = specPlot,probs = c(.9,.95),color.high = "red",alp = .5)
  #}
  
  # Other option: https://stackoverflow.com/questions/37326686/ggplot2-geom-ribbon-with-alpha-dependent-on-data-density-along-y-axis-for-each
  
  return(specPlot)
}

#' @export
#' @title Plot spectrum with confidence limits
#' @description Plot a a single spectrum, with confidence limits (no age ensemble). Useful for comparison with plotSpectraEns() in cases of no age uncertainty (e.g. GCM output)
#' @family plot
#' @family spectra
#' @param spec.df list or dataframe containing frequency (freq) and power (pwr)
#' @param cl.df list or dataframe containing confidence limits (90, 95 and 99\%) as well as frequency (freq)
#' @param x.lims range of plotted periodicities
#' @param x.ticks ticks to mark on the period axis. if NULL, defaults to (10, 20, 50, 100, 200, 500, 1000)
#' @param y.lims 2-vector for the y-axis. If NULL, computed from range(pwr)
#' @param color.line color of the line representing the spectrum
#' @param color.cl color of the lines representing the confidence limits (90, 95, 99\%)
#' @return a ggplot object
#' @author Julien Emile-Geay
#' @import ggplot2
#' @importFrom tidyr pivot_longer
plotSpectrum = function (spec.df,
                         cl.df = NULL,
                         x.lims=NULL,
                         x.ticks= c(10, 20, 50, 100, 200, 500, 1000),
                         y.lims = NULL,
                         color.line="black", 
                         color.cl = "red"){
  # TO DO: general handling of colors (theme)
  
  period <- 1/spec.df$freq
  if (is.null(x.lims)) {
    x.lims = c(min(period),max(period))
  } else {
    f.low = 1/x.lims[2]
    f.high = 1/x.lims[1]
  }
  freq_range = (spec.df$freq>= f.low & spec.df$freq<=f.high)
  
  if (is.null(y.lims)) {
    m <- floor(log10(min(spec.df$pwr[freq_range]))) 
    M <- ceiling(log10(max(spec.df$pwr[freq_range]))) 
  }
  else {
    m <- log10(y.lims[1])
    M <- log10(y.lims[2])
  }
  
  specPlot <- ggplot() + geom_line(aes(x=period,y=spec.df$pwr),color=color.line) + 
    scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  limits = c(10^m,10^M)) + 
    scale_x_continuous(breaks=x.ticks, minor_breaks = NULL, trans=reverselog10_trans(), limits = rev(x.lims)) +
    xlab("Period") + ylab("Power")
  
  if (!is.null(cl.df)) {# if data about confidence limit are provided, plot them
    cl.df.rs = tidyr::pivot_longer(cl.df,cols = -freq,values_to = "value",names_to = "variable") # reshape to facilitate one-line plotting call
    specPlot <- specPlot + geom_line(data=cl.df.rs,aes(x=1/freq,y=value,linetype=variable),color=color.cl)
  }
  
  return(specPlot)
}


#' @export
#' @title Annotate plot of spectra with given periodicities
#' @description Annotate plot of spectra (ensemble or otherwise) with vertical lines at specific periodicities (assumes log10 scaling)
#' @family plot help
#' @family spectra
#'
#' @param specPlot ggplot handle to figure containing spectrum
#' @param periods the periods to highlight in the spectrum
#' @param log10scale Use a log 10 scale TRUE (default) or FALSE
#' @param y.lims optionally specify y limits (default = NULL)
#' @param size font size for labels (default = 4)
#' @param color the color of the text and lines
#'
#' @return ggplot object of spectrum plot
#' @author Julien Emile-Geay
periodAnnotate = function (specPlot, 
                           periods, 
                           color = "orange",
                           log10scale = T, 
                           y.lims = NULL, 
                           size = 4){
  
  if (is.null(y.lims)) {
    ggp <- ggplot_build(specPlot)
    y.lims <- ggp$layout$panel_params[[1]]$y.range # this could break with multiplots... 
  }
  
  for(per in periods){
    specPlot <- specPlot + annotate("segment", x = per, xend = per, y = y.lims[1], yend = y.lims[2],
                                    color = color, alpha = 0.3, linetype = "dotdash")
    specPlot <- specPlot +  annotate("text", x = 1.03*per, y = 1.2*y.lims[2], label = format(per,digits=2, nsmall=0), color = color, size = size)
  }
  return(specPlot)  
}
