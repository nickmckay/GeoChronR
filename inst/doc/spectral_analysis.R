## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 8,fig.height = 6) 
knitr::opts_knit$set(progress = FALSE,verbose = FALSE)

## ----load data, message=FALSE-------------------------------------------------
library(lipdR) # to load the file
library(geoChronR) # to analyze it
library(ggthemes) # to define plotting theme
library(ggplot2)  # to plot

I <- readLipd("http://lipdverse.org/geoChronR-examples/ODP846.Lawrence.2006.lpd")

## -----------------------------------------------------------------------------
d18O =  I$chronData[[1]]$model[[1]]$summaryTable[[1]]$d180$values
t.med <- I$chronData[[1]]$model[[1]]$summaryTable[[1]]$median$values
L <- geoChronR::mapAgeEnsembleToPaleoData(I,paleo.meas.table.num = 1)
age = geoChronR::selectData(L,"ageEnsemble",meas.table.num = 1)
temp = geoChronR::selectData(L,"temp muller",meas.table.num = 1)
plotTimeseriesEnsRibbons(ggplot(),X=age,Y=temp,color.low="orchid",color.high="darkorange3",color.line="orange",line.width=0.5,alp=0.3) + scale_x_reverse() + theme_hc(style = "darkunica")

## ---- message=FALSE-----------------------------------------------------------
age.median = matrixStats::rowQuantiles(age$values,probs = 0.5)
temp.median = matrixStats::rowQuantiles(as.matrix(temp$values),probs = 0.5)
t <-age.median[age.median < 1000]
X <- temp.median[age.median < 1000]
X <- X - mean(X)
#dfs = dplyr::filter(df,t<=1000)
dt = diff(t)
ggplot() + xlim(c(0,10)) + 
  geom_histogram(aes(x=dt,y = ..density..), bins = 25, ,alpha = 0.8, fill = "orange") + ggtitle("Distribution of time intervals") + theme_hc(style = "darkunica") + xlab(expression(Delta*"t"))

## ----lomb-scargle-------------------------------------------------------------
spec.ls <- computeSpectraEns(t,X,method = 'lomb-scargle')
ls.df <- data.frame("freq" = spec.ls$freqs, "pwr" = spec.ls$power)
# estimate significance against a power-law fit
f.low <-  1e-3; f.high <- 0.1
plaw.ls <-  astrochron::pwrLawFit(ls.df, dof = 2, flow = f.low, fhigh = f.high, output = 1, genplot = F)

cl.df <- data.frame(plaw.ls[,union(1,c(5:7))]) # extract confidence limits 
# rename columns to be less silly
names(cl.df)[1] <- "freq"
names(cl.df)[2] <- "90% CL"
names(cl.df)[3] <- "95% CL"
names(cl.df)[4] <- "99% CL"

# plot this 
pticks = c(10, 20, 50, 100, 200, 500, 1000)
prange = c(10,1000)
yl = c(0.01,1000)
p.ls <- plotSpectrum(ls.df,cl.df,x.lims = prange,x.ticks = pticks, y.lims = yl,
                     color.line='orange', color.cl='white') + 
  ggtitle("IODP 846 d18O, Lomb-Scargle periodogram") +
  theme_hc(style = "darkunica") + theme(axis.ticks.x = element_line(color = "gray"))

# label periodicities of interest
p.ls <- periodAnnotate(p.ls, periods = c(19,23,41,100), y.lims =c(0.01,100))
show(p.ls)

## ----REDFIT-------------------------------------------------------------------
spec.redfit <- computeSpectraEns(t,X,method = 'redfit')
redfit.df <- data.frame("freq" = spec.redfit$freq, "pwr" = spec.redfit$power)
cl.df <- data.frame("freq" = spec.redfit$freq, "95% CL" = spec.redfit$power.CL)
names(cl.df)[2] <- "95% CL"

# plot this 
pticks = c(10, 20, 50, 100, 200, 500, 1000)
prange = c(10,1000)
yl = c(0.01,1000)
p.ls <- plotSpectrum(redfit.df,cl.df,x.lims=prange,x.ticks = pticks, y.lims = yl,
                     color.line='orange', color.cl='white') + 
  ggtitle("IODP 846 d18O, REDFIT estimation") +
  theme_hc(style = "darkunica") + theme(axis.ticks.x = element_line(color = "gray"))

# label periodicities of interest
p.ls <- periodAnnotate(p.ls, periods = c(19,23,41,100), y.lims =c(0.01,100))
show(p.ls)

## ----interpolation, results="hide"--------------------------------------------
library(astrochron)
dfs = data.frame(time=t,temp=X)
dti = 2.5  # interpolation interval, corresponding to the mode of the \Delta t distribution
dfe = linterp(dfs,dt=dti)
tbw = 2  #time-bandwidth product for the analysis; we use the minimum allowed to limit smoothing. 

## ----MTM, warning=F, message=F------------------------------------------------
spec.mtm <- computeSpectraEns(dfe$time,dfe$temp,method = 'mtm', tbw=tbw) # tbw is the time-bandwidth product, p in the above
sig.freq <- astrochron::mtm(dfe,tbw=tbw, padfac=5,ar1=TRUE,genplot = F,output=2, verbose = F, detrend=T)
mtm.df <- data.frame("freq" = spec.mtm$freqs, "pwr" = spec.mtm$power)

# plot this 
prange = c(5,1000)

p.mtm <- plotSpectrum(mtm.df,x.lims=prange,x.ticks = pticks, y.lims = c(1e-6,10), color.line='orange') + 
  ggtitle("IODP 846 d18O, Multi-taper method, AR(1) null") + xlab("Period (ky)") +
  theme_hc(style = "darkunica") + theme(axis.ticks.x = element_line(color = "gray"))

# label periodicities of interest
p.mtm <- periodAnnotate(p.mtm, periods = c(19,23,41,100), y.lims = c(1e-6,1))
p.mtm <- periodAnnotate(p.mtm, periods = 1/sig.freq$Frequency,color = "chartreuse",y.lims = c(1e-6,.1))
show(p.mtm)

## -----------------------------------------------------------------------------
mtmPL.df <- computeSpectraEns(t,X,method = 'mtm', tbw=tbw, mtm_null = 'power_law') # tbw is the time-bandwidth product, p in the above
sig.freqPL <- astrochron::mtmPL(dfe,tbw=tbw,padfac=5,genplot = F,output=2, verbose = F, flow=f.low, fhigh=f.high)
mtm.df = mtmPL.df[c(1,2)]
names(mtm.df)[1] <- "freq"
names(mtm.df)[2] <- "pwr"

cl.df <- data.frame(mtmPL.df$freqs,mtmPL.df$power.CL) # extract confidence limits 
# rename columns to be less silly
names(cl.df)[1] <- "freq"
#names(cl.df)[2] <- "90% CL"
names(cl.df)[2] <- "95% CL"
#names(cl.df)[4] <- "99% CL"


# plot it 
p.mtmPL <- plotSpectrum(mtm.df,cl.df,x.lims = prange,x.ticks = pticks, 
                        y.lims = c(1e-6,10),color.line='orange', color.cl='white') + 
  ggtitle("IODP 846 d18O, Multi-taper method, power-law null") + xlab("Period (ky)") +
  theme_hc(style = "darkunica") + theme(axis.ticks.x = element_line(color = "gray"))

# label periodicities of interest
p.mtmPL <- periodAnnotate(p.mtmPL, periods = c(19,23,41,100), y.lims = c(1e-6,1))
p.mtmPL <- periodAnnotate(p.mtmPL, periods = 1/sig.freqPL$Frequency,color = "chartreuse",y.lims = c(1e-6,.1))
show(p.mtmPL)

## -----------------------------------------------------------------------------
if(!requireNamespace("nuspectral",quietly = TRUE)){#see if nuspectral is already installed
  remotes::install_github("nickmckay/nuspectral") #install nuspectral if needed
}

## ----nuspectral estimation----------------------------------------------------
requireNamespace("nuspectral", quietly = TRUE)
nt = length(t)
tau = seq(min(t),max(t),length = max(nt %/% 2,5))
nuspec <-  nuspectral:::nuwavelet_psd(t,X,sigma=0.01,taus = tau)
nf <- length(nuspec$Frequency)
nus.df <- data.frame("freq" = nuspec$Frequency, "pwr" = nuspec$Power) 

# power law fit
plaw.fit = astrochron::pwrLawFit(nus.df, dof = 2, flow = 0.001, fhigh = 0.1, output = 1, genplot = F)
cl.df <- data.frame(plaw.fit[,union(1,c(5:7))]) # extract confidence limits 
# rename columns to be less silly
names(cl.df)[1] <- "freq"
names(cl.df)[2] <- "90% CL"
names(cl.df)[3] <- "95% CL"
names(cl.df)[4] <- "99% CL"

## ----nuspectral plotting------------------------------------------------------
p.nus <- plotSpectrum(nus.df,cl.df,
                      x.lim=c(10,1000),
                      x.ticks = pticks, 
                      y.lims = c(0.1,300),
                      color.line='orange',
                      color.cl='white') + 
  ggtitle("IODP 846 d18O, nuspectral periodogram") +
  theme_hc(style = "darkunica") + theme(axis.ticks.x = element_line(color = "gray"))

# label periodicities of interest
p.nus <- periodAnnotate(p.nus, periods = c(19,23,41,100),y.lims = c(0.1,100))
show(p.nus)

## ----ensemble-mode MTM, compute-----------------------------------------------
time = age$values[age.median < 1000,]  # age ensemble for ~ last 1 Ma
values = temp$values[age.median < 1000]
mtm.ens.spec = computeSpectraEns(time,values,
                                 max.ens = 1000, 
                                 method = 'mtm',
                                 tbw=tbw, padfac=5, 
                                 mtm_null = 'AR(1)')

## ----ensemble-mode MTM, plot--------------------------------------------------
ar1.df <- data.frame(mtm.ens.spec$freqs,mtm.ens.spec$power.CL)
# rename columns to be less silly
names(ar1.df)[1] <- "freq"
names(ar1.df)[2] <- "95% CL"

p.mtm.ens <- plotSpectraEns(spec.ens = mtm.ens.spec, cl.df = ar1.df, x.lims = c(10,150), y.lims = c(1e-5,1e-1),
                            color.line='darkorange',
                            color.low='darkorange2',
                            color.high='coral4',color.cl = 'white') + 
  ggtitle("IODP 846 age ensemble, MTM") + theme_hc(style = "darkunica") + 
  theme(axis.ticks.x = element_line(color = "gray"))

# label periodicities of interest
p.mtm.ens <- periodAnnotate(p.mtm.ens, periods = c(19,23,41,100), y.lims =c(0.01,0.05))
show(p.mtm.ens)

## ---- MTM ensemble, with significant frequencies------------------------------
per = 1/cl.df$freq
sig_per =  which(mtm.ens.spec$sig.freq>0.5)
p.mtm.ens <- periodAnnotate(p.mtm.ens, periods = per[sig_per],color = "chartreuse",y.lims = c(1e-5,5e-4))
show(p.mtm.ens)

## ----ensemble-mode REDFIT-----------------------------------------------------
spec.redfit.ens <- computeSpectraEns(time,values,max.ens = 1000, method = 'redfit')
cl.ens.df     <- data.frame("freq" = spec.redfit.ens$freq, "pwr95" = spec.redfit.ens$power.CL) # store confidence limits
names(cl.ens.df)[2] <- "95% CL"

## ----ensemble-mode REDFIT plot------------------------------------------------
p.redfit.ens <- plotSpectraEns(spec.ens = spec.redfit.ens, 
                               cl.df = cl.ens.df, 
                               x.lims = c(10,150), 
                               color.line='darkorange',
                               color.low='darkorange2',
                               color.high='coral4',
                               color.cl = 'white') + 
  ggtitle("IODP 846 age ensemble, redfit") + 
  theme_hc(style = "darkunica") + 
  theme(axis.ticks.x = element_line(color = "gray"))

# label periodicities of interest
p.redfit.ens <- periodAnnotate(p.redfit.ens, periods = c(19,23,41,100), y.lims =c(0.01,500))
show(p.redfit.ens)

