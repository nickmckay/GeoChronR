## ------------------------------------------------------------------------
library(lipdR)
library(geoChronR)
library(magrittr)
library(ggplot2)

## ---- message=FALSE------------------------------------------------------
hulu <- readLipd(system.file("extdata", "Hulucave.Wang.2001.lpd", package = "geoChronR"))

## ---- cache=TRUE, message=FALSE,warning=FALSE----------------------------
hulu <- runBchron(hulu,calCurves = "normal", iter = 10000,which.table = 2,interpolate = T,age14CVar = "age",age14CuncertaintyVar = "ageUncertaintyHigh",labIDVar = NULL,reject = F,ageVar = NULL,ageUncertaintyVar = NULL,rejectedAgesVar = NULL,extractDate = 10000)

## ----warning=FALSE-------------------------------------------------------
plotChron(hulu,truncateDist = 1e-4)

## ------------------------------------------------------------------------
hulu = mapAgeEnsembleToPaleoData(hulu,age.var = "ageEnsemble",which.pmt = 2)

## ------------------------------------------------------------------------
hulu.ae <- selectData(hulu,varName = "ageEnsemble",which.mt = 2)
hulu.d18O<- selectData(hulu,varName = "d18O",which.mt = 2)

## ------------------------------------------------------------------------
hulu.ts.plot = plotTimeseriesEnsLines(X = hulu.ae,Y = hulu.d18O,alp = 0.01,maxPlotN = 50,color = "blue")
hulu.ts.plot

## ------------------------------------------------------------------------
hulu.ts.plot = plotTimeseriesEnsRibbons(X = hulu.ae,Y = hulu.d18O,nbins = 1000)+xlim(c(35000,75000))
hulu.ts.plot

## ------------------------------------------------------------------------
hulu.ts.plot <- plotTimeseriesEnsLines(X = hulu.ae,Y = hulu.d18O,alp = 0.1,maxPlotN = 10,color = "red",add.to.plot = hulu.ts.plot)+ggtitle("Hulu Cave d18O")
hulu.ts.plot

## ---- message=FALSE------------------------------------------------------
gisp2 <- readLipd(system.file("extdata", "GISP2.Ally.2000.lpd", package = "geoChronR"))

## ------------------------------------------------------------------------
gisp2 = runBam(gisp2,which.paleo = 1, which.pmt = 1,which.chron = 1,which.model = 1,ens.number = 1, makeNew = T,nens = 1000,model = list(name = "poisson",param = 0.02, resize = 0, ns = 1000))

## ------------------------------------------------------------------------
gisp2.d18O <- selectData(gisp2,varName = "temp")

gisp2.ens <- selectData(gisp2,varName = "yearEnsemble")

gisp2.ens$values[1:10,1:5]


## ------------------------------------------------------------------------
gisp2.ens$units

## ------------------------------------------------------------------------
gisp2.ens <- convertAD2BP(gisp2.ens)

## ------------------------------------------------------------------------
gisp2.ts.plot <- plotTimeseriesEnsRibbons(X = gisp2.ens,Y = gisp2.d18O,nbins = 500) %>% 
  plotTimeseriesEnsLines(X = gisp2.ens,Y = gisp2.d18O,maxPlotN = 5,color = "red",alp = .1)+
  ggtitle("GISP2 temperature")

print(gisp2.ts.plot)


## ------------------------------------------------------------------------
library(gridExtra)
overlap <- c(36000,50000)
grid.arrange(grobs = list(gisp2.ts.plot+xlim(overlap),hulu.ts.plot+xlim(overlap)),nrow = 2)

## ---- cache=TRUE---------------------------------------------------------
corout <- corEns(gisp2.ens,gisp2.d18O,hulu.ae,hulu.d18O,binstep = 500,max.ens = 100)

## ------------------------------------------------------------------------
plotCorEns(corout,legendPosition = c(0.1, 0.8),significanceOption = "autocorr")


