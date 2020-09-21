## ---- results = FALSE, warning = FALSE, message= FALSE------------------------
library(geoChronR)
library(magrittr)
library(ggplot2)

## -----------------------------------------------------------------------------
if(requireNamespace("lipdR")){
  hulu <- lipdR::readLipd("http://lipdverse.org/geoChronR-examples/Hulucave.Wang.2001.lpd")
}else{
  hulu <- loadRemote("http://lipdverse.org/geoChronR-examples/Hulucave.Wang.2001.RData")
}

## ---- cache=TRUE, results = 'hide'--------------------------------------------
hulu <- runBchron(hulu,
                  cal.curves = "normal",
                  iter = 10000,
                  extractDate = 10000,
                  which.table = 2,
                  lab.id.var = NULL,
                  age.var = 'age',
                  age.uncertainty.var = 'ageUncertaintyHigh',
                  age.14c.var = NULL,
                  age.14c.uncertainty.var =  NULL,
                  depth.var = 'depth',
                  reservoir.age.14c.var = NULL,
                  reservoir.age.14c.uncertainty.var = NULL,
                  rejected.ages.var = NULL)

#correct units and rename
hulu$chronData[[1]]$model[[1]]$ensembleTable[[1]]$depth$units <- "mm"
hulu$chronData[[1]]$model[[1]]$ensembleTable[[1]]$depth$variableName <- "position"

## ----warning=FALSE,results = 'hide',message = FALSE---------------------------
chronPlot <- plotChronEns(hulu,truncate.dist = 1e-6)+ggtitle(NULL)+coord_cartesian(xlim = c(80000,30000))
print(chronPlot)

## ---- results = 'hide'--------------------------------------------------------
hulu <- mapAgeEnsembleToPaleoData(hulu,age.var = "ageEnsemble",paleo.meas.table.num = 2)

## -----------------------------------------------------------------------------
hulu.ae <- selectData(hulu,var.name = "ageEnsemble",meas.table.num = 2)
hulu.d18O <- selectData(hulu,var.name = "d18O",meas.table.num = 2)

## ----results = 'hide',warning = FALSE,message = FALSE-------------------------
hulu.ts.plot <-  plotTimeseriesEnsLines(X = hulu.ae,Y = hulu.d18O,alp = 0.05,n.ens.plot = 50,color = "blue")
print(hulu.ts.plot)

## ----results = 'hide',warning=FALSE,message = FALSE---------------------------
hulu.ts.plot <- plotTimeseriesEnsRibbons(X = hulu.ae,Y = hulu.d18O,n.bins = 1000)+xlim(c(35000,75000))
print(hulu.ts.plot)

## ----results = 'hide',warning = FALSE,message = FALSE-------------------------
hulu.ts.plot <- plotTimeseriesEnsLines(X = hulu.ae,Y = hulu.d18O,alp = 0.1,n.ens.plot = 10,color = "red",add.to.plot = hulu.ts.plot)+ggtitle("Hulu Cave d18O")
print(hulu.ts.plot)

## ---- message=FALSE-----------------------------------------------------------
if(requireNamespace("lipdR")){
  gisp2 <- lipdR::readLipd("http://lipdverse.org/geoChronR-examples/GISP2.Alley.2000.lpd")
}else{
  gisp2 <- loadRemote("http://lipdverse.org/geoChronR-examples/GISP2.Alley.2000.RData")
}

## ----results = 'hide'---------------------------------------------------------
gisp2 = runBam(gisp2,
               paleo.num = 1, 
               paleo.meas.table.num = 1,
               chron.num = 1,
               model.num = 1,
               ens.table.number = 1, 
               make.new = T,
               n.ens = 1000,
               model = list(name = "poisson",
                            param = 0.02, 
                            resize = 0, 
                            ns = 1000)
               )

## -----------------------------------------------------------------------------
gisp2.d18O <- selectData(gisp2,var.name = "temp")

gisp2.ens <- selectData(gisp2,var.name = "yearEnsemble")

gisp2.ens$values[1:10,1:5]


## -----------------------------------------------------------------------------
gisp2.ens$units

## -----------------------------------------------------------------------------
gisp2.ens <- convertAD2BP(gisp2.ens)

## ----results = 'hide',message = FALSE,warning=FALSE---------------------------
gisp2.ts.plot <- plotTimeseriesEnsRibbons(X = gisp2.ens,Y = gisp2.d18O,n.bins = 500) %>% 
  plotTimeseriesEnsLines(X = gisp2.ens,Y = gisp2.d18O,n.ens.plot = 5,color = "Reds",alp = .2)+
  ggtitle("GISP2 temperature")

print(gisp2.ts.plot)


## ----results = 'hide',warnings = FALSE----------------------------------------
library(gridExtra)
overlap <- c(36000,50000)
grid.arrange(grobs = list(gisp2.ts.plot+xlim(rev(overlap)),hulu.ts.plot+xlim(rev(overlap))),nrow = 2)

