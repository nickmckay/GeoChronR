## ---- results = FALSE, warning = FALSE, message= FALSE------------------------
library(lipdR)
library(geoChronR)
library(magrittr)
library(ggplot2)

## -----------------------------------------------------------------------------
hulu <- lipdR::readLipd("http://lipdverse.org/geoChronR-examples/Hulucave.Wang.2001.lpd")

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

## ----warning=FALSE------------------------------------------------------------
plotChronEns(hulu,truncate.dist = 1e-4)

## ---- results = 'hide'--------------------------------------------------------
hulu = mapAgeEnsembleToPaleoData(hulu,age.var = "ageEnsemble",paleo.meas.table.num = 2)

## -----------------------------------------------------------------------------
hulu.ae <- selectData(hulu,var.name = "ageEnsemble",meas.table.num = 2)
hulu.d18O<- selectData(hulu,var.name = "d18O",meas.table.num = 2)

## -----------------------------------------------------------------------------
hulu.ts.plot = plotTimeseriesEnsLines(X = hulu.ae,Y = hulu.d18O,alp = 0.05,n.ens.plot = 50,color = "blue")
hulu.ts.plot

## -----------------------------------------------------------------------------
hulu.ts.plot = plotTimeseriesEnsRibbons(X = hulu.ae,Y = hulu.d18O,n.bins = 1000)+xlim(c(35000,75000))
hulu.ts.plot

## -----------------------------------------------------------------------------
hulu.ts.plot <- plotTimeseriesEnsLines(X = hulu.ae,Y = hulu.d18O,alp = 0.1,n.ens.plot = 10,color = "red",add.to.plot = hulu.ts.plot)+ggtitle("Hulu Cave d18O")
hulu.ts.plot

## ---- message=FALSE-----------------------------------------------------------
gisp2 <- lipdR::readLipd("http://lipdverse.org/geoChronR-examples/GISP2.Alley.2000.lpd")

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
gisp2.ts.plot <- plotTimeseriesEnsRibbons(X = gisp2.ens,Y = gisp2.d18O,n.bins = 500) %>% 
  plotTimeseriesEnsLines(X = gisp2.ens,Y = gisp2.d18O,n.ens.plot = 5,color = "Reds",alp = .2)+
  ggtitle("GISP2 temperature")

print(gisp2.ts.plot)


## -----------------------------------------------------------------------------
library(gridExtra)
overlap <- c(36000,50000)
grid.arrange(grobs = list(gisp2.ts.plot+xlim(overlap),hulu.ts.plot+xlim(overlap)),nrow = 2)

## ---- cache=TRUE,  results = 'hide'-------------------------------------------
corout <- corEns(gisp2.ens,gisp2.d18O,hulu.ae,hulu.d18O,bin.step = 500,max.ens = 100)

## -----------------------------------------------------------------------------
plotCorEns(corout,legend.position = c(0.1, 0.8),significance.option = "autocorr")


