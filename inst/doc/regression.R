## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 8,fig.height = 6) 
knitr::opts_knit$set(progress = FALSE,verbose = FALSE)

## ---- results = FALSE, warning = FALSE, message= FALSE------------------------
library(lipdR) #to read and write LiPD files
library(geoChronR) #of course
library(readr) #to load in the instrumental data we need
library(ggplot2) #for plotting

## -----------------------------------------------------------------------------
if(requireNamespace("lipdR")){
K <- lipdR::readLipd("http://lipdverse.org/geochronr-examples/Kurupa.Boldt.2015.lpd")
}else{
K <- loadRemote("http://lipdverse.org/geochronr-examples/Kurupa.Boldt.2015.Rdata")
}

## -----------------------------------------------------------------------------
sp <- plotSummary(K,paleo.data.var = "RABD",summary.font.size = 6)
print(sp)

## ----results="hide",fig.keep="all",cache=TRUE---------------------------------
K <- runBacon(K,
              lab.id.var = 'labID', 
              age.14c.var = 'age14C', 
              age.14c.uncertainty.var = 'age14CUncertainty',
              age.var = 'age', 
              age.uncertainty.var = 'ageUncertainty', 
              depth.var = 'depth', 
              reservoir.age.14c.var = NULL, 
              reservoir.age.14c.uncertainty.var = NULL, 
              rejected.ages.var = NULL,
              bacon.acc.mean = 10,
              bacon.thick = 7,
              ask = FALSE,
              bacon.dir = "~/Cores",
              suggest = FALSE,
              close.connection = FALSE)

## -----------------------------------------------------------------------------
plotChron(K,age.var = "ageEnsemble",dist.scale = 0.2)

## -----------------------------------------------------------------------------
K <- mapAgeEnsembleToPaleoData(K,age.var = "ageEnsemble")

## -----------------------------------------------------------------------------
kae <-  selectData(K,"ageEnsemble")
rabd <- selectData(K,"RABD")

## -----------------------------------------------------------------------------
kurupa.instrumental <- readr::read_csv("http://lipdverse.org/geochronr-examples/KurupaInstrumental.csv")

## -----------------------------------------------------------------------------
kae$units

## -----------------------------------------------------------------------------
kae <- convertBP2AD(kae)

## -----------------------------------------------------------------------------
kyear <- list()
kyear$values <- kurupa.instrumental[,1]
kyear$variableName <- "year"
kyear$units <- "AD"

kinst <- list()
kinst$values <- kurupa.instrumental[,2]
kinst$variableName <- "Temperature"
kinst$units <- "deg (C)"

## ----results="hide",warning=FALSE---------------------------------------------
corout <- corEns(kae,rabd,kyear,kinst,bin.step=2,percentiles = c(.05,.5,.95 ))

## -----------------------------------------------------------------------------
plotCorEns(corout,significance.option = "eff-n")

## ----results="hide",fig.keep="all"--------------------------------------------
regout <- regressEns(time.x = kae,
                    values.x = rabd,
                    time.y =kyear,
                    values.y =kinst,
                    bin.step=3,
                    gaussianize = FALSE,
                    recon.bin.vec = seq(-4010,2010,by=20))

## -----------------------------------------------------------------------------
regPlots <- plotRegressEns(regout,alp = 0.01,font.size = 8)

