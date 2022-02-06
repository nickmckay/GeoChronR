library(magrittr)
library(lipdR)
agassiz <- readLipd("https://lipdverse.org/geoChronR-examples/arc2k/Arc-Agassiz.Vinther.2008.lpd") %>% 
  mapAgeEnsembleToPaleoData(strict.search = TRUE,
                            age.var = "ageEnsemble",
                            chron.depth.var = NULL )

ag.ae <- selectData(agassiz,"ageEnsemble")
ag.d18O <- selectData(agassiz,"d18O")


crete <- readLipd("https://lipdverse.org/geoChronR-examples/arc2k/Arc-Crete.Vinther.2010.lpd") %>% 
  mapAgeEnsembleToPaleoData(strict.search = TRUE,
                            age.var = "ageEnsemble",
                            chron.depth.var = NULL )
cr.ae <- selectData(crete,"ageEnsemble")
cr.d18O <- selectData(crete,"d18O")

ag <- plotTimeseriesEnsRibbons(X = ag.ae,Y = ag.d18O) + xlim(c(550,1950))
cr <- plotTimeseriesEnsRibbons(X = cr.ae,Y = cr.d18O)+ xlim(c(550,1950))

egg::ggarrange(plots = list(ag,cr),nrow = 2)


co <- corEns(ag.ae,ag.d18O,
             cr.ae,cr.d18O,
             bin.step = 10,
             max.ens = 100,
             gaussianize = FALSE,
             isopersistent = FALSE)

plotCorEns(co,use.fdr = FALSE)
