library(lipdR)
library(tictoc)
library(oxcAAR)
oxcAAR::setOxcalExecutablePath("/Applications/OxCal/bin/OxCalMac")


#L <- lipdR::readLipd("~/Dropbox/HoloceneLiPDLibrary/masterDatabase/Hallet.McKay.2009.lpd")
#L <- lipdR::readLipd("http://wiki.linked.earth/wiki/index.php/Special:WTLiPD?op=export&lipdid=MD01-2378.Xu.2008")


L <- readLipd("~/Downloads/BJ8-03-70GGC.Linsley.2010.lpd")
labIDVar = 'id', age14CVar = 'age14c', age14CuncertaintyVar = 'age14cuncertainty', ageVar = 'calendarage', ageUncertaintyVar = 'calendarageuncertainty', depthVar = 'depth', reservoirAge14CVar = NULL, reservoirAge14CUncertaintyVar = NULL, rejectedAgesVar = NULL)
L <- readLipd("~/Downloads/BJ8-03-13GGC.Linsley.2010 (2).lpd")
labIDVar = 'id', age14CVar = 'age14c', age14CuncertaintyVar = 'age14cuncertainty', ageVar = 'calendarage', ageUncertaintyVar = 'calendarageuncertainty', depthVar = 'depth', reservoirAge14CVar = NULL, reservoirAge14CUncertaintyVar = NULL, rejectedAgesVar = NULL)


L <- readLipd("~/Downloads/forDeborah/GeoB10069-3.Gibbons.2014.lpd")
cdf <- createChronMeasInputDf(L,labIDVar = NULL, age14CVar = 'age14c', age14CuncertaintyVar = 'age14c uncertainty', ageVar = NULL, ageUncertaintyVar = NULL, depthVar = 'depth', reservoirAge14CVar = NULL, reservoirAge14CUncertaintyVar = NULL, rejectedAgesVar = NULL)


#cdf <- createChronMeasInputDf(L)


#assign in reservoir ages?
cdf$reservoirAge <- 70
cdf$reservoirAgeUnc <- 50

#add surface age estimate?
sage <- data.frame(labID = "surface",
                   age = 0,
                   ageUnc = 25,
                   depth = 0)
cdf <- dplyr::bind_rows(sage,cdf)

       #                       ,labIDVar = 'labID', age14CVar = 'age', age14CuncertaintyVar = 'uncEstimate', ageVar = 'age', ageUncertaintyVar = NULL, depthVar = 'depth', reservoirAge14CVar = NULL, reservoirAge14CUncertaintyVar = NULL, rejectedAgesVar = NULL) 


#estimate

oxMod <- createOxcalModel(cdf,
                          depthInterval = .20,
                          eventsPerUnitLength = 10,
                          eventsPerUnitLengthUncertainty = 0,
                          outlierProb = .05,
                          calCurve = "marine")
tic("oxcal")
oxcalResultFilePath <- oxcAAR::executeOxcalScript(oxMod$modelText)
toc()
L <- loadOxcalOutput(L,oxcalResultFilePath,oxMod,modelNum = 1,makeNew = T)
plotChronEns(L)
