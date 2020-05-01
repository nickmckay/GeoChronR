L <- readLipd("~/Dropbox/HoloceneLiPDLibrary/masterDatabase/Hallet.McKay.2009.lpd")
cdf <- createChronMeasInputDf(L,labIDVar = 'lab_id', age14CVar = 'age', age14CuncertaintyVar = 'SD', ageVar = NULL, ageUncertaintyVar = NULL, depthVar = 'depth_bottom', reservoirAge14CVar = NULL, reservoirAge14CUncertaintyVar = NULL, rejectedAgesVar = NULL)


oxMod <- createOxcalModel(cdf,depthInterval = 100,eventsPerUnitLength = .01)

oxcalResultFilePath <- oxcAAR::executeOxcalScript(oxMod$modelText)