context("BChron - full test")
library(geoChronR)

test_that("We can reproduce an age model with BChron", {
  L <- runBchron(Kurupa,labIDVar = 'labID', age14CVar = 'age14C', age14CuncertaintyVar = 'age14CUncertainty', ageVar = 'age', ageUncertaintyVar = 'ageUncertainty', depthVar = 'depth', reservoirAge14CVar = NULL, reservoirAge14CUncertaintyVar = NULL, rejectedAgesVar = NULL,iter = 100,burn = 20)
  expect_type(L$chronData[[1]]$model[[1]]$ensembleTable[[1]]$ageEnsemble$values,"double") #check that's it;s numeric
  expect_length(L$chronData[[1]]$model[[1]]$ensembleTable[[1]]$ageEnsemble$values,900) #and that the ensemble is the right size .
  

})

test_that("We can reproduce an age model with BChron", {
  expect_type(plotChronEns(L,truncateDist = 1e-4),"list") #test plotting
  
  L = mapAgeEnsembleToPaleoData(L,age.var = "ageEnsemble")
  expect_length(L$paleoData[[1]]$measurementTable[[1]]$ageEnsemble$values,27864)


})

test_that("We can reproduce an age model with Bacon", {
  L <- runBacon(Kurupa,baconThick = 50,d.by = 10,labIDVar = 'labID', age14CVar = 'age14C', age14CuncertaintyVar = 'age14CUncertainty', ageVar = 'age', ageUncertaintyVar = 'ageUncertainty', depthVar = 'depth', reservoirAge14CVar = NULL, reservoirAge14CUncertaintyVar = NULL, rejectedAgesVar = NULL,baconDir = tempdir(),ask = FALSE,suggest = FALSE)
  expect_type(L$chronData[[1]]$model[[1]]$ensembleTable[[1]]$ageEnsemble$values,"double") #check that's it;s numeric
  expect_length(L$chronData[[1]]$model[[1]]$ensembleTable[[1]]$ageEnsemble$values,12000) #and that the ensemble is the right size .
  
  
})
