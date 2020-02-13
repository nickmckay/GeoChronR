context("BChron - full test")
library(geoChronR)

test_that("We can reproduce an age model with BChron", {
  skip_on_cran() #too slow to run on CRAN
  K <- runBchron(Kurupa,calCurves = "normal", iter = 100,which.table = 1,interpolate = T,age14CVar = "age14c",age14CuncertaintyVar = "age14cuncertainty",labIDVar = NULL,reject = F,ageVar = NULL,ageUncertaintyVar = NULL,rejectedAgesVar = NULL,extractDate = 10000) #create a Bchron model
  expect_type(H$chronData[[1]]$model[[1]]$ensembleTable[[1]]$ageEnsemble$values,"double") #check that's it;s numeric
  expect_length(H$chronData[[1]]$model[[1]]$ensembleTable[[1]]$ageEnsemble$values,268000) #and that the ensemble is the right size .
  

})

test_that("We can reproduce an age model with BChron", {
  expect_type(plotChronEns(H,truncateDist = 1e-4),"list") #test plotting
  H = mapAgeEnsembleToPaleoData(H,age.var = "ageEnsemble",which.pmt = 2)
  expect_length(H$ )
  

})