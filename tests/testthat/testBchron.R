context("BChron - full test")
library(geoChronR)

test_that("We can reproduce an age model with BChron", {
  skip_on_cran() #too slow to run on CRAN
  H <- runBchron(hulu,calCurves = "normal", iter = 10000,which.table = 2,interpolate = T,age14CVar = "age",age14CuncertaintyVar = "ageUncertaintyHigh",labIDVar = NULL,reject = F,ageVar = NULL,ageUncertaintyVar = NULL,rejectedAgesVar = NULL,extractDate = 10000) #create a Bchron model
  expect_type(H$chronData[[1]]$model[[1]]$ensembleTable[[1]]$ageEnsemble$values,"double") #check that's it;s numeric
  expect_length(H$chronData[[1]]$model[[1]]$ensembleTable[[1]]$ageEnsemble$values,268000) #and that the ensemble is the right size .

})