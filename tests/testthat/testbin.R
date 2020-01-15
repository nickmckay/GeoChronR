context("Binning")
library(geoChronR)

test_that("bin works properly", {
  tv <- seq(1,100)
  tvn <- tv
  tvn[seq(1,100,by = 2)] <- NA
  
  expect_equal(bin(tv,tvn,seq(6,95,by = 11))$y[5], 56)#check regular use
  expect_equal(bin(tv,tvn,seq(6,95,by = 11),binfun = sum)$y[5], 280)#sum
  expect_equal(bin(tv,tvn,seq(6,95,by = 11),binfun = median)$y[5], 56)#median
  expect_equal(bin(rev(tv),tvn,seq(6,95,by = 11))$y[4], 56)#reverse time
  
  
})

