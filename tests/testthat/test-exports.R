test_that("every geoChronR 1.x export is still available", {
  old <- readLines(test_path("exports-1x.txt"))
  expect_setequal(setdiff(old, getNamespaceExports("geoChronR")), character())
})

test_that("re-exports are the sub-package functions", {
  expect_identical(geoChronR::bin, ens::bin)
  expect_identical(geoChronR::plotChronEns, lipdViz::plotChronEns)
  expect_identical(geoChronR::runBacon, agemodelR::runBacon)
})
