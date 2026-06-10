library(geoChronR)

# Golden outputs captured from geoChronR 1.1.17 (pre-refactor) by
# tools/capture_reference.R. These lock in the exact numerical behavior of
# bin/binEns/corMatrix/corEns through the performance refactor.
ref <- readRDS(test_path("reference_outputs.rds"))

makeTestData <- function() {
  set.seed(42)
  nobs <- 80
  n.ens <- 12
  out <- list()
  out$time1 <- matrix(sort(runif(nobs, 0, 1000)) + rnorm(nobs * n.ens, sd = 5),
                      nrow = nobs, ncol = n.ens)
  out$vals1 <- as.matrix(cumsum(rnorm(nobs)))
  out$time2 <- matrix(sort(runif(nobs, 0, 1000)) + rnorm(nobs * n.ens, sd = 5),
                      nrow = nobs, ncol = n.ens)
  out$vals2 <- as.matrix(cumsum(rnorm(nobs)))
  out$vals1[c(5, 17, 44)] <- NA
  return(out)
}

test_that("bin works properly", {
  tv <- seq(1, 100)
  tvn <- tv
  tvn[seq(1, 100, by = 2)] <- NA

  expect_equal(bin(tv, tvn, seq(6, 95, by = 11))$y[5], 56)
  expect_equal(bin(tv, tvn, seq(6, 95, by = 11), bin.fun = sum)$y[5], 280)
  expect_equal(bin(tv, tvn, seq(6, 95, by = 11), bin.fun = median)$y[5], 56)
  expect_equal(bin(rev(tv), tvn, seq(6, 95, by = 11))$y[4], 56)
})

test_that("bin matches pre-refactor reference output", {
  tv <- seq(1, 100)
  tvn <- tv
  tvn[seq(1, 100, by = 2)] <- NA

  expect_equal(bin(tv, tvn, seq(6, 95, by = 11)), ref$bin_asc)
  expect_equal(bin(tv, tvn, seq(6, 95, by = 11), bin.fun = sum), ref$bin_sum)
  expect_equal(bin(tv, tvn, seq(6, 95, by = 11), bin.fun = median), ref$bin_median)
  expect_equal(bin(rev(tv), tvn, seq(6, 95, by = 11)), ref$bin_revtime)
  expect_equal(bin(tv, tvn, seq(95, 6, by = -11)), ref$bin_desc)
  expect_equal(bin(tv, tvn, c(0, 10, 200, 300)), ref$bin_empty)
  expect_equal(bin(tv, tvn, c(0, 10, 200, 300), bin.fun = sum), ref$bin_empty_sum)
})

test_that("binEns matches pre-refactor reference output", {
  d <- makeTestData()
  bv <- seq(0, 1000, by = 50)

  expect_equal(binEns(d$time1, d$vals1, bin.vec = bv), ref$binEns_age)
  expect_equal(binEns(d$time1[, 1], cbind(d$vals2, d$vals2 + 1), bin.vec = bv),
               ref$binEns_val)
  set.seed(7)
  expect_equal(binEns(d$time1, cbind(d$vals2, d$vals2 + 1, d$vals2 - 1), bin.vec = bv),
               ref$binEns_both)
  expect_equal(binEns(d$time1, d$vals1, bin.vec = bv, max.ens = 5), ref$binEns_max)
})

test_that("corMatrix matches pre-refactor reference output", {
  d <- makeTestData()
  bv <- seq(0, 1000, by = 50)
  set.seed(11)
  b1 <- binEns(d$time1, d$vals1, bin.vec = bv)$matrix
  b2 <- binEns(d$time2, d$vals2, bin.vec = bv)$matrix

  expect_equal(corMatrix(b1, b2, isospectral = FALSE, isopersistent = FALSE),
               ref$corMatrix)
  expect_equal(corMatrix(b1, b2, isospectral = FALSE, isopersistent = FALSE,
                         max.ens = 20),
               ref$corMatrix_maxens)
  expect_equal(corMatrix(b1, b2, isospectral = FALSE, isopersistent = FALSE,
                         cor.method = "spearman"),
               ref$corMatrix_spearman)
})

test_that("corEns matches pre-refactor reference output", {
  d <- makeTestData()
  set.seed(13)
  out <- corEns(d$time1, d$vals1, d$time2, d$vals2, bin.step = 50,
                isospectral = FALSE, isopersistent = FALSE, max.ens = 100)
  expect_equal(out, ref$corEns)
})

test_that("corMatrix Monte Carlo p-values are sane", {
  d <- makeTestData()
  bv <- seq(0, 1000, by = 50)
  b1 <- binEns(d$time1, d$vals1, bin.vec = bv, max.ens = 3)$matrix
  b2 <- binEns(d$time2, d$vals2, bin.vec = bv, max.ens = 3)$matrix
  set.seed(99)
  out <- corMatrix(b1, b2, isospectral = TRUE, isopersistent = TRUE, p.ens = 50)
  expect_true(all(c("r", "pSerial", "pRaw", "pIsopersistent", "pIsospectral")
                  %in% names(out)))
  expect_true(all(out$pIsospectral >= 0 & out$pIsospectral <= 1, na.rm = TRUE))
  expect_true(all(out$pIsopersistent >= 0 & out$pIsopersistent <= 1, na.rm = TRUE))
})
