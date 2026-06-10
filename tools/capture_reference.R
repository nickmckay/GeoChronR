# Capture golden reference outputs from the CURRENT implementation of
# bin/binEns/corMatrix/corEns before refactoring. Run from package root.
devtools::load_all(".", quiet = TRUE)

set.seed(42)
nobs <- 80
n.ens <- 12
time1 <- matrix(sort(runif(nobs, 0, 1000)) + rnorm(nobs * n.ens, sd = 5),
                nrow = nobs, ncol = n.ens)
vals1 <- as.matrix(cumsum(rnorm(nobs)))
time2 <- matrix(sort(runif(nobs, 0, 1000)) + rnorm(nobs * n.ens, sd = 5),
                nrow = nobs, ncol = n.ens)
vals2 <- as.matrix(cumsum(rnorm(nobs)))
vals1[c(5, 17, 44)] <- NA

ref <- list()

# bin(): ascending, descending, sum/median, empty bins
tv <- seq(1, 100)
tvn <- tv; tvn[seq(1, 100, by = 2)] <- NA
ref$bin_asc    <- bin(tv, tvn, seq(6, 95, by = 11))
ref$bin_sum    <- bin(tv, tvn, seq(6, 95, by = 11), bin.fun = sum)
ref$bin_median <- bin(tv, tvn, seq(6, 95, by = 11), bin.fun = median)
ref$bin_revtime<- bin(rev(tv), tvn, seq(6, 95, by = 11))
ref$bin_desc   <- bin(tv, tvn, seq(95, 6, by = -11))            # descending bin.vec
ref$bin_empty  <- bin(tv, tvn, c(0, 10, 200, 300))               # empty bin (200,300]
ref$bin_empty_sum <- bin(tv, tvn, c(0, 10, 200, 300), bin.fun = sum)

# binEns(): age ensemble, value ensemble, both
ref$binEns_age  <- binEns(time1, vals1, bin.vec = seq(0, 1000, by = 50))
ref$binEns_val  <- binEns(time1[, 1], cbind(vals2, vals2 + 1), bin.vec = seq(0, 1000, by = 50))
set.seed(7)
ref$binEns_both <- binEns(time1, cbind(vals2, vals2 + 1, vals2 - 1),
                          bin.vec = seq(0, 1000, by = 50))
ref$binEns_max  <- binEns(time1, vals1, bin.vec = seq(0, 1000, by = 50), max.ens = 5)

# corMatrix(): deterministic parts (r, pSerial, pRaw) without MC
set.seed(11)
b1 <- binEns(time1, vals1, bin.vec = seq(0, 1000, by = 50))$matrix
b2 <- binEns(time2, vals2, bin.vec = seq(0, 1000, by = 50))$matrix
ref$corMatrix <- corMatrix(b1, b2, isospectral = FALSE, isopersistent = FALSE)
ref$corMatrix_maxens <- corMatrix(b1, b2, isospectral = FALSE,
                                  isopersistent = FALSE, max.ens = 20)
ref$corMatrix_spearman <- corMatrix(b1, b2, isospectral = FALSE,
                                    isopersistent = FALSE, cor.method = "spearman")

# corEns() end-to-end, deterministic parts only
set.seed(13)
ref$corEns <- corEns(time1, vals1, time2, vals2, bin.step = 50,
                     isospectral = FALSE, isopersistent = FALSE, max.ens = 100)

saveRDS(ref, "tools/reference_outputs.rds")
cat("Saved", length(ref), "reference objects\n")
