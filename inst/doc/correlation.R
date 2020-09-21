## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup,message=FALSE,warning=FALSE,results='hide'-------------------------
library(geoChronR)
library(ggplot2)
library(gridExtra)

## ----load data----------------------------------------------------------------
loadRemote("http://lipdverse.org/geochronr-examples/introductionOutput.RData",useSavedNames = TRUE)

## ---- echo=FALSE,results='hide',warning = FALSE, message = FALSE,cache = TRUE----
corout <- corEns(time.1 = gisp2.ens,
                 values.1 = gisp2.d18O,
                 time.2 = hulu.ae,
                 values.2 = hulu.d18O,
                 bin.step = 200,
                 max.ens = 100,
                 isopersistent  = TRUE,
                 isospectral = TRUE,
                 gaussianize = TRUE)

## -----------------------------------------------------------------------------
corout$cor.stats

## ----warning=FALSE,message=FALSE,fig.width=6, fig.height=4--------------------
plotCorEns(corout,
           legend.position =c(.85,.8),
           f.sig.lab.position = c(.85,.6),
           significance.option = "raw",
           use.fdr = FALSE)+ggtitle("Distribution of correlation coefficients")

## ----warning=FALSE,message=FALSE,fig.width=6, fig.height=4--------------------
plotCorEns(corout,
           legend.position =c(.85,.8),
           f.sig.lab.position = c(.85,.6),
           significance.option = "eff-n",
           use.fdr = FALSE)+ggtitle("Effective-N significance testing")

## ----warning=FALSE,message=FALSE,fig.width=6, fig.height=4--------------------
plotCorEns(corout,
           legend.position =c(.85,.8),
           f.sig.lab.position = c(.85,.6),
           significance.option = "isopersistent",
           use.fdr = FALSE)+ggtitle("Isopersistent significance testing")

## ----warning=FALSE,message=FALSE,fig.width=6, fig.height=4--------------------
plotCorEns(corout,
           legend.position =c(.85,.8),
           f.sig.lab.position = c(.85,.6),
           significance.option = "isopersistent",
           use.fdr = TRUE)+ggtitle("Isopersistent significance testing, with False Discovery Rate effects")

## ----warning=FALSE,message=FALSE,fig.width=6, fig.height=4--------------------
plotCorEns(corout,
           legend.position =c(.85,.8),
           f.sig.lab.position = c(.85,.6),
           significance.option = "isospectral",
           use.fdr = TRUE)+ggtitle("Isospectral significance testing")

## ----warning=FALSE,message=FALSE,fig.width=6, fig.height=4,results='hide'-----
coroutHulu <- corEns(hulu.ae,hulu.d18O,hulu.ae,hulu.d18O,bin.step = 200,max.ens = 1000)
corPlotHulu <- plotCorEns(coroutHulu,
                          legend.position = c(0.1, 0.8),
                          significance.option = "isospectral")+ggtitle(NULL)

above90 <- round(sum(coroutHulu$cor.df$r > 0.9)/nrow(coroutHulu$cor.df)*100,1)
med <- round(median(coroutHulu$cor.df$r),2)

print(corPlotHulu)


