# File:      fdr.r 
# Date:      5/10/04
# Version:   0.1.3  
# Author:    Chris Paciorek - please contact the author with bug
#            reports: paciorek AT alumni.cmu.edu
# License:   GPL version 2 or later
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Purpose:   implement False Discovery Rate (FDR) functions for multiple testing, following the Ventura et al. reference below
# Usage:     source('fdr.r');  fdr(my.pvals)
# References:
#             Ventura, V., C.J. Paciorek, and J.S. Risbey.  2004.  Controlling the proportion of falsely-rejected hypotheses when conducting multiple tests with climatological data.  Journal of Climate, in press.  Also Carnegie Mellon University, Department of Statistics technical report 775 (www.stat.cmu.edu/tr/tr775/tr775.html).
#             Benjamini, Y, and Y. Hochberg. 1995. Controlling the false discovery rate: a practical and powerful approach to multiple testing.  JRSSB 57:289-300.
#             Benjamini, Y. and D. Yekutieli.  2001.  The control of the false discovery rate in multiple testing under dependency. Annals of Statistics 29:1165-1188.
#             Benjamini, Y., A. Krieger, and D. Yekutieli.  2001.  Two staged linear step up FDR controlling procedure.  Technical Report, Department of Statistics and Operations Research, Tel Aviv University.  URL: http://www.math.tau.ac.il/~ybenja/Papers.html
#             Storey, J. 2002.  A direct approach to false discovery rates.  JRSSB 64: 479--498.
#

fdr <- function(pvals,qlevel=0.05,method="original",adjustment.method=NULL,adjustment.args=NULL){
#
# Description:
#
#    This is the main function designed for general usage for determining significance based on the FDR approach.
#
# Arguments:
#
#   pvals (required):  a vector of pvals on which to conduct the multiple testing
#
#   qlevel: the proportion of false positives desired
#
#   method: method for performing the testing.  'original' follows Benjamini & Hochberg (1995); 'general' is much more conservative, requiring no assumptions on the p-values (see Benjamini & Yekutieli (2001)).  We recommend using 'original', and if desired, using 'adjustment.method="mean" ' to increase power.
#
#   adjustment.method: method for increasing the power of the procedure by estimating the proportion of alternative p-values, one of "mean", the modified Storey estimator that we suggest in Ventura et al. (2004), "storey", the method of Storey (2002), or "two-stage", the iterative approach of Benjamini et al. (2001)
#
#   adjustment.args: arguments to adjustment.method; see prop.alt() for description, but note that for "two-stage", qlevel and fdr.method are taken from the qlevel and method arguments to fdr()
#
# Value:
#
#   NULL if no significant tests, or a vector of the indices of the significant tests
#
# Examples:
#
#   signif <- fdr(pvals,method="original",adjustment.method="mean")
#
  n <- length(pvals)

  a <- 0   # initialize proportion of alternative hypotheses
  if(!is.null(adjustment.method)){
    if(adjustment.method=="two-stage"){  # set things up for the "two-stage" estimator
      qlevel <- qlevel/(1+qlevel)  # see Benjamini et al. (2001) for proof that this controls the FDR at level qlevel
      adjustment.args$qlevel <- qlevel
      adjustment.args$fdr.method <- method
      cat(paste('Adjusting cutoff using two-stage method, with method ',method,' and qlevel ',round(qlevel,4),'\n',sep=""))
    }
    if(adjustment.method=="mean" & is.null(adjustment.args)){
      adjustment.args <- list(edf.lower=0.8,num.steps=20)  # default arguments for "mean" method of Ventura et al. (2004)
      cat(paste('Adjusting cutoff using mean method, with edf.lower=0.8 and num.steps=20\n',sep=""))
    }
    a <- prop.alt(pvals,adjustment.method,adjustment.args)
  }
  if(a==1){    # all hypotheses are estimated to be alternatives
    return(1:n)
  } else{      # adjust for estimate of a; default is 0
    qlevel <- qlevel/(1-a)
  }
  
  return(fdr.master(pvals,qlevel,method))
}

fdr.master <- function(pvals,qlevel=0.05,method="original"){
#
# Description:
#
#    This is an internal function that performs various versions of the FDR procedure, but without the modification described in section 4 of our J of Climate paper.
#
# Arguments:
#
#   pvals (required):  a vector of pvals on which to conduct the multiple testing
#
#   qlevel: the proportion of false positives desired
#
#   method: one of 'original', the original method of Benjamini & Hochberg (1995), or 'general', the method of Benjamini & Yekutieli (2001), which requires no assumptions about the p-values, but which is much more conservative.  We recommend 'original' for climatological data, and suspect it works well generally for spatial data.
#
# Value:
#
#   NULL if no significant tests, or a vector of the indices of the significant tests
#
  n <- length(pvals)
  if(method=="general"){
    qlevel <- qlevel/sum(1/(1:n))  # This requires fewer assumptions but is much more conservative
  } else{
    if(method!="original"){
      stop(paste("No method of type: ",method,sep=""))
    }
  }
  return(fdr.basic(pvals,qlevel))
}


fdr.basic <- function(pvals,qlevel=0.05){
#
# Description:
#
#    This is an internal function that performs the basic FDR of Benjamini & Hochberg (1995).
#
# Arguments:
#
#   pvals (required):  a vector of pvals on which to conduct the multiple testing
#
#   qlevel: the proportion of false positives desired
#
# Value:
#
#   NULL if no significant tests, or a vector of the indices of the significant tests
#
  n <- length(pvals)
  sorted.pvals <- sort(pvals)
  sort.index <- order(pvals)
  indices <- (1:n)*(sorted.pvals<=qlevel*(1:n)/n)
  num.reject <- max(indices)
  if(num.reject){
    indices <- 1:num.reject
    return(sort(sort.index[indices]))  
  } else{
    return(NULL)
  }
}


storey <- function(edf.quantile,pvals){
#
# Description:
#
#    This is an internal function that calculates the basic Storey (2002) estimator of a, the proportion of alternative hypotheses.
#
# Arguments:
#
#   edf.quantile (required):  the quantile of the empirical distribution function at which to estimate a
# 
#   pvals (required):  a vector of pvals on which to estimate a
#
# Value:
#
#   estimate of a, the number of alternative hypotheses
#
#
  if(edf.quantile >=1 | edf.quantile <=0){
    stop('edf.quantile should be between 0 and 1')
  }
  a <- (mean(pvals<=edf.quantile)-edf.quantile)/(1-edf.quantile)
  if(a>0){
    return(a)
  } else{
    return(0)
  }
}


prop.alt <- function(pvals,adjustment.method="mean",adjustment.args=list(edf.lower=0.8,num.steps=20)){
#
# Description:
#
#    This is an internal function that calculates an estimate of a, the proportion of alternative hypotheses, using one of several methods.
#
# Arguments:
#
#   pvals (required):  a vector of pvals from which to estimate a
#
#   adjustment.method: method for  estimating the proportion of alternative p-values, one of "mean", the modified Storey estimator suggested in Ventura et al. (2004); "storey", the method of Storey (2002); or "two-stage", the iterative approach of Benjamini et al. (2001)
#
#   adjustment.args: arguments to adjustment.method;
#      for "mean", specify edf.lower, the smallest quantile at which to estimate a, and num.steps, the number of quantiles to use - the approach uses the average of the Storey (2002) estimator for the num.steps quantiles starting at edf.lower and finishing just less than 1
#      for "storey", specify edf.quantile, the quantile at which to calculate the estimator
#      for "two-stage", the method uses a standard FDR approach to estimate which p-values are significant; this number is the estimate of a; therefore the method requires specification of qlevel, the proportion of false positives and fdr.method ('original' or 'general'), the FDR method to be used.  We do not recommend 'general' as this is very conservative and will underestimate a.
#  
# Value:
#
#   estimate of a, the number of alternative hypotheses
#
# Examples:
#
#   a <- prop.alt(pvals,adjustment.method="mean")
#
  n <- length(pvals)
  if(adjustment.method=="two-stage"){
    if(is.null(adjustment.args$qlevel) | is.null(adjustment.args$fdr.method)){
      stop("adjustment.args$qlevel or adjustment.args$fdr.method not specified.  Two-stage estimation of the number of alternative hypotheses requires specification of the FDR threshold and FDR method ('original' or 'general')")
    }
    return(length(fdr.master(pvals,adjustment.args$qlevel,method=adjustment.args$fdr.method))/n)
  }

  if(adjustment.method=="storey"){
    if(is.null(adjustment.args$edf.quantile)){
      stop("adjustment.args$edf.quantile not specified. Using Storey's method for estimating  the number of alternative hypotheses requires specification of the argument of the p-value EDF at which to do the estimation (a number close to one is recommended)")
    }
    return(storey(adjustment.args$edf.quantile,pvals))
  }

  if(adjustment.method=="mean"){
    if(is.null(adjustment.args$edf.lower) | is.null(adjustment.args$num.steps)){
      stop("adjustment.args$edf.lower or adjustment.args$num.steps is not specified. Using the method of Ventura et al. (2004) for estimating  the number of alternative hypotheses requires specification of the lowest quantile of the p-value EDF at which to do the estimation (a number close to one is recommended) and the number of steps between edf.lower and 1, starting at edf.lower, at which to do the estimation")
    }
    if(adjustment.args$edf.lower >=1 | adjustment.args$edf.lower<=0){
      stop("adjustment.args$edf.lower must be between 0 and 1");
    }
    if(adjustment.args$num.steps<1 | adjustment.args$num.steps%%1!=0){
      stop("adjustment.args$num.steps must be an integer greater than 0")
    }
    stepsize <- (1-adjustment.args$edf.lower)/adjustment.args$num.steps
    edf.quantiles <- matrix(seq(from=adjustment.args$edf.lower,by=stepsize,len=adjustment.args$num.steps),nr=adjustment.args$num.steps,nc=1)
    a.vec <- apply(edf.quantiles,1,storey,pvals)
    return(mean(a.vec))
  }
}

