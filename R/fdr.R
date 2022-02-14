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

#
#' @export
#' @author Chris Paciorek
#' @title False Discovery Testing of p-values from multiple testing
#' @family FDR
#' @description iThis is the main function designed for general usage for determining significance based on the FDR approach, following the Ventura et al. reference below.
#'
#' @param pvals a vector of pvals on which to conduct the multiple testing
#' @param qlevel the proportion of false positives desired
#' @param method method for performing the testing.  'original' follows Benjamini & Hochberg (1995); 'general' is much more conservative, requiring no assumptions on the p-values (see Benjamini & Yekutieli (2001)).  We recommend using 'original', and if desired, using 'adjustment.method="mean" ' to increase power
#' @param adjustment.method method for increasing the power of the procedure by estimating the proportion of alternative p-values, one of "mean", the modified Storey estimator that we suggest in Ventura et al. (2004), "storey", the method of Storey (2002), or "two-stage", the iterative approach of Benjamini et al. (2001)
#' @param verbose Be verbose (TRUE or FALSE)
#' @param adjustment.args arguments to adjustment.method; see propAlt() for description, but note that for "two-stage", qlevel and fdr.method are taken from the qlevel and method arguments to fdr()
#' @references Ventura, V., C.J. Paciorek, and J.S. Risbey.  2004.  Controlling the proportion of falsely-rejected hypotheses when conducting multiple tests with climatological data.  Journal of Climate, in press.  Also Carnegie Mellon University, Department of Statistics technical report 775 (\url{www.stat.cmu.edu/tr/tr775/tr775.html}).
#' @references  Benjamini, Y, and Y. Hochberg. 1995. Controlling the false discovery rate: a practical and powerful approach to multiple testing.  JRSSB 57:289-300.
#' @references Benjamini, Y. and D. Yekutieli.  2001.  The control of the false discovery rate in multiple testing under dependency. Annals of Statistics 29:1165-1188.
#' @references Benjamini, Y., A. Krieger, and D. Yekutieli.  2001.  Two staged linear step up FDR controlling procedure.  Technical Report, Department of Statistics and Operations Research, Tel Aviv University.  URL: \url{http://www.math.tau.ac.il/~ybenja/Papers.html}
#' @references Storey, J. 2002.  A direct approach to false discovery rates.  JRSSB 64: 479--498.
#' @return NULL if no significant tests, or a vector of the indices of the significant tests
#' @examples
#' \dontrun{
#' signif <- fdr(pvals,method="original",adjustment.method="mean")
#' }
fdr <- function(pvals,qlevel=0.05,method="original",adjustment.method=NULL,adjustment.args=NULL,verbose = FALSE){
  pvals <- pvals[is.finite(pvals)]
  
  n <- length(pvals)
  if(length(n) == 0){
    stop("No p-values were passed to FDR")
  }

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
      if(verbose){
      cat(paste('Adjusting cutoff using mean method, with edf.lower=0.8 and num.steps=20\n',sep=""))
      }
    }
    a <- propAlt(pvals,adjustment.method,adjustment.args)
  }
  if(a==1){    # all hypotheses are estimated to be alternatives
    return(which(pvals<qlevel))
  } else{      # adjust for estimate of a; default is 0
    qlevel <- qlevel/(1-a)
  }
  
  return(fdr.master(pvals,qlevel,method))
}

#' @export
#' @author Chris Paciorek
#' @title FDR Master
#' @description This is an internal function that performs various versions of the FDR procedure, but without the modification described in section 4 of our J of Climate paper.
#' @family FDR
#' @param pvals (required):  a vector of pvals on which to conduct the multiple testing
#' @param qlevel the proportion of false positives desired
#' @param method one of 'original', the original method of Benjamini & Hochberg (1995), or 'general', the method of Benjamini & Yekutieli (2001), which requires no assumptions about the p-values, but which is much more conservative.  We recommend 'original' for climatological data, and suspect it works well generally for spatial data.
#' @return  NULL if no significant tests, or a vector of the indices of the significant tests

fdr.master <- function(pvals,qlevel=0.05,method="original"){

  n <- length(pvals)
  if(method=="general"){
    qlevel <- qlevel/sum(1/(1:n))  # This requires fewer assumptions but is much more conservative
  } else{
    if(method!="original"){
      stop(paste("No method of type: ",method,sep=""))
    }
  }
  return(fdrBasic(pvals,qlevel))
}

#' @export
#' @author Chris Paciorek
#' @title FDR Basic
#' @description This is an internal function that performs the basic FDR of Benjamini & Hochberg (1995).
#' @family FDR
#' @param pvals (required):  a vector of pvals on which to conduct the multiple testing
#' @param qlevel the proportion of false positives desired
#' @return  NULL if no significant tests, or a vector of the indices of the significant tests

fdrBasic <- function(pvals,qlevel=0.05){

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

#' @export
#' @author Chris Paciorek
#' @title Storey estimator
#' @description   This is an internal function that calculates the basic Storey (2002) estimator of a, the proportion of alternative hypotheses.
#' @family FDR
#' @param edf.quantile (required):  the quantile of the empirical distribution function at which to estimate a
#' @param pvals (required):  a vector of pvals on which to conduct the multiple testing
#' @return  estimate of a, the number of alternative hypotheses
storey <- function(edf.quantile,pvals){

  if(edf.quantile >=1 | edf.quantile <=0){
    stop('edf.quantile should be between 0 and 1')
  }
  a <- (mean(pvals<=edf.quantile, na.rm =TRUE)-edf.quantile)/(1-edf.quantile)

  if(a>0){
    return(a)
  } else{
    return(0)
  }
}

#' @export
#' @author Chris Paciorek
#' @title Proportion of alternate hypotheses
#' @description  This is an internal function that calculates an estimate of a, the proportion of alternative hypotheses, using one of several methods.
#' @family FDR
#' @param pvals a vector of pvals on which to conduct the multiple testing
#' @param adjustment.method method for increasing the power of the procedure by estimating the proportion of alternative p-values, one of "mean", the modified Storey estimator that we suggest in Ventura et al. (2004), "storey", the method of Storey (2002), or "two-stage", the iterative approach of Benjamini et al. (2001)
#' @param adjustment.args arguments to adjustment.method; see propAlt() for description, but note that for "two-stage", qlevel and fdr.method are taken from the qlevel and method arguments to fdr()
#' @return  estimate of a, the number of alternative hypotheses
#' @examples 
#' \dontrun{
#' a <- propAlt(pvals,adjustment.method="mean")
#' }
propAlt <- function(pvals,adjustment.method="mean",adjustment.args=list(edf.lower=0.8,num.steps=20)){

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

