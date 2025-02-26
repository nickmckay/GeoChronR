simulateVarves <- function(n, n.ens = 100, ar1 = NULL, H = NULL, shape = 2, mean = 1) {
  if (!is.null(ar1) && !is.null(H)) {
    stop("Specify either AR(1) coefficient 'ar1' or Hurst parameter 'H', not both.")
  }
  
  varveMat <- matrix(NA,nrow = n, ncol = n.ens)
  
    if (!is.null(ar1)) {
      # AR(1) Process with given ar1
      white_noise <- rnorm(n)  # Gaussian white noise
      ar_series <-  purrr::map(rep(n,times = n.ens),\(x) stats::arima.sim(n = x, list(ar = ar1), innov = white_noise)) |> list_c()  |> matrix(nrow = n, ncol = n.ens,byrow = FALSE)# Simulate fBm
      gamma_series <- gammify(ar_series, shape = shape, mean = mean)  # Convert to Gamma
    } else if (!is.null(H)) {
      # Fractal Brownian Motion (fBM) using fractional differencing
      fbm_series <- purrr::map(rep(n,times = n.ens),\(x) fracdiff::fracdiff.sim(n = x,d = H - 0.5)$series) |> list_c()  |> matrix(nrow = n, ncol = n.ens,byrow = FALSE)# Simulate fBm
      gamma_series <- gammify(fbm_series, shape = shape, mean = mean)  # Convert to Gamma
    } else {
      stop("You must specify either 'ar1' for AR(1) or 'H' for fBM.")
    }

  
  return(gamma_series)
}

gammify <- function (X,shape = 1.5, mean = 1,jitter=FALSE){ 
  #   Transform each column of data matrix X to a gaussian distribution using the inverse Rosenblatt transform.
  #
  # inspired by gaussianize.r and.R, split.m in normal.m by Van Albada, S.J., Robinson P.A. (2006)
  # Transformation of arbitrary distributions to the normal distribution with application to EEG
  # test-retest reliability. J Neurosci Meth, doi:10.1016/j.jneumeth.2006.11.004
  #
  #  Modified by matlab code written 26/06/2015 by Julien Emile-Geay (USC)
  #translated to R and added jitter option 7/06/2017 by Nick McKay (NAU) 
  
  if(!is.matrix(X)){
    X=as.matrix(X)
  }
  p=NCOL(X)
  n=NROW(X) 
  
  if(jitter){
    #add tiny random numbers to avoid ties
    X=array(rnorm(p*n,mean=0,sd=sd(as.vector(X))/1e6),c(n,p))+X
  }
  
  
  Xn    = matrix(0,n,p);
  for (j in 1:p){
    # Sort the data in ascending order and retain permutation indices
    R=rank(X[,j])
    # The cumulative distribution function
    CDF = R/n - 1/(2*n);
    # Apply the inverse Rosenblatt transformation
    Xn[,j] = qgamma(CDF,shape = shape,rate = shape/mean)  # Xn is now gamma distributed
  }
  
  return(Xn)
}
