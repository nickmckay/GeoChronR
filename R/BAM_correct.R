BAM_correct <- function(X, t, model=NULL){
  
  # Generate an ensemble of possible age corrected data:See www.clim-past-discuss.net/9/6077/2013/ for a detailed description of the model.
  # The time series in X are automatically flipped to range from most recent to oldest measurements when the intput t is given in increasing order. 
  #
  # res <- BAM_correct(X,t) will generate an ensemble of 1000 age models randomly following
  # a Poisson process with rate parameter theta=0.05 used to perturb data X
  #
  # res <- BAM_correct(X,t,model) will correct data X  with the model specified in
  # the model structure
  #
  # INPUT
  # X: data (vector or matrix n*p)
  # t: chronology for data X (n*1)
  # model$ns: number of samples
  # model$name: 'poisson' or 'bernoulli'
  # model$param: probability of growth band being perturbed (default: prob of missing band = prob of doubly-counted band = 0.05)
  #      if model$param is a single argument, then the perturbations are symmetric (prob of missing band = prob of doubly-counted band)
  #      if model$param = [a1 a2] and a1 neq a2 the model is asymmetric
  #                       a1 = prob(missing layer)
  #                       a2 = prob(layer counted multiple times)
  #      if model$param: 2xp matrix, then different miscounting prob. are defined for each time series. 
  # model$resize: do not resize: 0 (default), resize to shortest sample: -1, resize to longest sample: 1
  # model$tm: if a time model is provided, the code returns the corresponding corrected data
  
  # OUTPUT
  # res$Xc: realizations of age-perturbed data matrix of size tn*p*ns (could be 2 or 3d)
  # res$tc: new chronology tn*1
  # res$tmc: corresponding ensemble of time-correction matrices (tn*p*ns) to map realizations in Xp back to the original data X (2=insert nan, 0=remove double band) (2 or 3d)
  # where tn is the chronology length = n (default), shortest sample or longest sample
  # depending on the chosen resizing option.
  
  
  # transpose X if time is not the first dimension
  X <- as.array(X)
  if (dim(X)[1] < dim(X)[2] && is.finite(dim(X)[3])) 
    X <- aperm(X,c(2,1,3)) 
  else
    if (dim(X)[1] < dim(X)[2] && !is.finite(dim(X)[3]))
      X <- t(X)
  
  # reverse time if not from old to new
  if (mean(diff(t),na.rm = T)>0) {
    isflipped <- 1
    t <- rev(t)
    if (is.finite(dim(X)[3])){
      for (ii in 1:dim(X)[3])
        X[,,ii] <- apply(X[,,ii],2,rev)
    }
    else
      X <- apply(X,2,rev)
  } else {
    isflipped <- 0
  }
  
  n <- dim(X)[1]
  p <- dim(X)[2]
  # set default values
  if (is.null(model)) 
    model <- list(name='poisson',param=array(0.05,c(n,p,2)),ns=1000,resize=0)	
   
  # "ns" %in% names(model)
  
  if (is.null(model[["ns"]]))
    model$ns <- 1000
  
  if (is.null(model[["name"]]))
    model$name <- 'poisson'
  
  if (is.null(model[["param"]]))
    model$param <- array(0.05,c(n,p,2))
  
  if (is.null(model[["resize"]]))
    model$resize <- 0
  
  if (is.null(dim(model$param)[3])){
    model$param<-as.matrix(model$param)
    if (dim(model$param)[1] < dim(model$param)[2])
      model$param = t(model$param)
    
    p1 <- dim(model$param)[1]
    p2 <- dim(model$param)[2]
    
    if (p1==1)                           # case where a single argument was entered
      model$param <- array(model$param, dim=c(n,p,2))
    else {
      if (p1==2){                         # case where 2 values were given
        param1 <- matrix(model$param[1],n,p);
        param2 <- matrix(model$param[2],n,p);     
        model$param <- array(c(param1,param2), c(n,p,2))
      }
      else {
        if (p1==p && p2==2) {             # case where 2 vectors were given
          param1 <- matrix(t(model$param[,1]),n,p);
          param2 <- matrix(t(model$param[,2]),n,p);
          model$param <- array(c(param1,param2), c(n,p,2))      
        }
      }
    }
  }
  # Generate an ensemble of time perturbation models
  if (is.null(model[["tm"]])){
    ns <- model$ns
    tmc <- array(1,dim=c(n,p,ns))
    X <- array(X,c(n,p,ns));
    
    if (grepl("poisson",model$name)){     
      for(ii in 1:p){
        for(jj in 2:n){
          num_event_mis <-rpois(1,model$param[jj,ii,1]*ns)
          num_event_dbl <-rpois(1,model$param[jj,ii,2]*ns)
          
          jumps <- sample(1:ns, num_event_mis, replace = FALSE, prob = NULL) #place events uniformly on {1,...,ns}
          model$tm[jj,ii,jumps] = model$tm[jj,ii,jumps] + 1                 #remove 1 at jump locations
          jumps <- sample(1:ns, num_event_dbl, replace = FALSE, prob = NULL)
          model$tm[jj,ii,jumps] = model$tm[jj,ii,jumps] - 1
          
        }
      }
    }
    else{
      if (grepl("bernoulli",model$name)){
        
          for (ii in 1:p){
            for (jj in 2:n){
            model$tm[jj,ii,] = model$tm[jj,ii,] + rbinom(ns,1,model$param[jj,ii,1])
            model$tm[jj,ii,] = model$tm[jj,ii,] - rbinom(ns,1,model$param[jj,ii,2])
          }
        }
      }
      else print("Unknown age model ; acceptable inputs are ''poisson'' and ''bernoulli'' ")
    }
  }
  else {
    tmc <- model$tm;
    ns <- dim(model$tm)[3];
    if (isflipped ==1)
      for (nn in 1:ns)
        tmc[,,nn] <- apply(tmc[,,nn],2,rev)    
  }
  # generate age perturbed data Xp
  # expand length of Xp and tXp if resizing is required
  if (model$resize == 1){
    t_ext <- ceiling(2*max(model$param)*n)
    tn <- n + t_ext
    X <- abind(X, array(NaN,c(t_ext,p,ns)),along=1)  
    dt <- t[2]-t[1]
    time_ext <- seq(tail(t,1)+dt,tail(t,1)+t_ext*dt,by=dt)
    tc <- c(t,time_ext)
  }
  else{
    tn <- n
    tc <- t
  }
  
  Xc <- array(NaN,dim = c(tn,p,ns))
  Tmax <- 0
  Tmin <- n
  for (nn in 1:ns) {
    
    for (ii in 1:p) {
      xcount <- 1
      Xcount <- 1
      tt <- 1
      while (tt < n+1) {
        
        if (tmc[tt,ii,nn] == 0){     # remove band
          Xcount <- min(Xcount+1,tn)
          Xc[xcount,ii,nn] <- X[min(tn,Xcount),ii,nn]
          tt <- tt+1
        }
        else{
          if (tmc[tt,ii,nn] == 2){   # insert Nan
            Xc[xcount,ii,nn] <- NaN
            xcount <- min(tn,xcount+1)
            Xc[xcount,ii,nn] <- X[Xcount,ii,nn]
          }
          else{
            Xc[xcount,ii,nn] <- X[Xcount,ii,nn]
          }
        }
        xcount <- min(tn,xcount+1)
        Xcount <- min(tn,Xcount+1)
        tt <- tt+1
      }
      kall <- which(!is.nan(Xc[,ii,nn]), arr.ind=T)
      k <- tail(kall,1)
      if (k > Tmax) Tmax <- k
      if (k < Tmin) Tmin <- k
    }
  }
  
  if (model$resize == -1){
    Xc <- Xc[1:Tmin,,]
    tc <- tc[1:Tmin]
  }
  # expand output size to longest (non-nan) sequence
  if (model$resize == 1){
    Xc <- Xc[1:Tmax,,];
    tc <- tc[1:Tmax];
  }
    
  if (dim(X)[2]==1) {
    Xc  <- array(Xc,c(n,model$ns))
    tmc <- array(tmc,c(n,model$ns))
  }
  
  if (isflipped == 1){
    if (dim(X)[2]==1)
      Xc = apply(Xc,2,rev)
    else
      for (nn in 1:ns) {
        Xc[,,nn] = apply(Xc[,,nn],2,rev)
        tmc[,,nn] = apply(tmc[,,nn],2,rev)
      }
    
    tc = rev(tc);
  }
  res <- list("Xc"=Xc,"tc"=tc,"tmc"=tmc)
  return(res)
}