#' @export
#' @family BAM
#' @author Nick McKay
#' @author Maud Comboul (BAM)
#' @title Generate a Banded Age Model (BAM) and add it into a LiPD object
#' @description This is a high-level function that uses BAM to simulate age uncertainty in layer counted records, and stores this as an age-ensemble in a paleoData measurementTable, and in a model in chronData. If needed input variables are not entered, and cannot be deduced, it will run in interactive mode. BAM produces reasonable results for non-layer counted data, and can generate ensembles for unevenly spaced data, and thus is useful for generating ensembles for tie-point chronologies that are missing the necessary data to calculate ensembles properly. See Comboul et al. (2015) doi:10.5194/cp-10-825-2014 for details.
#' @inheritParams selectData
#' @param chron.num the number of the chronData object that you'll be working in
#' @param paleo.num the number of the paleoData object that you'll be working in
#' @param paleo.meas.table.num the number of the measurementTable you'll be working in
#' @param model.num the number of the chronData model where you want to store the model information
#' @param ens.table.number the number of the ensembleTable you want to put the output into
#' @param make.new Forces the creation of a new model (TRUE or FALSE{default})
#' @param n.ens The number of members in the ensemble
#' @param model a list that describes the model to use in BAM
#' \itemize{
#' \item model$ns: number of samples 
#' \item model$name: 'poisson' or 'bernoulli' 
#' \item model$param: probability of growth band being perturbed (default: prob of missing band = prob of doubly-counted band = 0.05)
#' \itemize{
#'      \item if model$param is a single argument, then the perturbations are symmetric (prob of missing band = prob of doubly-counted band)
#'      \item if model$param = [a1 a2] and a1 neq a2 the model is asymmetric
#'      \itemize{
#'                  \item a1 = prob(missing layer) - undercounted
#'                  \item a2 = prob(layer counted multiple times) - overcounted
#'                  }
#'      \item if model$param: 2xp matrix, then different miscounting prob. are defined for each time series.
#'      }
#' \item model$resize: do not resize: 0 (default), resize to shortest sample: -1, resize to longest sample: 1
#' \item model$tm: if a time model is provided, the code returns the corresponding perturbed data
#' }
#' @return L The single LiPD object that was entered, with ageEnsemble and chronData model added.
#' @examples 
#' \dontrun{
#' #Run in interactive mode:
#' L = runBam(L)
#' 
#' #Run in noninteractive mode, describing everything:
#' L = runBam(L,paleo.num = 1, paleo.meas.table.num = 1, model.num = 3, make.new = TRUE,
#' nEns = 100, model = list(name = "poisson",param = 0.05, resize = 0, ns = n.ens))
#' }
#' @section Long-form example:
#' \href{http://nickmckay.github.io/GeoChronR/articles/Introduction.html}{View a full-fledged example of how to use this function.}



runBam <- function(L,
                   paleo.num=NA,
                   paleo.meas.table.num=NA,
                   chron.num=1,
                   model.num=NA,
                   ens.table.number = NA,
                   make.new=FALSE,
                   n.ens = 1000,
                   model = NA){
  
  #initialize paleo.num
  if(is.na(paleo.num)){
    if(length(L$paleoData)==1){
      paleo.num=1
    }else{
      paleo.num=as.integer(readline(prompt = "Which paleoData do you want to put this age ensemble in? "))
    }
  }
  
  #initialize measurement table number
  if(is.na(paleo.meas.table.num)){
    if(length(L$paleoData[[paleo.num]]$measurementTable)==1){
      #only one pmt
      paleo.meas.table.num=1
    }else{
      print(paste("PaleoData", paleo.num, "has", length(L$paleoData[[paleo.num]]$measurementTable), "measurement tables"))
      paleo.meas.table.num=as.integer(readline(prompt = "Which measurement table do you want to put the ensemble in? Enter an integer "))
    }
  }
  
  #Which age/year vector do you want to perturb?
  yearData = selectData(L,var.name = "year",alt.names = "age", paleo.or.chron.num = paleo.num, meas.table.num=paleo.meas.table.num,always.choose = FALSE)
  
  
  #make sure that the most recent year is first
  if(grepl(pattern = "CE",yearData$units,ignore.case = TRUE) | grepl(pattern = "AD",yearData$units,ignore.case = TRUE)){
    #then its in calendar years
    calYear=TRUE
  }else if(grepl(pattern = "ka",yearData$units,ignore.case = TRUE) | grepl(pattern = "BP",yearData$units,ignore.case = TRUE) ){
    #then its BP
    calYear=FALSE
    
  }else{
    #then we have to ask
    answer = as.integer(readline(prompt = "Are the time data in Years (AD or CE) (enter 1) or in Age (BP or ka) (enter 2) ?"))
    if(answer==1){
      calYear = TRUE
    }else if(answer==2){
      calYear=FALSE
    }else{
      stop("You didn't enter 1 or 2")
    }
  }
  
  
  #make sure most recent is up.
  flipped=FALSE
  ydnn=na.omit(yearData$values)
  if(calYear){
    #the largest year should be first
    if(min(ydnn,na.rm =TRUE)==ydnn[1]){
      flipped=TRUE
    }
  }else{
    #the largest year should be last
    if(max(ydnn,na.rm =TRUE)==ydnn[1]){
      flipped=TRUE
    }
  }
  
  
  #does this lipd have a chronData?
  if(is.null(L$chronData)){
    L$chronData=vector(mode = "list",length=1)
  }
  
  C=L$chronData[[chron.num]]
  
  #initialize model
  if(is.na(model.num)){
    if(is.null(L$chronData[[chron.num]]$model[[1]])){
      #no models, this is first
      model.num=1
    }else{
      print(paste("You already have", length(L$chronData[[chron.num]]$model), "chron model(s) in chronData" ,chron.num))
      model.num=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  

  
  if(length(L$chronData[[chron.num]]$model)<model.num){
    if(make.new){
      L$chronData[[chron.num]]$model[[model.num]]=NA
    }else{
      nm=readline(prompt = paste("model",model.num,"doesn't exist. Create it? y or n "))
      if(grepl(pattern = "y",x = tolower(nm))){
        L$chronData[[chron.num]]$model[[model.num]]=NA
      }else{
        stop("Stopping, since you didn't want to create a new model")
      }
    }
  }
  
  
  
  CM=L$chronData[[chron.num]]$model[[model.num]]
  #get BAM parameters
  if(all(is.na(CM))){
    CM=list()
  }
  if(all(is.null(CM$methods))){
    CM$methods=list()
  }
  CM$methods$algorithm = "BAM"
  
  if(all(is.na(model))){
    
    #specify model type
    if(all(is.null( CM$methods$parameters$modelType))){
      print("Which type of model do you want to use for BAM?")
      print("1 - Poisson (default)")
      print("2 - Bernoulli")
      mi = as.integer(readline(prompt = "Pick a number: "))
      if(mi!=2){
        CM$methods$parameters$modelType="poisson"
      }else{
        CM$methods$parameters$modelType="bernoulli"
      }
    }
    
    #specify undercounting rate
    if(all(is.null( CM$methods$parameters$undercountingProbability))){
      print("What's the probability of undercounting")
      CM$methods$parameters$undercountingProbability = as.numeric(readline(prompt = "Enter a number between 0 and 1: "))
    }
    #specify overcounting rate
    if(all(is.null( CM$methods$parameters$overcountingProbability))){
      print("What's the probability of overcounting")
      CM$methods$parameters$overcountingProbability = as.numeric(readline(prompt = "Enter a number between 0 and 1: "))
    }
  }
  
  #ensemble members
  if(all(is.null( CM$methods$parameters$nEns))){
    if(!is.na(n.ens)){
      CM$methods$parameters$nEns=n.ens
    }else{
      CM$methods$parameters$nEns = as.integer(readline(prompt = "How many ensemble members?"))
    }
  }
  
  #this shouldn't change I think
  CM$methods$parameters$resize = 0
  
  if(all(is.na(model))){
    #create model
    model <- list(name= CM$methods$parameters$modelType,param=c(CM$methods$parameters$undercountingProbability,CM$methods$parameters$overcountingProbability)
                  ,ns=CM$methods$parameters$nEns,resize=CM$methods$parameters$resize)	
  }
  
  yearDataToRun = as.matrix(yearData$values)
  if(flipped){
    #then invert it before running
    yearDataToRun=as.matrix(yearDataToRun[nrow(yearDataToRun):1,])
  }
  
  #run BAM
  bamOut=simulateBam(yearDataToRun,yearDataToRun,ageEnsOut=TRUE,model = model)
  
  #check for existing ensembles
  n.enstables = length(CM$ensembleTable)
  if(n.enstables==0){#create 1
    #store output appropriately in model
    ens.table.number = 1
  }else if(all(is.na(ens.table.number))){
    print(paste("You already have", n.enstables, "ensemble table(s) in this model"))
    ens.table.number=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
  }
  
  if(n.enstables==0){#create
    CM$ensembleTable = vector(mode = "list",length = 1)
  }
  
  
  
  
  ensOut =  bamOut$ageEns
  if(flipped){
    #then flip it back
    ensOut  = as.matrix(ensOut[nrow(ensOut):1,])
  }
  
  #assign the appropriate name and units
  if(calYear){
    ensName = "yearEnsemble"
    ensUnits = "AD"
  }else{
    ensName = "ageEnsemble"
    ensUnits = "BP"
  }
  
  
  CM$ensembleTable[[ens.table.number]][[ensName]]$values = ensOut
  CM$ensembleTable[[ens.table.number]][[ensName]]$units = ensUnits
  CM$ensembleTable[[ens.table.number]][[ensName]]$variableName = ensName
  
  # CM$ensembleTable[[ens.table.number]]$timeCorrectionMatrix$values = bamOut$tmc
  # CM$ensembleTable[[ens.table.number]]$timeCorrectionMatrix$units = NA
  # CM$ensembleTable[[ens.table.number]]$timeCorrectionMatrix$description = "corresponding ensemble of time-correction matrices (tn*p*ns) to map realizations in Xp back to the original data X (2=insert nan, 0=remove double band)"
  
  L$chronData[[chron.num]]$model[model.num]=list(CM)
  
  #place into paleoData appropriately.
  #assign into measurementTable
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensName]]$variableName = ensName
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensName]]$values = ensOut
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensName]]$units = yearData$units
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensName]]$fromChronData = chron.num
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensName]]$fromModel = model.num
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensName]]$description = paste("age ensemble pulled from chronData", chron.num,"model",model.num,"- fit to paleoData depth with linear interpolation")
  
  return(L)
  
  
}


#' @export
#' @family BAM
#' @author Maud Comboul
#' @title Corrects a Banded Age Model (BAM)
#' @description Generate an ensemble of possible age corrected data:See www.clim-past-discuss.net/9/6077/2013/ for a detailed description of the model.
#' The time series in X are automatically flipped to range from most recent to oldest measurements when the intput t is given in increasing order. 
#' @examples
#' \dontrun{
#' res <- bamCorrect(X,t) 
#' #will generate an ensemble of 1000 age models randomly following
#' #a Poisson process with rate parameter theta=0.05 used to perturb data X
#
#' res <- bamCorrect(X,t,model) 
#' #will correct data X  with the model specified in
#' #the model structure
#' }
#' @param X data (vector or matrix n*p)
#' @param t chronology for data X (n*1)
#' @param model a list that describes the model to use in BAM
#' \itemize{
#' \item model$ns: number of samples 
#' \item model$name: 'poisson' or 'bernoulli' 
#' \item model$param: probability of growth band being perturbed (default: prob of missing band = prob of doubly-counted band = 0.05)
#' \itemize{
#'      \item if model$param is a single argument, then the perturbations are symmetric (prob of missing band = prob of doubly-counted band)
#'      \item if model$param = [a1 a2] and a1 neq a2 the model is asymmetric
#'      \itemize{
#'                  \item a1 = prob(missing layer) - undercounted
#'                  \item a2 = prob(layer counted multiple times) - overcounted
#'                  }
#'      \item if model$param: 2xp matrix, then different miscounting prob. are defined for each time series.
#'      }
#' \item model$resize: do not resize: 0 (default), resize to shortest sample: -1, resize to longest sample: 1
#' \item model$tm: if a time model is provided, the code returns the corresponding perturbed data
#' }
#' @return res a list with
#' \itemize{
#' \item res$Xc: realizations of age-perturbed data matrix of size tn*p*ns (could be 2 or 3d)
#' \item res$tc: new chronology tn*1
#' \item res$tmc: corresponding ensemble of time-correction matrices (tn*p*ns) to map realizations in Xp back to the original data X (2=insert nan, 0=remove double band) (2 or 3d)
#' where tn is the chronology length = n (default), shortest sample or longest sample depending on the chosen resizing option.
#' }
bamCorrect <- function(X, t, model=NULL){

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

#' @export
#' @family BAM
#' @author Maud Comboul
#' @title Simulate a Banded Age Model (BAM)
#' @description Generate an ensemble of possible age corrected data:See www.clim-past-discuss.net/9/6077/2013/ for a detailed description of the model.
#' The time series in X are automatically flipped to range from most recent to oldest measurements when the input t is given in increasing order. 
#' @examples
#' \dontrun{
#'res <- simulateBam(X,t) 
#' #will generate an ensemble of 1000 age models randomly following
#' #a Poisson process with rate parameter theta=0.05 used to perturb data X
#'
#' res <- simulateBam(X,t,model) 
#' #will perturb data X  with the model specified in
#' #the model structure
#' }
#' @param X data (vector or matrix n*p)
#' @param t chronology for data X (n*1)
#' @param model a list that describes the model to use in BAM
#' \itemize{
#' \item model$ns: number of samples 
#' \item model$name: 'poisson' or 'bernoulli' 
#' \item model$param: probability of growth band being perturbed (default: prob of missing band = prob of doubly-counted band = 0.05)
#' \itemize{
#'      \item if model$param is a single argument, then the perturbations are symmetric (prob of missing band = prob of doubly-counted band)
#'      \item if model$param = [a1 a2] and a1 neq a2 the model is asymmetric
#'      \itemize{
#'                  \item a1 = prob(missing layer) - undercounted
#'                  \item a2 = prob(layer counted multiple times) - overcounted
#'                  }
#'      \item if model$param: 2xp matrix, then different miscounting prob. are defined for each time series.
#'      }
#' \item model$resize: do not resize: 0 (default), resize to shortest sample: -1, resize to longest sample: 1
#' \item model$tm: if a time model is provided, the code returns the corresponding perturbed data
#' }
#' @param ageEnsOut TRUE or FALSE - return the ageEnsemble
#' @return res a list with
#' \itemize{
#' \item res$Xc: realizations of age-perturbed data matrix of size tn*p*ns (could be 2 or 3d)
#' \item res$tc: new chronology tn*1
#' \item res$tmc: corresponding ensemble of time-correction matrices (tn*p*ns) to map realizations in Xp back to the original data X (2=insert nan, 0=remove double band) (2 or 3d)
#' where tn is the chronology length = n (default), shortest sample or longest sample depending on the chosen resizing option.
#' \item res$ageEnsemble (optional): Returnd the full age ensemble if desired. 
#' }
simulateBam <- function(X, t, model=NULL,ageEnsOut=FALSE){
  # transpose X if time is not the first dimension
  X <- as.array(X)
  
  if (dim(X)[1] < dim(X)[2]) {
    X <- t(X)
  }
  
  # reverse time if not from old to new
  if (mean(diff(t),na.rm = TRUE)>0) {
    isflipped <- 1
    t <- rev(t)
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
  ns <- model$ns
  # Generate an ensemble of time perturbation models
  if (is.null(model[["tm"]])){
    model$tm = array(1,dim=c(n,p,ns))
    if (grepl("poisson",model$name)){
      for(ii in 1:p){
        for(jj in 2:n){
          num_event_mis <-rpois(1,model$param[jj,ii,1]*ns)
          num_event_dbl <-rpois(1,model$param[jj,ii,2]*ns)
          
          jumps <- sample(1:ns, num_event_mis, replace = FALSE, prob = NULL) #place events uniformly on {1,...,ns}
          model$tm[jj,ii,jumps] = model$tm[jj,ii,jumps] - 1                 #remove 1 at jump locations
          jumps <- sample(1:ns, num_event_dbl, replace = FALSE, prob = NULL)
          model$tm[jj,ii,jumps] = model$tm[jj,ii,jumps] + 1
          
        }
      }
    }
    else{
      if (grepl("bernoulli",model$name)){
        
        for (ii in 1:p){
          for (jj in 2:n){
            model$tm[jj,ii,] = model$tm[jj,ii,] - rbinom(ns,1,model$param[jj,ii,1])
            model$tm[jj,ii,] = model$tm[jj,ii,] + rbinom(ns,1,model$param[jj,ii,2])
          }
        }
      }
      else print("Unknown age model ; acceptable inputs are ''poisson'' and ''bernoulli'' ")
    }
  }
  
  # generate age perturbed data Xp
  # expand length of Xp and tXp if resizing is required
  if (model$resize == 1){
    t_ext <- ceiling(2*max(model$param)*n)
    tn <- n + t_ext
    X <- rbind(X, matrix(NaN,t_ext,p))
    dt <- t[2]-t[1];
    time_ext <- seq(tail(t,1)+dt,tail(t,1)+t_ext*dt,by=dt)
    tp <- c(t,time_ext)
  }
  else{
    tn <- n
    tp <- t
  }
  
  Xp <- array(NaN,dim = c(tn,p,ns))
  Tmax <- 0
  Tmin <- n
  tmc <- array(1, dim = c(tn,p,ns))
  
  for (nn in 1:ns) {
    
    for (ii in 1:p) {
      xcount <- 1
      Xcount <- 1
      tt <- 1
      while (tt < n+1) {
        
        if (model$tm[tt,ii,nn] == 0){     # remove band
          Xcount <- min(Xcount+1,tn)
          tmc[xcount,ii,nn] <- tmc[xcount,ii,nn]+1
        }
        else{
          if (model$tm[tt,ii,nn] == 2){   # insert double band
            Xp[xcount,ii,nn] <- X[Xcount,ii]
            tmc[xcount,ii,nn] <- tmc[xcount,ii,nn]-1
            xcount <- min(tn,xcount+1)
          }
        }
        Xp[xcount,ii,nn] <- X[Xcount,ii]
        xcount <- min(tn,xcount+1)
        Xcount <- min(tn,Xcount+1)
        tt <- tt+1
      }
      kall <- which(!is.nan(Xp[,ii,nn]), arr.ind=TRUE);
      k <- tail(kall,1)
      if (k > Tmax) Tmax <- k
      if (k < Tmin) Tmin <- k
    }
  }
  
  if (model$resize == -1)
    n <-Tmin
  
  # expand output size to longest (non-nan) sequence
  if (model$resize == 1)
    n <- Tmax
  
  Xp <- Xp[1:n,,];
  tp <- tp[1:n];
  tmc <- tmc[1:n,,];
  
  if (dim(X)[2]==1) {
    Xp  <- array(Xp,c(n,model$ns))
    tmc <- array(tmc,c(n,model$ns))
  }
  
  if (isflipped == 1){
    if (dim(X)[2]==1)
      Xp = apply(Xp,2,rev)
    else
      for (nn in 1:ns) {
        Xp[,,nn] = apply(Xp[,,nn],2,rev)
        tmc[,,nn] = apply(tmc[,,nn],2,rev)
      }
    
    tp = rev(tp);
  }
  if(ageEnsOut){
    ageEns=array(1, dim = c(tn,ns))
    for(i in 1:model$ns){
      ageEns[,i] = c(tp[1],tp[1]+cumsum(diff(tp)*tmc[-1,i]))
    }
    res <- list("Xp"=Xp,"tp"=tp,"tmc"=tmc,"ageEns"=ageEns)
    
    
  }else{
    res <- list("Xp"=Xp,"tp"=tp,"tmc"=tmc)
  }
  return(res)
}
