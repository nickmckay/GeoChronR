#sample radiocarbon dates from LiPD distribution table

#DT <- L$chronData[[1]]$model[[1]]$distributionTable

#sample 1 from LiPD distribution object
sampleAge <- function(dto){
  #calculate the cumulative probabilites, after adding in a tiny slope to avoid ties, and normalizing to sum to 1.
  cdf <- (cumsum(dto$probabilityDensity$values) + seq(0,0.00000001,length.out = length(dto$probabilityDensity$values)))/sum(dto$probabilityDensity$values)
  sampledAge <- approx(x = cdf, y = dto$age$values, xout = runif(1))$y
  return(sampledAge)
}

fitMonotonicSpline <- function(age,depth,model.depths,kFrac){
    k <- min(max(round(kFrac * sum(is.finite(age))),5),sum(is.finite(age)))
    
    scam_fit <- scam::scam(age ~ s(depth, bs = "mpi",k = k), sp = 1) |>   
      predict(newdata = data.frame(depth = model.depths))
    
    return(scam_fit)
  }



removeOutliers <- function(age,depth) {
  #first sort by depth
  di <- sort(depth,index.return = TRUE)$ix
  origIndex <- seq_along(di)[di]
  
  age <- age[di]
  
  ageOrig <- age
  
  diffs <- diff(age)
  # 
  # #removal probabilities
  # diffs[diffs > 0] <- 0
  # removeWeights <- diffs/sum(diffs)
  # removeWeights[removeWeights < 1/length(diffs)] <- 1/length(diffs)
  # removeWeights <- c(1/length(diffs),removeWeights) #add in another for the first age
  removeWeights <- rep(1,times = length(age))
  
  
  while(any(diff(na.omit(age)) <= 0,na.rm = TRUE)){
    tr <- sample(seq_along(age),prob = removeWeights,size = 1)
    age[tr] <- NA
    removeWeights[tr] <- 0
  }
  
  #now let's try to add as many back in as we can.
  removed <- which(is.na(age))
  shuffled <- sample(removed)
  
  for(i in 1:length(shuffled)){
    age[shuffled[i]] <- ageOrig[shuffled[i]]
    if(any(diff(na.omit(age)) <= 0,na.rm = TRUE)){
      #reversal! take it back out
      age[shuffled[i]] <- NA
    }
  }
  
  return(age[order(di)])
}


ageProb <- function(dto,ageToTest){
  #calculate the cumulative probabilites, after adding in a tiny slope to avoid ties, and normalizing to sum to 1.
  pdf <- dto$probabilityDensity$values
  prob <- approx(y = pdf, x = dto$age$values, xout = ageToTest)$y
  min.prob <- min(pdf[pdf > 0])
  
  if(is.na(prob)){#then its off the scale, interpolate between 0 and the lowest point

    if(ageToTest < min(dto$age$values)){#too young
      prob <- approx(x = c(0,dto$age$values[which.min(dto$age$values)]), y = c(0,min.prob), xout = ageToTest)$y
    }else{
      prob <- approx(x = c(10* dto$age$values[which.min(dto$age$values)],dto$age$values[which.min(dto$age$values)]), y = c(0,min.prob), xout = ageToTest)$y
    }
  }
  
  logProb <- log(prob)
  if(!is.finite(logProb)){
    logProb <- log(min.prob)
  }
  return(logProb)
}





createVarveAgePriors <- function(DT,
                                 model.depths  = NA,
                                 model.depth.step = 1,
                                 varveScalingFactor = 30,#prior
                                 H = NULL, #prior
                                 ar1 = NULL, #prior
                                 kFrac = 1/3, #prior
                                 n.ms.ens = 100,
                                 n.varve.ens = 100,
                                 heuristicOutlierRemoval = TRUE,
                                 outlierRemovedFraction = .95,
                                 progress = TRUE){
  
  
  sampledAgeEns <- purrr::map(1:n.ms.ens,\(x) map_dbl(DT,sampleAge)) |> 
  purrr::list_c() |> 
  matrix(nrow = length(DT),ncol = n.ms.ens)
  
  
  sampleDepths <- purrr::map_dbl(DT,"depth")
  
  if(heuristicOutlierRemoval){
    filteredSampledAgeEns <- apply(sampledAgeEns,2, removeOutliers, sampleDepths)
    wc <- sample(seq_len(ncol(sampledAgeEns)),size = round(ncol(sampledAgeEns) * outlierRemovedFraction))
    sampledAgeEns[,wc] <- filteredSampledAgeEns[,wc]
  }



if(all(is.na(model.depths))){
  model.depths <- seq(0,ceiling(max(sampleDepths)),by = model.depth.step)
}

depthStep <- median(diff(model.depths),na.rm = TRUE)

if(progress){message(crayon::blue("Estimating low-frequency variability in the model..."))}

if(progress){
aeig <- pbapply::pbapply(X = sampledAgeEns, 
                         MARGIN = 2, 
                         FUN = fitMonotonicSpline, 
                         depth = sampleDepths, 
                         model.depth = model.depths, 
                         kFrac = kFrac)
}else{
  aeig <- apply(X = sampledAgeEns, 
                           MARGIN = 2, 
                           FUN = fitMonotonicSpline, 
                           depth = sampleDepths, 
                           model.depth = model.depths, 
                           kFrac = kFrac)
}
#calculate accum rates
mgDepthSteps <- matrix(rep(diff(model.depths),times = n.ms.ens),ncol = n.ms.ens,nrow = length(model.depths) - 1) 
mgAgeSteps <- apply(X = aeig, MARGIN = 2,FUN = diff)
yrPerDepth <- mgAgeSteps/mgDepthSteps
meanScamSedrate <- 1/mean(yrPerDepth)
varveMean <- meanScamSedrate * varveScalingFactor


#add the high frequency bit
if(progress){message(crayon::blue("Estimating high-frequency variability in the model..."))}
varvedPrior <- addVarves(ages = aeig,
                        model.depths = model.depths,
                        yrPerDepth = yrPerDepth,
                        varveMean = varveMean,
                        H = H,
                        ar1 = ar1, 
                        n.varve.ens = n.ms.ens,
                        DT = DT,
                        progress = progress)

return(varvedPrior)
}




addVarves <- function(ages, model.depths,  yrPerDepth, varveMean, H, ar1, n.varve.ens, DT,progress = TRUE){
  nYears <- apply(ages, 2, \(x) ceiling(max(x)) - floor(min(x)))
  
  #speed up here? Arima (AR1) is much faster
  v1 <- purrr::map(nYears,
                   simulateVarves,
                   H = H,
                   ar1 = ar1,
                   mean = varveMean,
                   n.ens = 1,
                   length.out = max(nYears),
                   .progress = progress) |> 
    purrr::list_c() |> 
    matrix(ncol = length(nYears), nrow = max(nYears))
  
  #v1 <- simulateVarves(nYears,n.ens = n.varve.ens,H = H,ar1 = ar1,mean = varveMean)
  vInv <- 1/v1
  d <- seq_len(nrow(vInv))
  depthStep <- median(abs(diff(d)))
  
  #speed up here? yep, dplyr is MUCH faster
  #bvInv <- geoChronR::binEns(time = d, values = vInv,bin.vec = model.depths)
  
  ageDepths <- model.depths[map_dbl(d, \(x) which.min(abs(x - model.depths)))]
  
  df <- as.data.frame(cbind(ageDepths,vInv)) |> 
    pivot_longer(-ageDepths,values_to = "accRate", names_to = "ens") |> 
    group_by(ageDepths, ens) |> 
    summarize(binned = mean(accRate,na.rm = TRUE)) |> 
    pivot_wider(names_from = ens,values_from = binned)
  
  bvInv <- as.matrix(df)[,-1]
  ageDepths <- df$ageDepths
  
  if(ncol(bvInv) == ncol(yrPerDepth)){
    prior <- bvInv * yrPerDepth
  }else{
    stop("Feature is not set up yet - n.varve.ens and n.ms.ens need to be the same for now")
  }
  
  totalAge <- colSums(prior * depthStep)
  
  #scale to total age
  adjustFactor <- nYears/totalAge * depthStep
  adjustedForTotalAgePrior <- matrix(adjustFactor,ncol = ncol(prior),nrow = nrow(prior), byrow = TRUE) * prior
  
  
  if(!all(round(colSums(adjustedForTotalAgePrior * depthStep)) == nYears)){
    stop("Mismatch")
  }
  
  #sum up the ages
  cumAges <- apply(adjustedForTotalAgePrior,MARGIN = 2,cumsum)
  
  #adjust for starting age
  minAge <- apply(X = ages,MARGIN = 2,min)
  agePriors <- matrix(minAge,ncol = ncol(prior),nrow = nrow(prior),byrow = TRUE) + cumAges
  
  #assess fit
  #ages and depths may not match?
  varvedPriorLogObj <- ageProbsDT(DT,agePriors,ageDepths)
  
  
  return(list(agePriors = agePriors, 
              ageDepths = ageDepths,
              varvedPriorLogObj = varvedPriorLogObj))
}


getAgeLikelihoodFromEns <- function(i,DT,sampleAE){
  oneEns <- sampleAE[,i]
  logObj <- sum(purrr::map_dbl(seq_along(DT),\(x) ageProb(DT[[x]],ageToTest = oneEns[x])))
  return(logObj)
}


ageProbsDT <- function(DT, ageEstimates,depths){
  sampleDepths <- purrr::map_dbl(DT,"depth")
  whichSample <- purrr::map_dbl(sampleDepths,\(x) which.min(abs(x-depths)))
  
  sampleAE <- ageEstimates[whichSample,]
  aeDepths <- depths[whichSample]
  
  ensObj <- purrr::map_dbl(seq_len(ncol(ageEstimates)), getAgeLikelihoodFromEns,DT,sampleAE)
  
  return(ensObj)
}
# 
# 
# if(length(whichFun) == 0){
#   stop("Depth is not within a cm of an age ensemble table depth")
# }
# if(length(whichFun) > 1){
#   whichFun <- whichFun[1]
# }
# ap <- cdfFuns[[whichFun]](ageEstimates)
# if(ap <= 0){ap <- 1e-16}
# 
# return(log(ap))
# }
# 



ageProbs <- function(ATdepth, ageEstimates, cdfFuns){
  whichFun <- which(dplyr::near(ATdepth,ATdepths,tol = 0.5))
  if(length(whichFun) == 0){
    stop("Depth is not within a cm of an age ensemble table depth")
  }
  if(length(whichFun) > 1){
    whichFun <- whichFun[1]
  }
  ap <- cdfFuns[[whichFun]](ageEstimates)
  if(ap <= 0){ap <- 1e-16}
  
  return(log(ap))
}


modelProbs <- function(newModel){
  agesEstimatesAtATDepths <- Hmisc::approxExtrap(x = depthBins,y = newModel,xout = ATdepths)$y
  
  logObj <- sum(purrr::map2_dbl(ATdepths,agesEstimatesAtATDepths,ageProbs,cdfFuns))
  return(logObj)
}



