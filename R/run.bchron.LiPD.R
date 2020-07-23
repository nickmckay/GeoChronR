#'@export
#'@author Deborah Khider
#'@author Andrew Parnell
#'@author Nick McKay
#'@family Bchron
#'@title Generate a Bayesian Reconstruction Age Model  (Bacon) and add it into a LiPD object
#'@description This is a high-level function that uses Bchron to simulate an age model, and stores this as an age-ensemble in a model in chronData. If needed input variables are not entered, and cannot be deduced, it will run in interactive mode. See Haslett and Parnell (2008) doi:10.1111/j.1467-9876.2008.00623.x for details.
#'@inheritParams selectData
#'@param chron.num the number of the chronData object that you'll be working in
#'@param site.name the name of the site
#'@param model.num chron.numModel do you want to use?
#'@param cal.curves The calibration curves to be used. Enter either "marine13", intcal13", "shcal13" or "normal". Will prompt if not provided.
#'@return L. The single LiPD object that was entered, with methods, ensembleTable, summaryTable and distributionTable added to the chronData model.
#'@import Bchron
#'@examples
#'Run in interactive mode:
#'L = runBchron(L)
#'
#'Run in noninteractive mode:
#'L = runBchron(L,chron.num = 1, site.name = "MyWonderfulSite", model.num = 3, cal.curves = "marine13") 

runBchron =  function(L,
                      chron.num=NA,
                      which.table = NA,
                      site.name=L$dataSetName,
                      model.num=NA, 
                      cal.curves = NA,
                      iter = 10000,
                      outlier.probs = 0.05,
                      ask = TRUE,
                      lab.id.var="labID",  
                      age.14c.var = "age14C", 
                      age.14c.uncertainty.var = "age14CUnc", 
                      age.var = "age",
                      age.uncertainty.var = "ageUnc", 
                      depth.var = "depth", 
                      reservoir.age.14c.var = "reservoirAge",
                      reservoir.age.14c.uncertainty.var = "reservoirAge14C",
                      rejected.ages.var="rejected",
                      depth.units = "cm",
                      ...){
  
  
  cur.dir = getwd()
  
  #initialize chron.num
  if(is.na(chron.num)){
    if(length(L$chronData)==1){
      chron.num=1
    }else{
      chron.num=as.integer(readline(prompt = "Which chronData do you want to run Bchron for? "))
    }
  }
  
  
  #initialize model number
  if(is.na(model.num)){
    if(is.null(L$chronData[[chron.num]]$model[[1]])){
      #no models, this is first
      model.num=1
    }else{
      print(paste("You already have", length(L$chronData[[chron.num]]$model), "chron model(s) in chronData" ,chron.num))
      model.num=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  #get chron data data frame
  cdf <- createChronMeasInputDf(L,
                                chron.num,
                                which.table,
                                lab.id.var,
                                age.14c.var, 
                                age.14c.uncertainty.var, 
                                age.var, 
                                age.uncertainty.var, 
                                depth.var, 
                                reservoir.age.14c.var,
                                reservoir.age.14c.uncertainty.var,
                                rejected.ages.var)
  
  #replace NAs appropriately
  cdf[is.na(cdf[,1]),1] <- "unknown"
  cdf[is.na(cdf[,11]),11] <- 1
  cdf[is.na(cdf[,12]),12] <- "unknown"
  cdf[is.na(cdf[,7]),7] <- 0
  cdf[is.na(cdf[,8]),8] <- 0

  
  # Prompt the user for the calibration curve
  if(is.na(cal.curves)){
    if(!is.null(L$archiveType)){#make an educated guess
      if(grepl(L$archiveType,pattern = "marine")){
        cal.curves <- "marine13"
      }else{
        cal.curves <- "intcal13"
      }
    }else{
         possible_curve = c("marine13","intcal13","shcal13","normal")
         print("You haven't specified a calibration curve")
         for (i in seq(from=1, to=length(possible_curve), by =1)){
           print(paste(i,": ",possible_curve[i]))}
         cal.curves = possible_curve[as.integer(readline(prompt = "Enter the number of the calibration curve you'd like to use: "))]
    }
  }
  
  # Ask the user for the number of iterations
  if(is.na(iter)){
  print("How many iterations would you like to perform?")
  iter = as.integer(readline(prompt = "Enter the number of iterations: "))
  }
  if(ask & iter<10000){
    iter =10000
  }else if (iter>1000000){
    print("This is a large number of iterations!!!")
    are_you_sure = readline(prompt = "Do you want to continue (y/n)?: ")
    if (are_you_sure == 'n'){
      stop("Ok, let's get a more reasonable number of iterations.")
    }else if (are_you_sure != 'n' && are_you_sure != 'y'){
      stop("Enter 'y' or 'n'")
    }
  }
  
  
  
  # Set up everything for the Bchron run
  # adjust for reservoir ages
  #remove the reservoir age correction from the 14C ages
    cdf$adjustedAges <- cdf$allAge - cdf$reservoirAge
    
    # calculate the uncertainty due to the radiocarbon measurement and the reservoir age correction
    cdf$adjustedAgeUncertainty <- sqrt(cdf$allUnc^2 + cdf$reservoirAgeUnc^2)
    
    #figure out calcurves
    cdf$cal.curve <- cal.curves
    
    which.calage <- which(grepl(cdf$ageType,pattern = "cal"))
    cdf$cal.curve[which.calage] <- "normal"
    
    too.old <- which(cdf$adjustedAges + 3*cdf$adjustedAgeUncertainty > 35000)
    cdf$cal.curve[too.old] <- "normal"
    
    #add in outlier probs
    cdf$outlier.probs <- outlier.probs


  # Perfom the run (finally)
  run <-  Bchron::Bchronology(ages = cdf$adjustedAges, 
                            ageSds = cdf$adjustedAgeUncertainty, 
                            calCurves = cdf$cal.curve, 
                            positions = cdf$depth,
                            positionThicknesses = rep(1,length(cdf$depth)),
                            iterations = iter,
                            jitterPositions = TRUE,
                            outlierProbs = cdf$outlier.probs,
                            ...)
                            
  
  # Write back into a LiPD file
  
  # Create the place holder for the LiPD file
  # Grab the methods first
  methods = list()
  methods$algorithm = 'Bchron'
  
  
  #write it out
  
  L$chronData[[chron.num]]$model[[model.num]]=list(methods=methods)
  
  
  # Ensemble table since it's easy to access in Bchron
  ageEns = list()
  ageEns$ageEnsemble$values = t(run$thetaPredict)
  ageEns$ageEnsemble$variableName <- "ageEnsemble"
  
  bc <- which(colSums(is.finite(ageEns$ageEnsemble$values)) == 0)
  if(length(bc) > 0){
    if(length(bc) == ncol(ageEns$ageEnsemble$values)){
      stop("all ensemble values are NA")
    }
    ageEns$ageEnsemble$values <- ageEns$ageEnsemble$values[,-bc]
    
  }
  
  
  ageEns$ageEnsemble$units = 'yr BP'
  ageEns$depth$values = run$predictPositions
  ageEns$depth$units =  depth.units
  
  L$chronData[[chron.num]]$model[[model.num]]$ensembleTable[[1]]=ageEns
  
  #Probability distribution table
  for (i in seq(from=1, to=length(run$calAges), by =1)){
    distTable=list()
    distTable$depth = run$calAges[[i]]$positions
    distTable$depthunits ='cm'
    distTable$calibrationCurve = cal.curves
    distTable$age14C = run$calAges[[i]]$ages
    distTable$sd14C = run$calAges[[i]]$ageSds
    distTable$probabilityDensity$variableName = "probabilityDensity"
    distTable$probabilityDensity$values = run$calAges[[i]]$densities
    distTable$probabilityDensity$units = NA
    distTable$probabilityDensity$description = "probability density that for calibrated ages at specific ages"
    distTable$age$values = run$calAges[[i]]$ageGrid
    distTable$age$units = "yr BP"
    distTable$age$variableName <- "age"
    
    # write it out
    L$chronData[[chron.num]]$model[[model.num]]$distributionTable[[i]]=distTable
  }
  
  # Summary Table
  sumTable = list()
  sumTable$depth$values = depth
  sumTable$depth$units = depth.units
  
  sumTable$meanCalibratedAge$values = rowMeans(t(run$theta))
  sumTable$meanCalibratedAge$units = "yr BP"
  
  L$chronData[[chron.num]]$model[[model.num]]$summaryTable[[1]]=sumTable
  
  return(L)
  
}

