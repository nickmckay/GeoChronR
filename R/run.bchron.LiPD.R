#'@export
run.bchron.lipd <-  function(L,which.chron=NA,site.name=L$dataSetName,modelNum=NA, calCurves = NA){
  cur.dir = getwd()
  
  #initialize which.chron
  if(is.na(which.chron)){
    if(length(L$chronData)==1){
      which.chron=1
    }else{
      which.chron=as.integer(readline(prompt = "Which chronData do you want to run Bchron for? "))
    }
  }
  
  # load the Bchron package
  library(Bchron)
  
  #initialize model number
  if(is.na(modelNum)){
    if(is.null(L$chronData[[which.chron]]$chronModel[[1]])){
      #no models, this is first
      modelNum=1
    }else{
      print(paste("You already have", length(L$chronData[[which.chron]]$chronModel), "chron model(s) in chronData" ,which.chron))
      modelNum=as.integer(readline(prompt = "Enter the number for this model- will overwrite if necessary "))
    }
  }
  
  #pull out chronology
  C=L$chronData[[which.chron]]
  
  # Prompt the user for the calibration curve
  if(is.na(calCurves)){
    possible_curve = c("marine13","intcal13","shcal13","normal")
    print("You haven't specified a calibration curve")
    for (i in seq(from=1, to=length(possible_curve), by =1)){
      print(paste(i,": ",possible_curve[i]))}
    calCurves = possible_curve[as.integer(readline(prompt = "Enter the number of the calibration curve you'd like to use: "))]
  }
  
  #check for chronmeasurementTables
  if(length(C$chronMeasurementTable)!=1){
    stop("Bchron doesn't know how to handle more (or less) than 1 chron measurement table. You should teach it!")
  }
  
  MT=C$chronMeasurementTable[[1]]
  
  #go through required fields for BChron
  
  #14C age
  print("Looking for radiocarbon ages...")
  print("If using the normal calibration option, point to the U/Th ages")
  c14i = getVariableIndex(MT,"age14C")
  if (is.na(c14i)){
    stop("Bchron requires ages.")
  }else{
    age14C <- MT[[c14i]]$values}
  # Make sure this is in yr BP and not kyr
  if (mean(age14C, na.rm = TRUE)<10){
    age14C = 1000*age14C
  }
  
  #14C age uncertainty
  print("Looking for radiocrabon age uncertainty...")
  print("If using the normal calibration option, point to the U/Th ages uncertainty")
  c14unci = getVariableIndex(MT,"age14Cuncertainty",altNames = c("age","uncertainty"))
  if (is.na(c14unci)){
    print("No radiocarbon age uncertainty given in the chron measurement table, please enter an estimate")
    age14Cuncertainty = as.numeric(readline(prompt = "Enter the radiocarbon age uncertainty in years: "))
  }else{
    age14Cuncertainty <- MT[[c14unci]]$values}
  # Make sure the uncertainties are reported in years as well
  if (mean(age14Cuncertainty, na.rm = TRUE)<5){
    age14Cuncertainty = 1000*age14Cuncertainty
  }
  
  #age (calibrated)
  print("Looking for calibrated ages...")
  agei = getVariableIndex(MT,"age",altNames = "age")
  if (is.na(agei)){
    print("No calibrated age given in the chron measurement table")
  }else{
    calibratedAge <- MT[[agei]]$values} 
  
  #age uncertainty (calibrated)
  print("Looking for calibrated age uncertainty...")
  ageunci = getVariableIndex(MT,"ageUncertainty",altNames = c("age","uncertainty"))
  if (is.na(ageunci)){
    print("No calibrated age uncertainty given in the chron measurement table")
  }else{
    calibratedAgeU <- MT[[ageunci]]$values}
  
  #depth
  print("Looking for depth...")
  depthi = getVariableIndex(MT,"depth")
  if(is.na(depthi)){
    stop("Depth is required for Bchron")
  }else{
    depth=MT[[depthi]]$values
  }
  
  #reservoir age
  # only for marine13
  if (calCurves == 'marine13'){
    which.resi = readline(prompt = "Would you like to use the study's reservoir age (s) or use your own (o)? ")
    if(which.resi == "s"){                      
      print("Looking for radiocarbon reservoir age offsets (deltaR)...")
      print("can also use radiocarbon reservoir ages if need be...")
      resi = getVariableIndex(MT,"reservoirAge14C",altNames = "reservoir")
      reservoir <-MT[[resi]]$values
      if(is.na(resi)){
        print("The chron measurement table does not contain information about a reservoir age. Please enter your own")
        print("If you don't wish to apply an additional reservoir age correction, please enter 0. The marine 13 curve alreay contains a 400yr reservoir age correction.")
        reservoir = as.numeric(readline(prompt = "Enter the reservoir age in years: "))
      }else{
        print("Below are the values for the reservoir age correction applied in the study: ")
        print(MT[resi]$reservoirAge14C$values)
        subtract.standard = readline(prompt = "Do these values include the standard age correction of 400 years (y/n)?: ")
        if (subtract.standard == 'y'){
          reservoir = reservoir - 400
        }else if(subtract.standard != 'y' && subtract.standard != 'n'){
          stop("Please enter 'y' or 'n'")
        }
      }
    }else if(which.resi=="o"){
      reservoir = as.numeric(readline(prompt = "Enter the reservoir age in years: "))
    }else{stop("Only enter 's' or 'o'")}
  }
  
  #reservoir uncertainty
  # only for marine 13
  if (calCurves == 'marine13'){
    which.resUnci = readline(prompt = "Would you like to use the study's reservoir age uncertainty (s) or use your own (o)? ")
    if(which.resUnci == "s"){                      
      print("Looking for radiocarbon reservoir age uncertainties...")
      resUnci = getVariableIndex(MT,"reservoirAge14CUncertainty",altNames = c("reservoir","unc"))
      reservoirUnc <- MT[[resUnci]]$values
      if(is.na(resUnci)){
        print("The chron measurement table does not contain information about reservoir age uncertainty. Please enter your own")
        print("If you don't wish to apply an additional reservoir age correction, please enter 0.")
        reservoirUnc = as.numeric(readline(prompt = "Enter the reservoir age uncertainty in years: "))}
    }else if(which.resUnci == "o"){
      reservoirUnc = as.numeric(readline(prompt = "Enter the reservoir age uncertainty in years: "))
    }else{stop("Only enter 's' or 'o'")}
  }
  
  # Lab ID (not required by Bchron but needed for LiPD output)
  #labID
  print("Looking for laboratory ID...")
  idi = getVariableIndex(MT,"labID")
  if (is.na(idi)){
    print("No LabID provided in the chron measurement table")
  }else{
    LabID <- MT[[idi]]$values}
  
  #rejected ages
  print("Looking for column of reject ages, or ages not included in age model")
  rejeci = getVariableIndex(MT,"rejectedAges",altNames = c("reject","ignore"))
  if (is.na(rejeci)){
    print("No ages were rejected in the original study")
    print(age14C)
    print("Warning: Bchron will return an error message if the ages are outside of the calibration curve.")
    reject.anyway  = readline(prompt = "Would you like to reject any ages (y/n)? ")
    if (reject.anyway == 'y'){
      rejindex <- c()
      which.rejindex = as.integer(readline(prompt = "Enter the index of the first date you want to ignore: "))
      rejindex <- c(rejindex,which.rejindex)
      while (which.rejindex!=0){
        which.rejindex = as.integer(readline(prompt = "Enter the index of the of the other dates you want to ignore one by one. Enter zero when done: "))
      }
      age14C = age14C[-rejindex]
      age14Cuncertainty = age14Cuncertainty[-rejindex]
      depth = depth[-rejindex]
      LabID = LabID[-rejindex]
      if (length(reservoir)>1){
        reservoir = reservoir[-rejindex]
      }
      if (length(reservoirUnc)>1){
        reservoir = reservoirUnc[-rejindex]
      }
    } else if (reject.anyway != 'y' && reject.anyway != 'n'){
      stop('Please enter "y" or "n"')}
  }
  
  # ask user if they would rather use the depth from the paleo table
  
  which.depth = readline(prompt = "Would you like to interpolate the age model at the depth horizons for the paleoproxy data? (y/n): " )
  
  if (which.depth == 'n'){
    depth_predict = depth
  }else if (which.depth == 'y'){
    if(length(L$paleoData)==1){
      which.paleo=1
    }else{
      which.paleo=as.integer(readline(prompt = "Which paleoData do you want to run Bacon for? "))}
    P = L$paleoData[[which.paleo]]
    PT=P$paleoMeasurementTable[[1]]
    if(is.null(P)){
      stop("No paleo data measurement table available, please choose another option")}
    print("Looking for depth")
    depthip = getVariableIndex(PT,"depth")
    depth_predict <- PT[[depthip]]$values
    if (is.na(depthip)){
      stop("No depth in the measurement table available, please choose another option")}
  }else{stop("Please enter only 'y' or 'n'")}
  
  # Ask the user for the number of iterations
  print("How many iterations would you like to perform?")
  iter = as.integer(readline(prompt = "Enter the number of iterations: "))
  if (iter<10000){
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
  
  # Ask the user for the year the core has been extracted
  extractDate = as.numeric(readline(prompt = "When was this sample taken in years BP? Enter 0 if unknown: "))
  # check that people actually used years BP as asked
  if (extractDate>1900){extractDate=1950-extractDate}
  
  # Set up everything for the Bchron run
  # if marine13 is selected, make the necessary adjustmenet
  if (calCurves == "marine13"){
    #remove the reservoir age correction from the 14C ages
    ages = age14C - reservoir
    # calculate the uncertainty due to the radiocarbon measurement and the reservoir age correction
    age_sds = sqrt(age14Cuncertainty^2 + reservoirUnc^2)
    
    # perform one more check to make sure that the dates are in the calibration range of the selected 
    max_ages = ages-3*age_sds
    min_ages = ages+3*age_sds
    index_out = which(max_ages<=400 | min_ages>=35000)
    
    if (!is.null(index_out)){
      ages = ages[-index_out]
      age_sds = age_sds[-index_out]
      depth = depth[-index_out]
      labID = LabID[-index_out]
    } 
  }else {
    ages = age14C
    age_sds = age14Cuncertainty
  }
  
  # Perfom the run (finally)
  if (extractDate !=0){
    run = Bchronology(ages = ages, ageSds = age_sds, calCurves = c(rep(calCurves,length(depth))), positions = depth,
                      predictPositions = depth_predict, iterations = iter, extractDate = extractDate)
  } else {
    run = Bchronology(ages = ages, ageSds = age_sds, calCurves = c(rep(calCurves,length(depth))), positions = depth,
                      predictPositions = depth_predict, iterations = iter)
  }
  
  # Write back into a LiPD file
  
  # Create the place holder for the LiPD file
  L$chronData[[which.chron]]$chronModel[[modelNum]]=NA
  
  # Grab the methods first
  methods = list()
  methods$algorithm = 'Bchron'
  
  
  #write it out
  
  L$chronData[[which.chron]]$chronModel[[modelNum]]$methods=methods
  
  
  # Ensemble table since it's easy to access in Bchron
  ageEns = list()
  ageEns$values = t(run$thetaPredict)
  ageEns$units = 'calendar year BP'
  ageEns$depth = MT[[depthi]]$values
  ageEns$depth$units = MT[[depthi]]$units
  
  L$chronData[[which.chron]]$chronModel[[modelNum]]$ensembleTable=ageEns
  
  #Probability distribution table
  for (i in seq(from=1, to=length(run$calAges), by =1)){
    distTable=list()
    distTable$depth = run$calAges[[i]]$positions
    distTable$depthunits ='cm'
    distTable$calibrationCurve = calCurves
    distTable$age14C = run$calAges[[i]]$ages
    distTable$sd14C = run$calAges[[i]]$ageSds
    distTable$density = run$calAges[[i]]$densities
    distTable$grid = run$calAges[[i]]$ageGrid
    # write it out
    L$chronData[[which.chron]]$chronModel[[modelNum]]$distributionTable[[i]]=distTable
  }
  
  # Summary Table
  sumTable = list()
  sumTable$LabID = LabID
  sumTable$depth = depth
  sumTable$meanCalibratedAge = rowMeans(t(run$theta))
  L$chronData[[which.chron]]$chronModel[[modelNum]]$summaryTable=sumTable
  
  return(L)
  
}

