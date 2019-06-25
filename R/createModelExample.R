# #todo  - make into function
# 
# #create paleoEnsemble table
# 
# recaMatrix = matrix(rnorm(10000),ncol = 100) #replace with real data
# depths = seq(1,100) #replace with lipd File
# #L is lipd file
# 
# newModel <- vector(mode = "list",length = 2) #one model for RECA ensembles, a second for flux ensembles
# newModel[[1]]$methods <- list(description = "ensemble created by sampling uncertainty in grain size distribution and running through RECA", parameters = "nModes = 3")#example
# newModel[[1]]$ensembleTable <- vector(mode = "list",length = 1)
# #add in "columns"
# newModel[[1]]$ensembleTable[[1]]$depth$variableName <- "depth"
# newModel[[1]]$ensembleTable[[1]]$depth$units <- "cm"
# newModel[[1]]$ensembleTable[[1]]$recaEnsemble$values <- depths
# 
# 
# newModel[[1]]$ensembleTable[[1]]$recaEnsemble$variableName <- "recaEnsemble"
# newModel[[1]]$ensembleTable[[1]]$recaEnsemble$units <- "% volume"
# newModel[[1]]$ensembleTable[[1]]$recaEnsemble$values <- recaMatrix
# 
# #repeat for flux ensemble
# newModel[[2]]$methods <- list(description = "dust flux ensemble...", parameters = "binSize = 1")#example
# newModel[[2]]$ensembleTable <- vector(mode = "list",length = 1)
# #add in "columns"
# newModel[[2]]$ensembleTable[[1]]$depth$variableName <- "depth"
# newModel[[2]]$ensembleTable[[1]]$depth$units <- "cm"
# newModel[[2]]$ensembleTable[[1]]$recaEnsemble$values <- depths
# 
# 
# newModel[[2]]$ensembleTable[[1]]$dustFluxEnsemble$variableName <- "dustFluxEnsemble"
# newModel[[2]]$ensembleTable[[1]]$dustFluxEnsemble$units <- "g/cm2/yr"
# newModel[[2]]$ensembleTable[[1]]$dustFluxEnsemble$values <- dustFlux
# 
# #assign into LiPD file
# L$paleoData[[1]]$model <- newModel
