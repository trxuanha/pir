source("Utility.R")

baseFolder = getwd()

inputName<- 'hrML'
target <- 'time_spend_company'
manipulableFactorNames<- c('satisfaction_level', 'average_montly_hours')
manipulableFactorValues<- list(c(1, 2, 3),
                               c(1, 2, 3, 4, 5))
excludeVars <- c('left')

baseFolder = getwd()
method <- 'PIR'
fold <- 5
inputBase<- paste (baseFolder, '/input/', inputName, '/split', sep='') 
outputBase <- paste (baseFolder, '/output/', method,'/', inputName, sep='')
dir.create(file.path(outputBase), showWarnings = FALSE)
matrixPath <- paste (baseFolder, '/output/DAGs/', inputName, '_dag.csv' ,sep='')

doCrossValCat(inputName, target, matrixPath, fold, inputBase, outputBase, 
              manipulableFactorNames, manipulableFactorValues, newPG=T, excludeVars=excludeVars, outlierThresHold=2)