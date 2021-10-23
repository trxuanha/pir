source("Utility.R")

baseFolder = getwd()

inputName<- 'turnoverML'
target <- 'Long_emp_retention'

manipulableFactorNames<- c('Extraversion', 'Grey_wage')
manipulableFactorValues<- list(c(1, 2, 3, 4),
                               c(0,1))
excludeVars <- c('stag', 'event')

baseFolder = getwd()
method <- 'PIR'
fold <- 5
inputBase<- paste (baseFolder, '/input/', inputName, '/split', sep='') 
outputBase <- paste (baseFolder, '/output/', method,'/', inputName, sep='')
dir.create(file.path(outputBase), showWarnings = FALSE)
matrixPath <- paste (baseFolder, '/output/DAGs/', inputName, '_dag.csv' ,sep='')

doCrossValCat(inputName, target, matrixPath, fold, inputBase, outputBase, 
              manipulableFactorNames, manipulableFactorValues, newPG=T, excludeVars=excludeVars, outlierThresHold=1)