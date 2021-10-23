source("LiftUtility.R")
library(foreach)
library(doParallel)

inputName<- 'adultML'
target <- 'Prof'
manipulableFactorNames<- c('Education', 'WorkHour', 'SelfEmp')
manipulableFactorValues<- list(c(1, 2, 3),
                               c(1, 2, 3, 4),
                               c(0, 1))
excludeVars <- c()

baseFolder = getwd()
methods <- c('CF', 'CT', 'TOT', 'TST', 'FT')

fold <- 5
splitRule = ''
cvRule = ''
inputBase<- paste (baseFolder, '/input/', inputName, '/split', sep='') 

no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)

foreach(icount=1:5) %dopar% {
    
    method <- methods[icount]
    outputBase <- paste (baseFolder, '/output/', method,'/', inputName, sep='')
    dir.create(file.path(outputBase), showWarnings = FALSE)
    matrixPath <- paste (baseFolder, '/output/DAGs/', inputName, '_dag.csv' ,sep='')
    
    doCrossValTreesML(inputName, target, matrixPath, fold, inputBase, outputBase, manipulableFactorNames, 
                      manipulableFactorValues, method, excludeVars = excludeVars )    
    
}

