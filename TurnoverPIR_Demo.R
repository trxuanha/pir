source("Utility.R")

baseFolder = getwd()
inputName<- 'turnoverML'
target <- 'Long_emp_retention'

manipulableFactorNames<- c('Extraversion', 'Grey_wage')
manipulableFactorValues<- list(c(1, 2, 3, 4),
                               c(0,1))

excludeVars <- c( 'stag', 'event')
method <- 'PIR'

inputBase<- paste (baseFolder, '/input/', inputName, sep='') 
inBasePath <- paste (baseFolder, '/input/dag', sep='')
outputBase <- paste (baseFolder, '/output/PIR/',  sep='')

matrixPath <- paste (baseFolder, '/output/DAGs/', inputName, '_dag.csv' ,sep='')
trainFile <- paste (inputBase,'/', inputName, '.csv', sep='') 
trainDat <- read.csv(file = trainFile)
trainDat<- dplyr::select(trainDat, -excludeVars)

testFile <- paste (inputBase,'/', inputName, '_full.csv', sep='')   
orginalTestDat <-read.csv(file = testFile)
testDat <-dplyr::select(orginalTestDat, -excludeVars)

causalMax <- read.csv(file = matrixPath)
causalMax <- as(causalMax, "matrix")
mygraph <- as(causalMax,"graphNEL")
nattrs <- list(node=list(shape="ellipse", fixedsize=FALSE, fontsize=20))
plot(mygraph, attrs=nattrs)


pCGraph <- NULL
aCGraph <- NULL
pCausalGraphObj <- PCausalGraph(mygraph, target, manipulableFactorNames, manipulableFactorValues, 
                                trainDat, pCGraph, aCGraph, outlierThresHold=1)
results <- pCausalGraphObj$generateRecommendation(testDat)


fullPath <- paste(c(outputBase,'/', 'result_demo.csv' ), collapse = "")
write.csv(results,fullPath, row.names = FALSE)
	
	

