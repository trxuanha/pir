source("Utility.R")


baseFolder = getwd()

inputName<- 'hrML'
target <- 'time_spend_company'
manipulableFactorNames<- c('satisfaction_level', 'average_montly_hours')
manipulableFactorValues<- list(c(1, 2, 3),
                               c(1, 2, 3, 4, 5))
excludeVars <- c('left')


method <- 'PIR'

inputBase<- paste (baseFolder, '/input/', inputName, sep='') 
outputBase <- paste (baseFolder, '/output/PIR/', inputName,'/One', sep='')
dir.create(file.path(outputBase), showWarnings = FALSE)
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


# Convert data with columns are instances
colTrainDat <- t(trainDat)
colnames(colTrainDat) <- row.names(trainDat)
colnames(colTrainDat) <- paste("T", 1:ncol(colTrainDat), sep="")

colTestDat <- t(testDat)
colnames(colTestDat) <- row.names(testDat)
colnames(colTestDat) <- paste("Te", 1:ncol(colTestDat), sep="")


pCGraph <- NULL
aCGraph <- NULL
outlierThresHold <- 2
pCausalGraphObj <- PCausalGraph(mygraph, target, manipulableFactorNames, manipulableFactorValues, 
                                trainDat, pCGraph, aCGraph, outlierThresHold)
pCScores <- pCausalGraphObj$predict(testDat)

outFileName <- paste (inputName, '_PIR','.csv', sep='')  
causes <- pCausalGraphObj$getCauses() 
derivedCauses <- pCausalGraphObj$getDerivedCauses() 

testDat <- as.data.frame(orginalTestDat)
newTestData <- cbind(testDat, pCScores)
getBestUpLiftScoreML(derivedCauses, newTestData, target, outputBase, outFileName)

