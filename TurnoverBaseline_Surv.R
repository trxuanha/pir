source("LiftUtility.R")
library(foreach)
library(doParallel)

baseFolder = getwd()
inputName<- 'turnoverML'
targetVar <- 'Long_emp_retention'
manipulableFactorNames<- c('Extraversion', 'Grey_wage')
manipulableFactorValues<- list(c(1, 2, 3, 4),
                               c(0,1))

methods <- c('CT', 'TOT', 'TST', 'FT', 'CF')
excludeVars <- c( 'stag', 'event')

no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)

foreach(icount=1:5) %dopar% {
    
    method <- methods[icount]
    
    if(method == 'CT'){
        splitRule = 'CT'
        cvRule = 'CT'
    }
    if(method == 'TOT'){
        splitRule = 'TOT'
        cvRule = 'TOT'
    }
    if(method == 'TST'){
        splitRule = 'tstats'
        cvRule = 'fit'
    }
    if(method == 'FT'){
        splitRule = 'fit'
        cvRule = 'fit'
    } 
    
    testType <- method
    separation<- '_vv_'
    
    inputBase<- paste (baseFolder, '/input/', inputName, sep='') 
    outputBase <- paste (baseFolder, '/output/', method,'/', inputName, '/One', sep='')
    dir.create(file.path(outputBase), showWarnings = FALSE)
    matrixPath <- paste (baseFolder, '/output/DAGs/', inputName, '_dag.csv' ,sep='')
    trainFile <- paste (inputBase,'/', inputName, '.csv', sep='') 
    trainDat <- read.csv(file = trainFile)
    trainDat<- dplyr::select(trainDat, -excludeVars)
    
    testFile <- paste (inputBase,'/', inputName, '_full.csv', sep='') 
    orginalTestDat<- read.csv(file = testFile)
    
    causalMax <- read.csv(file = matrixPath)
    causalMax <- as(causalMax, "matrix")
    mygraph <- as(causalMax,"graphNEL")
    nattrs <- list(node=list(shape="ellipse", fixedsize=FALSE, fontsize=20))
    plot(mygraph, attrs=nattrs)
    
    potentialCauses <- inEdges(targetVar, mygraph)
    causes <- c()
    for(cause in potentialCauses[[targetVar]]){
        if( cause %in%  manipulableFactorNames){
            causes <- c(causes, cause)
        }
    }            
    
    outFileName <- paste (inputName, '_',method , sep='')
    
    
    
    covariates = unique(setdiff(colnames(trainDat), c(targetVar, causes)))
    
    
    globalPredictions <- NULL
    globalTempColNames <- c()
    
    for (factor in causes) {
        parentsOfCause <- inEdges(factor, mygraph)
        parentsOfTarget <- setdiff (potentialCauses[[targetVar]], factor)
        allParents = unique(c(parentsOfCause[[factor]], parentsOfTarget))
        
        vIndex <- -1
        for(icount in 1: length(manipulableFactorNames)){
            if(manipulableFactorNames[icount] == factor){
                vIndex <- icount
                break
            }
        }
        
        predictions <- NULL
        causaVals <- unlist(manipulableFactorValues[vIndex])
        tempColNames <- c()
        
        for (ival in 2: length(causaVals)){
            # causaVals[1]: control group
            f_train_data <- dplyr::filter(trainDat, !!as.name(factor ) %in% c( causaVals[1], causaVals[ival] ) )
            
            f_train_data[factor] <- ifelse(f_train_data[factor] == causaVals[1],0, 1)
            
            testDat <- data.frame(orginalTestDat)
            
            if(testType == 'CF'){
                
                forest <- causal_forest(X = f_train_data[covariates], Y = f_train_data[[targetVar]], 
                                        W = f_train_data[[factor]])
                assign(paste0(factor, separation, causaVals[ival]) , predict(forest, newdata = testDat[covariates]))          
            } else{
                
                form.ps = paste(factor,'~ ' ,paste(allParents, collapse=' + '),  collapse='' )
                
                reg<-glm(as.formula(form.ps)
                         , family=binomial
                         , data=f_train_data)  
                propensity_scores = reg$fitted
                
                form.ps = paste(targetVar, paste(covariates, collapse = "+"), sep = "~")
                
                tree <- causalTree(as.formula(form.ps)
                                   , data = f_train_data, treatment = f_train_data[[factor]],
                                   split.Rule = splitRule, cv.option = cvRule, split.Honest = T, cv.Honest = T, split.Bucket = F, 
                                   xval = 5, cp = 0,  propensity = propensity_scores, 
                )
                
                assign(paste0(factor, separation, causaVals[ival]) , predict(tree, newdata = testDat[covariates]))
                
            } 
            
            
            if (is.null(predictions)){
                predictions <- data.frame(eval( as.name (paste0(factor, separation, causaVals[ival]) ) )   )
            }else{
                predictions <- cbind (predictions, eval( as.name (paste0(factor, separation, causaVals[ival]) ) ) )
            }
            
            if (is.null(globalPredictions)){
                globalPredictions <- data.frame(eval( as.name (paste0(factor, separation, causaVals[ival]) ) )   )
            }else{
                globalPredictions <- cbind (globalPredictions, eval( as.name (paste0(factor, separation, causaVals[ival]) ) ) )
            }
            
            
            tempColNames <- c(tempColNames, paste0(factor, separation, causaVals[ival]))
            globalTempColNames <- c(globalTempColNames, paste0(factor, separation, causaVals[ival]))
            
            
        }## end of causaVals
        
        
        colnames(predictions) <- tempColNames
        predictions['LIFT_SCORE'] <- apply(predictions, 1, max)
        predictions['TREATMENT_NAME'] <- colnames(predictions[tempColNames])[apply(predictions[tempColNames], 1, which.max)]
        
        
    }##############
    
    
    colnames(globalPredictions) <- globalTempColNames
    globalPredictions['LIFT_SCORE'] <- apply(globalPredictions, 1, max)
    
    globalPredictions['TREATMENT_NAME'] <- colnames(globalPredictions[globalTempColNames])[apply(globalPredictions[globalTempColNames], 1, which.max)]
    
    testBestDat <- data.frame(orginalTestDat)
    testBestDat <- cbind(testBestDat, globalPredictions)
    testBestDat <- estimateBestUplift(testBestDat, targetVar)
    factOutputBase <- paste (outputBase, '/', 'bestFactor' ,sep='')
    dir.create(file.path(outputBase, 'bestFactor'), showWarnings = FALSE)
    outFileName <- paste (inputName, '_', testType ,'.csv', sep='') 
    fullPath <- paste(c(factOutputBase,'/', outFileName ), collapse = "")
    write.csv(testBestDat, fullPath, row.names = FALSE)
}

