library(hash)
library(graph)
library(tidyr)
library (xgboost)
library(rpart.plot)
library(nnet)
library(stringr)


######################
## The main class
PCausalGraph<- function(causalGraph, targetVar, manipulableFactorNames, manipulableFactorValues, trainData, pCGraph=NULL, aCGraph=NULL, outlierThresHold=NULL){
    
    self.causalGraph <- causalGraph
    self.targetVar <- targetVar
    self.prRegModels <- hash()
    self.prPCGModels <- hash()
    self.manipulableFactorNames <- manipulableFactorNames
    self.manipulableFactorValues <- manipulableFactorValues
    self.causes <- c()
    self.orginalCauses <- c() 
    self.pCGraph <- pCGraph
    self.covariates <- c()
    self.separation<- '_vv_'
    self.aCGraph<- aCGraph
    self.outlierThresHold <- outlierThresHold
    
    ###############################
    ## inner functions
    
    self.estimatePCausalStrength<- function(inMatrix, externalData=NULL){
        inData <- as.data.frame(t(inMatrix), stringsAsFactors = TRUE)
        potentialCauses <- inEdges(self.targetVar, self.causalGraph)
        causalMax <- as(self.causalGraph, "matrix") ## get default values# no causal strenths -  just 1 and 0
        
        
        for (orcause in self.orginalCauses){
            
            ## Search to find treatment levels, the first is the control
            
            vIndex <- -1
            for(icount in 1: length(self.manipulableFactorNames)){
                if(self.manipulableFactorNames[icount] == orcause){
                    vIndex <- icount
                    break
                }
            }
            
            # find parents of a causes
            parentsOfCause <- inEdges(orcause, self.causalGraph)
            parentsOfTarget <- setdiff (potentialCauses[[self.targetVar]], orcause)
            allParents <- unique(c(parentsOfCause[[orcause]], parentsOfTarget))
            
            allParents <- unique(parentsOfCause[[orcause]])
            
            causaVals <- unlist(self.manipulableFactorValues[vIndex])
            
            # Propensity score model
            
            if(length(allParents) == 0){
                form.ps = NULL
            }else{
                form.ps = paste(orcause,'~ ' ,paste(allParents, collapse=' + '),  collapse='' )
            }
            
            propensities <- data.frame()
            if(length(causaVals) == 2){
                
                if(is.null(form.ps)){
                    propen <- rep(0.5, nrow(inData))
                }else{
                    if(has.key( orcause, self.prRegModels ) == FALSE){
                        
                        reg<-glm(as.formula(form.ps)
                                 , family="binomial"
                                 , data=inData)
                        self.prRegModels[[orcause]] <<- reg
                    }
                    reg <- self.prRegModels[[orcause]]
                    propen<-predict(reg, inData[allParents], type="response")   
                }
                # logistic regression
                
                levelbase <-  paste(orcause, self.separation , 0, sep ='')
                level1 <-  paste(orcause,self.separation , 1, sep ='')
                propensities <- data.frame(propen)
                colnames(propensities) <- c(level1)
                propensities[levelbase] <- 1 - propensities[level1] 
                
            }else{
                ## No catch
                
                if(is.null(form.ps)){
                    
                    causeName <-  paste(orcause, self.separation , causaVals[1], sep ='')
                    propen <- data.frame( causeName = rep(1/length(causaVals), nrow(inData)))
                    tempColNames <- c(causeName)
                    for (ival in 2: length(causaVals)){
                        causeName <-  paste(orcause, self.separation , causaVals[ival], sep ='')
                        tempVec <- rep(1/length(causaVals), nrow(inData))
                        propen <- cbind(propen, tempVec)
                        tempColNames <- c(tempColNames, causeName)
                    }
                    colnames(propen) <- tempColNames
                    
                }else{
                    # multinormila
                    if(has.key( orcause, self.prRegModels ) == FALSE){
                        reg<-multinom(as.formula(form.ps), data = inData,trace = FALSE)
                        self.prRegModels[[orcause]] <<- reg
                    }
                    reg <- self.prRegModels[[orcause]]
                    propen<- predict(reg, inData[allParents], type="probs")                        
                }
                if(! is.null(form.ps)){
                    # rename
                    lapply(colnames(propen), FUN=function(name) {  
                        colnames(propen)[colnames(propen) == name] <<- paste0(orcause, self.separation, name) 
                    })
                }
                propensities <- data.frame(propen)
            }
                
            untreated_outcome_adj <- 0
            untreated_weight <- 0 
            for (ival in 1: length(causaVals)){
                causeName <-  paste(orcause, self.separation , causaVals[ival], sep ='')
                tempVec <- rep(0, ncol(causalMax))
                causalMax <- rbind(causalMax, tempVec) 
                rownames(causalMax)[nrow(causalMax)] <- causeName
                
                if(ival > 1){
                    
                    treated_outcome_adj <- (inData[[causeName]])*(inData[[self.targetVar]])/propensities[[causeName]]
                    treated_weight <- inData[[causeName]]/propensities[[causeName]]
                    ipwEffect <- (sum(treated_outcome_adj)/sum(treated_weight)) - (sum(untreated_outcome_adj)/sum(untreated_weight))
                    causalMax[causeName, self.targetVar] <- ipwEffect   
                    
                }else{
                    untreated_outcome_adj <- (inData[[causeName]])*(inData[[self.targetVar]])/propensities[[causeName]]
                    untreated_weight <- inData[[causeName]]/propensities[[causeName]]
                    
                }
                
                
            }
        }
        
        allInterestRow <- c(self.causes, self.orginalCauses)
        allInterestCol <- c( self.targetVar, self.orginalCauses)
        return (causalMax[allInterestRow, allInterestCol])        
    }
    
    ## end of inner function
    ################################
    
    inTrainData <-data.frame(trainData)
    potentialCauses <- inEdges(self.targetVar, self.causalGraph)
    
    ## Preprocess data self.orginalCauses
    allLevels <-c()
    for(icount in 1: length(self.manipulableFactorNames)){
        varName <- self.manipulableFactorNames[icount]
        varVals <- self.manipulableFactorValues[icount]
        if( varName %in%  potentialCauses[[self.targetVar]]){
            
            ## add a new cause
            self.orginalCauses <- c(self.orginalCauses, varName)
            ## populate cause for each treatment level
            varVals <- unlist(varVals)
            
            for (ival in 1: length(varVals)){
                
                newFacName <-  paste(varName, self.separation , varVals[ival], sep ='')
                # The first value is the base/control value
                if(ival > 1){
                    self.causes <- c(self.causes, newFacName)    
                }
                allLevels <- c(allLevels, newFacName)
                
                inTrainData[newFacName] <- ifelse(inTrainData[varName] == varVals[ival],1,0)
            }
        }
    }
    
    #####################################
    
    # Convert data with columns are instances
    colTrainDat <- t(inTrainData)
    colnames(colTrainDat) <- row.names(inTrainData)
    colnames(colTrainDat) <- paste("T", 1:ncol(colTrainDat), sep="")  
    
    if(is.null(self.pCGraph) || is.null(self.aCGraph)){
        
        ## Start sample specifics
        nrsamples <- ncol(colTrainDat)
        samples <- colnames(colTrainDat)
        net <- self.estimatePCausalStrength(colTrainDat)
        self.aCGraph<- net
        agg <- c(net)
        lionessOutput <- matrix(NA, nrow(net) * ncol(net), nrsamples + 2)
        colnames(lionessOutput) <- c("reg", "tar", samples)
        lionessOutput[, 1] <- rep(row.names(net), ncol(net))
        lionessOutput[, 2] <- rep(colnames(net), each = nrow(net))
        lionessOutput <- as.data.frame(lionessOutput, stringsAsFactors = F)
        lionessOutput[, 3:ncol(lionessOutput)] <- sapply(lionessOutput[, 3:ncol(lionessOutput)], as.numeric)
        for (i in 1:nrsamples) {
            #print(paste0('sample: ', i))
            ss <- c(self.estimatePCausalStrength(colTrainDat[, -i]))
            lionessOutput[, i + 2] <- nrsamples * (agg - ss) + ss
        }
        self.pCGraph <- lionessOutput        
        
    }
    
    self.covariates <- setdiff( colnames(inTrainData), c(self.targetVar, allLevels, self.orginalCauses))
    ###############
    
    effThreshold <- 0
    excludeCauses <- c()
    
    tempGraph <- self.aCGraph[ self.aCGraph[ , 1] %in% self.causes, self.targetVar]
    meanEffect <- mean(abs(tempGraph))
    
    effThreshold <- 0
    for(i in 1:nrow(self.aCGraph)) {
        
        effect <- self.aCGraph[i, self.targetVar]
        
        if((effect < effThreshold) || (effect < 0)){
            excludeCauses <- c(excludeCauses, as.character(self.aCGraph[i, 1]))
            
        }
    }
    self.causes <- setdiff (self.causes, excludeCauses)
    
    
    ###############
    
    for (orcause in self.orginalCauses){
        vIndex <- -1
        for(icount in 1: length(self.manipulableFactorNames)){
            if(self.manipulableFactorNames[icount] == orcause){
                vIndex <- icount
                break
            }
        }
        
        causaVals <- unlist(self.manipulableFactorValues[vIndex])
        
        for (ival in 1: length(causaVals)){
            factor <-  paste(orcause, self.separation , causaVals[ival], sep ='')
            
            if( !(factor %in% self.causes ) ) {
                next
            }
            
            
            selRowIndex <- -1
            for(row in 1: nrow(self.pCGraph)){
                if(self.pCGraph[row,'reg'] == factor && self.pCGraph[row,'tar'] == self.targetVar){
                    selRowIndex <- row
                }
            }
            
            targetEffectVar <-  paste('CScore_',factor, sep ='') 
            ## add new columns
            
            tempInTrainData <- data.frame(inTrainData)
            
            
            for(row in 1: nrow(tempInTrainData)){
                tempInTrainData[row,  targetEffectVar ] <- self.pCGraph[selRowIndex, row + 2]
            }  
            
            f_train_data <- tempInTrainData
            baseFactor <-  paste(orcause, self.separation , causaVals[1], sep ='')
            
            set.seed(20)
            
            df <- f_train_data[targetEffectVar]
            z_scores <- as.data.frame(sapply(df, function(df) (abs(df-mean(df))/sd(df))))
            f_train_data ['z_scores'] <- z_scores
            if(is.null(self.outlierThresHold)){
                no_outliers <- f_train_data
            }else{
                no_outliers <- f_train_data [f_train_data$z_scores < self.outlierThresHold, ]
            }
            
            fitTree<- xgboost(data = as(no_outliers[self.covariates] , "matrix"), label = no_outliers[[targetEffectVar]],  
                              nthread = 4, 
                              nrounds = 3, 
                              subsample = 0.8,
                              num_parallel_tree =500,
                              eta = 1, 
                              max.depth = 5,
            )
            
            form.ps = paste(targetEffectVar,'~ ' ,paste(self.covariates, collapse=' + '),  collapse='' )
            self.prPCGModels[[factor]] <- fitTree
        }
    }
    
    
    
    ##############################
    
    structure(class = "PCausalGraph", list(
        getCauses = function() self.orginalCauses,
        getPCGraph = function() self.pCGraph,
        getACGraph = function() self.aCGraph,
        getDerivedCauses = function() self.causes,
        # methods
        predict = function(inputData) {
            
            print('self.covariates: ')
            print(self.covariates)
            
            
            testDat <-data.frame(inputData)
            targetCol = c()
            for (factor in self.causes){
                targetEffectVar <-  paste('CScore_',factor, sep ='')
                targetCol <- c(targetCol, targetEffectVar)
                predictModel <- self.prPCGModels[[factor]]
                testDat [targetEffectVar] <- predict(predictModel, as(testDat[self.covariates], "matrix"))
            }
            return (testDat[targetCol])
        }, # end of predict
		
        generateRecommendation = function(orginalTestDat) {
            
			inputData <- as.data.frame(orginalTestDat)
            print('self.covariates: ')
            print(self.covariates)
            
            
            testDat <-data.frame(inputData)
            targetCol = c()
            for (factor in self.causes){
                targetEffectVar <-  paste('CScore_',factor, sep ='')
                targetCol <- c(targetCol, targetEffectVar)
                predictModel <- self.prPCGModels[[factor]]
                testDat [targetEffectVar] <- predict(predictModel, as(testDat[self.covariates], "matrix"))
            }
			
			for(row in 1: nrow(testDat)){
				templiftCore <- -99999999
				tempSelFactor <- NULL
				
				for (selectedFactor in self.causes){
					
					targetEffectVar <-  paste('CScore_',selectedFactor, sep ='')
					
					if(testDat[row,targetEffectVar] > templiftCore) {
						
						templiftCore <- testDat[row,targetEffectVar]
						tempSelFactor <- selectedFactor
					} 
				}
				

					TREATMENT_TEMPVAL <- tempSelFactor
					TREATMENT_TEMPVAL <- strsplit(TREATMENT_TEMPVAL, self.separation)
					TREATMENT_TEMPVAL <- unlist(TREATMENT_TEMPVAL)
					TREATMENT_NAME <- TREATMENT_TEMPVAL[1]
					TREATMENT_VAL <- TREATMENT_TEMPVAL[2]  

				testDat[row,'ITE'] <- templiftCore
				testDat[row,'Recommended factor'] <- TREATMENT_NAME
				testDat[row,'Recommended level'] <- TREATMENT_VAL				
			
			}
			
			testDat<- dplyr::select(testDat, -targetCol)
			return (testDat)
			
			
        } # end of generateRecommendation		
        
    )) ## end methods def
}

####### end class definition


####

doCrossValCat<- function(inputName, target, matrixPath, fold, inputBase, outputBase, 
                         manipulableFactorNames, manipulableFactorValues, newPG=F , excludeVars=c(), outlierThresHold=NULL){
    
	set.seed(20)
    if(length(excludeVars > 0)){
        print('excludeVars')
        print(excludeVars)
    }
    causalMax <- read.csv(file = matrixPath)
    causalMax <- as(causalMax, "matrix")
    mygraph <- as(causalMax,"graphNEL")
    nattrs <- list(node=list(shape="ellipse", fixedsize=FALSE, fontsize=20))
    plot(mygraph, attrs=nattrs)
    for(timecn in 1: fold){
        
        trainFile <- paste (inputBase,'/', inputName, '_train_',timecn,'.csv', sep='') 
        trainDat <- read.csv(file = trainFile)
        trainDat<- dplyr::select(trainDat, -excludeVars)
        
        testFile <- paste (inputBase,'/', inputName, '_test_',timecn,'.csv', sep='') 
        orginalTestDat <-read.csv(file = testFile)
        testDat <- dplyr::select(orginalTestDat, -c(excludeVars, target))
        
        
        pCGraph <- NULL
        aCGraph <- NULL
        
        if(!newPG){
            newFileName <- paste (inputName, '_individual_',timecn,'.csv', sep='') 
            fullPath <- paste(c(outputBase,'/',newFileName ), collapse = "")
            pCGraph <-read.csv(file = fullPath) 
            pCGraph <- pCGraph[ , -1]
            
            newFileName <- paste (inputName, '_aggregate_',timecn,'.csv', sep='') 
            fullPath <- paste(c(outputBase,'/',newFileName ), collapse = "")
            aCGraph <-read.csv(file = fullPath)            
        }
        
        pCausalGraphObj <- PCausalGraph(mygraph, target, manipulableFactorNames, manipulableFactorValues, 
                                        trainDat, pCGraph, aCGraph, outlierThresHold)
        pCScores <- pCausalGraphObj$predict(testDat)
        
        if(newPG){
            newFileName <- paste (inputName, '_individual_',timecn,'.csv', sep='') 
            fullPath <- paste(c(outputBase,'/',newFileName ), collapse = "")
            write.csv(pCausalGraphObj$getPCGraph(), fullPath, row.names = T)
            
            newFileName <- paste (inputName, '_aggregate_',timecn,'.csv', sep='') 
            fullPath <- paste(c(outputBase,'/',newFileName ), collapse = "")
            write.csv(pCausalGraphObj$getACGraph(), fullPath, row.names = T)   
        }
        
        outFileName <- paste (inputName, '_PIR_',timecn,'.csv', sep='')   
        causes <- pCausalGraphObj$getCauses() 
        derivedCauses <- pCausalGraphObj$getDerivedCauses() 
        
        testDat <- as.data.frame(orginalTestDat)
        newTestData <- cbind(testDat, pCScores)
        
        getBestUpLiftScoreML(derivedCauses, newTestData, target, outputBase, outFileName)
        
        for (factor in causes){
            getUpLiftScoreML(factor, derivedCauses, newTestData, target, outputBase, outFileName)
        } 
        
        
        
        
    }## end of cross validation
}

getUpLiftScoreML<- function(infactor, derivedCauseList, inputDat, outComeColName, outputFileFolder, outFileName){
    data <- data.frame(inputDat)
    data['LIFT_SCORE'] = 0
    data['TREATMENT_NAME'] = ''
    data['UPLIFT'] = 0
    data ['Y_TREATED'] = 0
    data ['N_TREATED'] = 0
    data ['Y_UNTREATED'] = 0
    data ['N_UNTREATED'] = 0
    data ['FOLLOW_REC'] = 0
    separation <- '_vv_'
    
    factorList <- c()
    
    for(dcause in derivedCauseList){
        
        if( str_detect(dcause, infactor) ){
            
            factorList <- c(factorList, dcause)
        }
    }
    
    if(length(factorList) == 0){
        return(0)
    }
    
    for(row in 1: nrow(data)){
        
        templiftCore <- -99999999
        tempSelFactor <- NULL
        
        for (selectedFactor in factorList){
            
            targetEffectVar <-  paste('CScore_',selectedFactor, sep ='')
            
            
            if(data[row,targetEffectVar] > templiftCore) {
                
                templiftCore <- data[row,targetEffectVar]
                tempSelFactor <- selectedFactor
            } 
        }
        
        data[row,'LIFT_SCORE'] <- templiftCore
        data[row,'TREATMENT_NAME'] <- tempSelFactor
    }
    
    
    data$ABS_LIFT_SCORE <- abs(data$LIFT_SCORE)
    
    data <- data[order(-data$LIFT_SCORE),]
    
    
    y_treated <- 0
    n_treated <- 0
    y_untreated <- 0
    n_untreated <- 0
    for(row in 1: nrow(data)){
        TREATMENT_TEMPVAL = data[row,'TREATMENT_NAME']
        TREATMENT_TEMPVAL <- toString(TREATMENT_TEMPVAL)
        TREATMENT_TEMPVAL <- strsplit(TREATMENT_TEMPVAL, separation)
        TREATMENT_TEMPVAL <- unlist(TREATMENT_TEMPVAL)
        TREATMENT_NAME <- TREATMENT_TEMPVAL[1]
        TREATMENT_VAL <- TREATMENT_TEMPVAL[2]        
        data[row,'N_TREATED']<- n_treated
        data[row,'Y_TREATED']<- y_treated
        data[row,'N_UNTREATED']<- n_untreated
        data[row,'Y_UNTREATED']<- y_untreated
        
        ## binary causal factor
        if(length(factorList) == 1){
            TREATMENT_CRT <- '0'
        }else{
            TREATMENT_CRT <- '1'
        }
        
        if((TREATMENT_NAME != 'NA')&&(
            
            (data[row,TREATMENT_NAME] == TREATMENT_VAL)
        )
        
        ){
            data [row, 'FOLLOW_REC'] = 1
            n_treated <- n_treated + 1
            data[row,'N_TREATED']<- n_treated
            y_treated <- y_treated + data[row,outComeColName]
            data[row,'Y_TREATED']<- y_treated
        }else{
            n_untreated <- n_untreated + 1
            data[row,'N_UNTREATED']<- n_untreated
            y_untreated <- y_untreated + data[row,outComeColName]
            data[row,'Y_UNTREATED']<- y_untreated
        }
        if(n_treated == 0) {
            data[row,'UPLIFT'] = 0
        }else if(n_untreated == 0){
            data[row,'UPLIFT'] = 0
        }else{
            liftestimate = ((y_treated/n_treated) - (y_untreated/n_untreated) )*(n_treated + n_untreated)
            qiniestimate = ((y_treated) - (y_untreated*(n_treated/n_untreated) ))
            data[row,'UPLIFT'] <- liftestimate
        }
    }
    
    totalIncrease <- ((y_treated/n_treated) - (y_untreated/n_untreated) )
    for(row in 1: nrow(data)){
        n_treated <- data[row,'N_TREATED']
        y_treated <- data[row,'Y_TREATED']
        n_untreated <- data[row,'N_UNTREATED']
        y_untreated <- data[row,'Y_UNTREATED']
        liftestimate <- (((y_treated/n_treated) - (y_untreated/n_untreated) ))
        liftestimateWithBase <- (((y_treated/n_treated) - (y_untreated/n_untreated) ))/totalIncrease
        data[row,'UPLIFT'] <- liftestimate
    }
    dir.create(file.path(outputFileFolder, infactor), showWarnings = FALSE)
    fullPath <- paste(c(outputFileFolder,'/', infactor , '/', outFileName ), collapse = "")
    write.csv(data,fullPath, row.names = FALSE)
}




getBestUpLiftScoreML<- function(derivedCauseList, inputDat, outComeColName, outputFileFolder, outFileName){
    data <- data.frame(inputDat)
    data['LIFT_SCORE'] = 0
    data['TREATMENT_NAME'] = ''
    data['UPLIFT'] = 0
    data ['Y_TREATED'] = 0
    data ['N_TREATED'] = 0
    data ['Y_UNTREATED'] = 0
    data ['N_UNTREATED'] = 0
    data ['FOLLOW_REC'] = 0
    separation <- '_vv_'
    factorList <- derivedCauseList
    for(row in 1: nrow(data)){
        
        templiftCore <- -99999999
        tempSelFactor <- NULL
        
        for (selectedFactor in factorList){
            
            targetEffectVar <-  paste('CScore_',selectedFactor, sep ='')
            
            if(data[row,targetEffectVar] > templiftCore) {
                
                templiftCore <- data[row,targetEffectVar]
                tempSelFactor <- selectedFactor
            } 
        }
        
        data[row,'LIFT_SCORE'] <- templiftCore
        data[row,'TREATMENT_NAME'] <- tempSelFactor
    }
    
    
    data$ABS_LIFT_SCORE <- abs(data$LIFT_SCORE)
    
    data <- data[order(-data$LIFT_SCORE),]
    
    
    y_treated <- 0
    n_treated <- 0
    y_untreated <- 0
    n_untreated <- 0
    for(row in 1: nrow(data)){
        TREATMENT_TEMPVAL = data[row,'TREATMENT_NAME']
        TREATMENT_TEMPVAL <- toString(TREATMENT_TEMPVAL)
        TREATMENT_TEMPVAL <- strsplit(TREATMENT_TEMPVAL, separation)
        TREATMENT_TEMPVAL <- unlist(TREATMENT_TEMPVAL)
        TREATMENT_NAME <- TREATMENT_TEMPVAL[1]
        TREATMENT_VAL <- TREATMENT_TEMPVAL[2]        
        data[row,'N_TREATED']<- n_treated
        data[row,'Y_TREATED']<- y_treated
        data[row,'N_UNTREATED']<- n_untreated
        data[row,'Y_UNTREATED']<- y_untreated
        if((TREATMENT_NAME != 'NA')&&(
            
            ((data[row,TREATMENT_NAME] == TREATMENT_VAL) && (data[row,'LIFT_SCORE'] > 0))
            
            ||
            
            ((data[row,TREATMENT_NAME] == TREATMENT_VAL) && (data[row,'LIFT_SCORE'] <= 0))
        )
        
        ){
            data [row, 'FOLLOW_REC'] = 1
            n_treated <- n_treated + 1
            data[row,'N_TREATED']<- n_treated
            y_treated <- y_treated + data[row,outComeColName]
            data[row,'Y_TREATED']<- y_treated
        }else{
            n_untreated <- n_untreated + 1
            data[row,'N_UNTREATED']<- n_untreated
            y_untreated <- y_untreated + data[row,outComeColName]
            data[row,'Y_UNTREATED']<- y_untreated
        }
        if(n_treated == 0) {
            data[row,'UPLIFT'] = 0
        }else if(n_untreated == 0){
            data[row,'UPLIFT'] = 0
        }else{
            liftestimate = ((y_treated/n_treated) - (y_untreated/n_untreated) )*(n_treated + n_untreated)
            qiniestimate = ((y_treated) - (y_untreated*(n_treated/n_untreated) ))
            data[row,'UPLIFT'] <- liftestimate
        }
    }
    
    totalIncrease <- ((y_treated/n_treated) - (y_untreated/n_untreated) )
    for(row in 1: nrow(data)){
        n_treated <- data[row,'N_TREATED']
        y_treated <- data[row,'Y_TREATED']
        n_untreated <- data[row,'N_UNTREATED']
        y_untreated <- data[row,'Y_UNTREATED']
        liftestimate <- (((y_treated/n_treated) - (y_untreated/n_untreated) ))
        liftestimateWithBase <- (((y_treated/n_treated) - (y_untreated/n_untreated) ))/totalIncrease
        data[row,'UPLIFT'] <- liftestimate
    }
    dir.create(file.path(outputFileFolder, 'bestFactor'), showWarnings = FALSE)
    fullPath <- paste(c(outputFileFolder,'/', 'bestFactor' , '/', outFileName ), collapse = "")
    write.csv(data,fullPath, row.names = FALSE)
}

splitData<- function(inputFilePath, outputFileFolder, splitRatio, fold){
    
    set.seed(20)
    fileToSplit <-read.csv(file = inputFilePath)
    nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
    
    #fileToSplit <- as.data.frame(lapply(fileToSplit, nor))
    
    totalSize = nrow(fileToSplit)
    fileName = basename(inputFilePath)
    fileNameParts <- strsplit(fileName ,'\\.')
    fileNameParts <- unlist(fileNameParts)
    for(icount in 1:fold){
        toIndex <- sample.int(n = totalSize, size = floor(splitRatio*totalSize), replace = F)
        train <- fileToSplit[toIndex, ]
        test  <- fileToSplit[-toIndex, ]
        newFileName <- paste(c(fileNameParts[1],'_train', '_',icount,'.', fileNameParts[2]), collapse = "")
        fullPath <- paste(c(outputFileFolder,'/',newFileName ), collapse = "")
        write.csv(train,fullPath, row.names = FALSE)
        newFileName <- paste(c(fileNameParts[1],'_test', '_',icount,'.', fileNameParts[2]), collapse = "")
        fullPath <- paste(c(outputFileFolder,'/',newFileName ), collapse = "")
        write.csv(test,fullPath, row.names = FALSE)
    }
}
