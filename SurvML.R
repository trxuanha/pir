library(survival)
library(survminer)
library(ggpubr)


factor <- 'bestFactor'
baseFolder <- getwd()
topPercent <- 0.25
dataforPlot <- NULL

getSurvPlot <-function(inData, method, topPercent, inputName){
    
    liftSurvivalData <-data.frame(inData)
    liftSurvivalData <- liftSurvivalData[order(-liftSurvivalData$LIFT_SCORE),]
    topRow = as.integer(nrow(liftSurvivalData)*topPercent)
    liftSurvivalData <- liftSurvivalData[1:topRow, ]
    dataforPlot <<- liftSurvivalData
	
	if(inputName == 'turnoverML'){
	    fit2 <- survfit(Surv(time = dataforPlot$stag, event = dataforPlot$event)
                    ~ FOLLOW_REC , data = dataforPlot)
		surv_diff <- survdiff(Surv(time = dataforPlot$stag, event = dataforPlot$event)
							  ~ FOLLOW_REC, data = dataforPlot, rho = 0)
	}

	if(inputName == 'hrML'){
		fit2 <- survfit(Surv(time = dataforPlot$time_spend_company, event = dataforPlot$left)
						~ FOLLOW_REC , data = dataforPlot)
		surv_diff <- survdiff(Surv(time = dataforPlot$time_spend_company, event = dataforPlot$left)
							  ~ FOLLOW_REC, data = dataforPlot, rho = 0)
	}
	
	
    options(scipen=999)

    if(method == 'Xlearner'){
        method = 'XL'
    }  

	if(inputName == 'turnoverML'){
		SVC <- ggsurvplot(fit2, data = dataforPlot, pval = F, conf.int = T,
						  legend.title = '', 
						  legend.labs = c("Non-Follower", "Follower"),
						  legend = "bottom",
						  font.x = c(40, "bold"),
						  font.y = c(25, "bold"),
						  font.tickslab = c(40, "plain"),
						  font.legend = c(40, "bold", "black"),
						  xlim = c(0, 185),
						  break.x.by = 150,
						  break.y.by = 0.5,
						  ylab = c(""),
						  xlab = c("Weeks") 
		) + labs(title = method) + guides(colour = guide_legend(nrow = 1))	
	
	}

	if(inputName == 'hrML'){

		SVC <- ggsurvplot(fit2, data = dataforPlot, pval = F, conf.int = T,
						  legend.title = '', 
						  legend.labs = c("Non-Follower", "Follower"),
						  legend = "bottom",
						  font.x = c(40, "bold"),
						  font.y = c(25, "bold"),
						  font.tickslab = c(40, "plain"),
						  font.legend = c(40, "bold", "black"),
						  xlim = c(0, 12),
						  break.x.by = 10,
						  break.y.by = 0.5,
						  ylab = c(""),
						  xlab = c("Years") 
		) + labs(title = method) + guides(colour = guide_legend(nrow = 1))
    	
	}

    return (SVC$plot +  theme(plot.title = element_text(hjust = 0.5, size=40)))
}

methods <- c('CT', 'TOT', 'TST',  'FT',  'CF', 'Xlearner','DRL', 'PIR')
comGraph <- list()

inputName<- 'turnoverML'
dataforPlot <- NULL

for (method in methods){
    outputBase <- paste(baseFolder, '/output/', method,'/', inputName ,sep='')
    combinedData <- NULL
    inFilePath <- paste (outputBase, '/One/', factor , '/', inputName,'_',method,  '.csv' , sep='')
    combinedData <-read.csv(file = inFilePath)
	SVgp <- getSurvPlot(combinedData, method, topPercent, inputName)
	comGraph <- c(comGraph, list(SVgp))	
}


inputName<- 'hrML'
dataforPlot <- NULL

for (method in methods){
    outputBase <- paste(baseFolder, '/output/', method,'/', inputName ,sep='')
    combinedData <- NULL
    inFilePath <- paste (outputBase, '/One/', factor , '/', inputName,'_',method,  '.csv' , sep='')
    combinedData <-read.csv(file = inFilePath)
	SVgp <- getSurvPlot(combinedData, method, topPercent, inputName)
	comGraph <- c(comGraph, list(SVgp))	
}



outputBase <- paste(baseFolder, '/output/PerformanceEval/' ,sep='')
outFileName <- paste('SurvML', '.png' ,sep='') 
ggsave(paste0(outputBase, outFileName), ggarrange(plotlist=comGraph, common.legend = TRUE, ncol=8, nrow =2, legend='bottom'),
       width=90, height=25,
       units='cm')
	   

