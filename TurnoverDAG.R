library(pcalg)
library(graph)

baseFolder = getwd()
#######################
inputName<- 'turnover'
inBasePath <- paste (baseFolder, '/input/dag', sep='')
inputPath = paste (inBasePath, '/', inputName, '.csv', sep='')
dagData <- read.csv(file = inputPath)
dagData <- subset(dagData, select=-c(stag, event))
suffStat <- list(C = cor(dagData), n = nrow(dagData))
V<- colnames(dagData)               
print(V)

##############
myFixedGaps <- matrix(1:169, nrow=13, ncol=13)
myFixedGaps[] <- FALSE 

#Extraversion -> Long_emp_retention
myFixedGaps[11,13] <- TRUE
myFixedGaps[13, 11] <- TRUE


myEdges <- matrix(1:169, nrow=13, ncol=13)
myEdges[] <- FALSE

#Extraversion -> Long_emp_retention
myEdges[8,13] <- TRUE
myEdges[13,8] <- TRUE

#Grey_wage -> Long_emp_retention
myEdges[6,13] <- TRUE
myEdges[13,6] <- TRUE
##################
res <- pc(suffStat, labels = names(dagData), 
          fixedGaps = myFixedGaps, fixedEdges = myEdges, 
          indepTest = gaussCItest, alpha = 0.05) #

mygraph = attr(res, 'graph')
nattrs <- list(node=list(shape="ellipse", fixedsize=FALSE, fontsize=20))
plot(mygraph, attrs=nattrs)

causalMax <- as(mygraph, "matrix")
fullPath <-  paste(baseFolder, '/output/DAGs/' ,inputName, '_dag.csv' ,sep='')
write.csv(causalMax,fullPath, row.names = FALSE)

fullPath <-  paste(baseFolder, '/output/DAGs/' ,inputName, 'ML_dag.csv' ,sep='')
write.csv(causalMax,fullPath, row.names = FALSE)