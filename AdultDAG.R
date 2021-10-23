library(pcalg)
library(graph)
library(Matching)

baseFolder = getwd()
#######################

inputName<- 'adult'

inBasePath <- paste (baseFolder, '/input/dag', sep='')
inputPath = paste (inBasePath, '/', inputName, '.csv', sep='')
dagData <- read.csv(file = inputPath)
suffStat <- list(C = cor(dagData), n = nrow(dagData))
V<- colnames(dagData)               
print(V)

res <- pc(suffStat, labels = names(dagData), 
          indepTest = gaussCItest, alpha = 0.05) #

mygraph = attr(res, 'graph')
nattrs <- list(node=list(shape="ellipse", fixedsize=FALSE, fontsize=20))
plot(mygraph, attrs=nattrs)

causalMax <- as(mygraph, "matrix")
fullPath <-  paste(baseFolder, '/output/DAGs/' ,inputName, '_dag.csv' ,sep='')
write.csv(causalMax,fullPath, row.names = FALSE)

fullPath <-  paste(baseFolder, '/output/DAGs/' ,inputName, 'ML_dag.csv' ,sep='')
write.csv(causalMax,fullPath, row.names = FALSE)