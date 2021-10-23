library(pcalg)
library(graph)
library(Matching)

baseFolder = getwd()
#######################
inputName<- 'hr'

inBasePath <- paste (baseFolder, '/input/dag', sep='')
inputPath = paste (inBasePath, '/', 'hr', '.csv', sep='')
dagData <- read.csv(file = inputPath)

dagData <- subset(dagData, select=-c(left))

print(nrow(dagData))


suffStat <- list(C = cor(dagData), n = nrow(dagData))
V<- colnames(dagData)               
print(V)

myEdges <- matrix(1:36, nrow=6, ncol=6)
myEdges[] <- FALSE

myEdges[6,1] <- TRUE
myEdges[1,6] <- TRUE

myEdges[5,1] <- TRUE
myEdges[1,5] <- TRUE

##################
res <- pc(suffStat, labels = names(dagData), fixedEdges = myEdges,
          indepTest = gaussCItest, alpha = 0.05) #

mygraph = attr(res, 'graph')
nattrs <- list(node=list(shape="ellipse", fixedsize=FALSE, fontsize=20))


plot(mygraph, attrs=nattrs)

causalMax <- as(mygraph, "matrix")
fullPath <-  paste(baseFolder, '/output/DAGs/' ,inputName, '_dag.csv' ,sep='')
write.csv(causalMax,fullPath, row.names = FALSE)

fullPath <-  paste(baseFolder, '/output/DAGs/' ,inputName, 'ML_dag.csv' ,sep='')
write.csv(causalMax,fullPath, row.names = FALSE)