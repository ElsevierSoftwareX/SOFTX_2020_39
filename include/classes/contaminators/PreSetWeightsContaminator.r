library(igraph)

# This contaminator module tries to contaminate the nodes that are neighbors to already infected nodes.
#
# Each node is assigned a pre-drawn [0-1] value for comparing with the configured propagation probability
# in order to verify whether or not the contamination should occur (contaminationProbabilityMatrix).
#
# This contaminator uses a single average value of propagation probability for all nodes. If there is a need
# to simulate scenarios in which some nodes are provided more incentives than the other nodes,
# in order to increase their propagation probability, a more advanced version of PreSetWeightsVectorContaminator
# can be used.
#
# probabilityOfContamination - a single value representing the average propagation probability for the network.
#                              Each node is assigned this value.
# contaminationProbabilityMatrix - a matrix containing 4-element rows. Each row consists of:
#           1) from node
#           2) to node
#           3) randomly drawn weight from FROM node to TO node
#           4) randomly drawn weight back

PreSetWeightsContaminator <- setRefClass(
  "PreSetWeightsContaminator",
  fields = c('probabilityOfContamination', 'contaminationProbabilityMatrix', 'enableLog', 'logFileName', 'logLabel'),
  methods = list(
    contaminate = function(graph, iteration) {
      # browser();
      contaminatingVertices <- V(graph)[isInfected == TRUE & isUsed == FALSE & infectedAtIteration < iteration];
      verticesToInfect = c();
      if (length(contaminatingVertices) > 0) {
        edgesWithEnoughProbabilityToContaminate <- contaminationProbabilityMatrix[(contaminationProbabilityMatrix[,1] == contaminatingVertices & contaminationProbabilityMatrix[,3] < probabilityOfContamination),];
        indicesOfNodesToInfect <- edgesWithEnoughProbabilityToContaminate[,2];
        verticesToInfect <- V(graph)[indicesOfNodesToInfect];
        verticesToInfect <- verticesToInfect[isInfected == FALSE];
        edgesWithEnoughProbabilityToContaminate <- edgesWithEnoughProbabilityToContaminate[edgesWithEnoughProbabilityToContaminate[,2] %in% verticesToInfect,]
      }
      
      numVerticesToInfect <- length(verticesToInfect);
      
      if (enableLog == TRUE & numVerticesToInfect) {
        logLabelLocal <- paste(logLabel, iteration, sep=",");
        matrixToWrite <- matrix(ncol=5, nrow = numVerticesToInfect);
        # browser();
        matrixToWrite <- cbind(c(logLabelLocal), edgesWithEnoughProbabilityToContaminate);
        write.table(matrixToWrite, sep=',', quote=FALSE, append=TRUE, row.names=FALSE, col.names=FALSE, file=logFileName);
      }
      
      return(list(verticesToInfect=verticesToInfect, verticesUsed=as.vector(contaminatingVertices)));
    }
  )
)
