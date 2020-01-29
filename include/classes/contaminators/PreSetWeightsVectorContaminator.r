library(igraph)

# This contaminator module tries to contaminate the nodes that are neighbors to already infected nodes.
#
# Each node is assigned a pre-drawn [0-1] value for comparing with the configured propagation probability
# in order to verify whether or not the contamination should occur (contaminationProbabilityMatrix).
#
# This contaminator allows to adjust the probability of contamination to particular nodes based on their rank.
# This allows to simulate scenarios in which some nodes are provided more incentives than the other nodes,
# in order to increase their propagation probability. In case if such feature is not needed,
# a more basic version of PreSetWeightsContaminator can be used.
#
# nodeContaminationStrategyVector - a vector of <num_nodes> values of [0,1], representing the probability of propagation
#                                      for each node
# contaminationProbabilityMatrix - a matrix containing 4-element rows. Each row consists of:
#           1) from node
#           2) to node
#           3) randomly drawn weight from FROM node to TO node
#           4) randomly drawn weight back
# nodeRanks - ranks of each node, ordered in node order, ie the rank of node 1 is located in nodeRanks[1]

PreSetWeightsVectorContaminator <- setRefClass(
  "PreSetWeightsVectorContaminator",
  fields = c('nodeContaminationStrategyVector', 'contaminationProbabilityMatrix', 'nodeRanks', 'enableLog', 'logFileName', 'logLabel'),
  methods = list(
    contaminate = function(graph, iteration) {
      
      # browser();
      
      contaminationStrategyVectorOrderedByNodes <- numeric(length(nodeContaminationStrategyVector));
      for (i in 1:length(nodeContaminationStrategyVector)) {
        contaminationStrategyVectorOrderedByNodes[i] <- nodeContaminationStrategyVector[nodeRanks[i]];
      }
      
      # fetching nodes that are contaminated and not used, so they can contaminate further if lucky
      contaminatingVertices <- V(graph)[isInfected == TRUE & isUsed == FALSE & infectedAtIteration < iteration];
      
      
      verticesToInfect = c();
      if (length(contaminatingVertices) > 0) {
        # browser();
        # choose only the edges coming from the nodes that can contaminate
        contaminatingVerticesContaminationProbabiliyMatrix <- contaminationProbabilityMatrix[(contaminationProbabilityMatrix[,1] %in% contaminatingVertices), ]
        # browser();
        if (class(contaminatingVerticesContaminationProbabiliyMatrix) != 'matrix') {
          contaminatingVerticesContaminationProbabiliyMatrix <- rbind(contaminatingVerticesContaminationProbabiliyMatrix);
        }
        if (nrow(contaminatingVerticesContaminationProbabiliyMatrix) > 0) {
          edgesWithEnoughProbabilityToContaminate <- NULL;
          for (row in 1:nrow(contaminatingVerticesContaminationProbabiliyMatrix)) {
            contaminationProbabilityMatrixRow <- contaminatingVerticesContaminationProbabiliyMatrix[row, ];
            shouldAddEdge <- contaminationProbabilityMatrixRow[3] < contaminationStrategyVectorOrderedByNodes[ (contaminationProbabilityMatrixRow[[1]]) ];
            if (! is.na(shouldAddEdge) & shouldAddEdge) {
              edgesWithEnoughProbabilityToContaminate <- rbind(edgesWithEnoughProbabilityToContaminate, contaminationProbabilityMatrixRow);
            }
          }
          indicesOfNodesToInfect <- edgesWithEnoughProbabilityToContaminate[,2];
          verticesToInfect <- V(graph)[indicesOfNodesToInfect];
          verticesToInfect <- verticesToInfect[isInfected == FALSE];
          
          # browser();
          
          # remove edges info about the edges which were already infected
          edgesWithEnoughProbabilityToContaminate <- edgesWithEnoughProbabilityToContaminate[edgesWithEnoughProbabilityToContaminate[,2] %in% verticesToInfect,]
        }
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
