library(igraph)

# This seeder module infects initial nodes based on their degree rank. Multiple seeding iterations are possible
# in order to allow studying sequential seeding approaches.
#
# fractionOfVerticesToInfect - a single value representing the fraction of vertices that need to be seeded
# in each seeding iteration.
#
# seedingIterations - vector of iterations numbers in which the seeder should run.

MultipleDegreeInfectionSeeder <- setRefClass(
  "MultipleDegreeInfectionSeeder",
  fields = c('fractionOfVerticesToInfect', 'seedingIterations', 
             'logFileName', 'enableLog', 'logLabel'),
  methods = list(
    seed = function(graph, iteration) {
      if (! iteration %in% seedingIterations) {
        return (c());
      }
      # browser();
      
      numVertices <- vcount(graph);
      numVerticesToInfect <- round(fractionOfVerticesToInfect * numVertices);
      
      verticesNotInfected = V(graph)[isInfected == FALSE & isUsed == FALSE];
      
      degreeOfVertices <- degree(graph);
      degreeOfNotInfectedVertices <- degreeOfVertices[verticesNotInfected];
      indexesOfVertices <- order(degreeOfNotInfectedVertices, decreasing=TRUE);
      
      verticesToInfect <- verticesNotInfected[indexesOfVertices[1:numVerticesToInfect]];
      
      if (enableLog == TRUE) {
        matrixToWrite <- matrix(ncol=4, nrow = numVerticesToInfect);
        matrixToWrite <- cbind(c(logLabel), verticesToInfect, indexesOfVertices[verticesToInfect], degreeOfVertices[verticesToInfect]);
        write.table(matrixToWrite, sep=',', quote=FALSE, append=TRUE, row.names=FALSE, col.names=FALSE, file=logFileName);
      }
      
      return(verticesToInfect);
    }
  )
)
