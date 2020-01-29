library(igraph)

# This seeder module infects initial nodes based on their rank provided in one of the input parameters.
# Multiple seeding iterations are possible in order to allow studying sequential seeding approaches.
#
# fractionOfVerticesToInfect - a single value representing the fraction of vertices that need to be seeded
# in each seeding iteration.
#
# seedingIterations - vector of iterations numbers in which the seeder should run.
#
# nodeRanks - vector of node ranks

MultipleRankBasedInfectionSeeder <- setRefClass(
  "MultipleRankBasedInfectionSeeder",
  fields = c('fractionOfVerticesToInfect', 'seedingIterations', 'nodeRanks',
             'logFileName', 'enableLog', 'logLabel'),
  methods = list(
    seed = function(graph, iteration) {
      if (! iteration %in% seedingIterations) {
        return (c());
      }
      # browser();
      
      numVertices <- vcount(graph);
      numVerticesToInfect <- round(fractionOfVerticesToInfect * numVertices);
      # print(paste('Vertices to infect: ', numVerticesToInfect, sep= ''));
      
      verticesNotInfected = V(graph)[isInfected == FALSE & isUsed == FALSE];
      
      ranksOfNotInfectedVertices <- nodeRanks[verticesNotInfected];
      indexesOfVertices <- order(ranksOfNotInfectedVertices, decreasing=FALSE);
      
      verticesToInfect <- verticesNotInfected[indexesOfVertices[1:numVerticesToInfect]];
      
      if (enableLog == TRUE) {
        matrixToWrite <- matrix(ncol=5, nrow = numVerticesToInfect);
        matrixToWrite <- cbind(c(logLabel), iteration, verticesToInfect, indexesOfVertices[verticesToInfect], nodeRanks[verticesToInfect]);
        write.table(matrixToWrite, sep=',', quote=FALSE, append=TRUE, row.names=FALSE, col.names=FALSE, file=logFileName);
      }
      
      return(verticesToInfect);
    }
  )
)
