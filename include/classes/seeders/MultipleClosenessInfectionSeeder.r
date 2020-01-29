library(igraph)

# This seeder module infects initial nodes based on their closeness rank. Multiple seeding iterations are possible
# in order to allow studying sequential seeding approaches.
#
# fractionOfVerticesToInfect - a single value representing the fraction of vertices that need to be seeded
# in each seeding iteration.
#
# seedingIterations - vector of iterations numbers in which the seeder should run.

MultipleClosenessInfectionSeeder <- setRefClass(
  "MultipleClosenessInfectionSeeder",
  fields = c('fractionOfVerticesToInfect', 'seedingIterations'),
  methods = list(
    seed = function(graph, iteration) {
      if (! iteration %in% seedingIterations) {
        return (c());
      }
      # browser();
      
      numVertices <- vcount(graph);
      numVerticesToInfect <- round(fractionOfVerticesToInfect * numVertices);
      
      verticesNotInfected = V(graph)[isInfected == FALSE & isUsed == FALSE];
      
      closenessOfVertices <- closeness(graph);
      closenessOfNotInfectedVertices <- closenessOfVertices[verticesNotInfected];
      indexesOfVertices <- order(closenessOfNotInfectedVertices, decreasing=FALSE);
      
      verticesToInfect <- verticesNotInfected[indexesOfVertices[1:numVerticesToInfect]];
      
      return(verticesToInfect);
    }
  )
)
