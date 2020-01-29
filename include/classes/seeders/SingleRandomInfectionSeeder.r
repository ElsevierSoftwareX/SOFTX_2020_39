library(igraph)

# This seeder module infects initial nodes based on random. Only a single seeding iteration is possible.
#
# fractionOfVerticesToInfect - a single value representing the fraction of vertices that need to be seeded
# in each seeding iteration.

SingleRandomInfectionSeeder <- setRefClass(
  "SingleInfectionSeeder",
  fields = c('fractionOfVerticesToInfect'),
  methods = list(
    seed = function(graph, iteration) {
      if (iteration != 1) {
        return (c());
      }
      
      numVertices <- vcount(graph);
      numVerticesToInfect <- round(fractionOfVerticesToInfect * numVertices);
      
      verticesNotInfected = V(graph)[isInfected == FALSE & isUsed == FALSE];
      verticesToInfect = sample(verticesNotInfected, numVerticesToInfect);
      
      return(verticesToInfect);
    }
  )
)
