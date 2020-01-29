library(igraph)

# This seeder module infects initial nodes based on random. Multiple seeding iterations are possible
# in order to allow studying sequential seeding approaches. This is a very simple seeder and works best
# when multitude of simulations is run in order to obtain statistical data. Due to lack of repeatability
# of the random results, it is not suggested to use it for experiments with low quantity of trials.
#
# fractionOfVerticesToInfect - a single value representing the fraction of vertices that need to be seeded
# in each seeding iteration.
#
# seedingIterations - vector of iterations numbers in which the seeder should run.

MultipleRandomInfectionSeeder <- setRefClass(
  "MultipleRandomInfectionSeeder",
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
      verticesToInfect = sample(verticesNotInfected, numVerticesToInfect);
      
      return(verticesToInfect);
    }
  )
)
