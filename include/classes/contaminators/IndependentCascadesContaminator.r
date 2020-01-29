library(igraph)

# This contaminator module tries to contaminate the nodes that are neighbors to already infected nodes.
#
# This is a very simple module, which draws the random value [0-1] to compare with the propagation probability value
# on each infection trial. Therefore, this contaminator is useful for some simple tests, but is inadequate
# if repetitive results are required. If using this contaminator, please consider running multitude of repeated
# simulations to obtain average statistic values for your further research.
#
# probabilityOfContamination - a single value representing the average propagation probability for the network.
#                              Each node is assigned this value.

IndependentCascadesContaminator <- setRefClass(
  "IndependentCascadesContaminator",
  fields = c('probabilityOfContamination'),
  methods = list(
    contaminate = function(graph, iteration) {
      # browser();
      contaminatingVertices <- V(graph)[isInfected == TRUE & isUsed == FALSE & infectedAtIteration < iteration];
      verticesToInfect = c();
      if (length(contaminatingVertices) > 0) {
        for (contaminatingVertex in contaminatingVertices) {
          possibleVictims <- neighbors(graph, contaminatingVertex, mode="out");
          possibleVictims <- possibleVictims[isInfected == FALSE]
          if (length(possibleVictims)) {
            for (possibleVictim in possibleVictims) {
              drawValue <- runif(1)
              if (drawValue < probabilityOfContamination) {
                verticesToInfect <- append(verticesToInfect, possibleVictim);
              }
            }
          }
        }
      }
      
      return(list(verticesToInfect=verticesToInfect, verticesUsed=as.vector(contaminatingVertices)));
    }
  )
)
