library(igraph)

InfectionRunner <- setRefClass(
  "InfectionRunner",
  fields = c('graph', 'vertices', 'currentIteration', 'infectionSeeder', 'contaminator', 'resultPrinter'),
  methods = list(
    setGraph = function(fgraph) {
      graph <<- fgraph;
    },
    
    init = function() {
      currentIteration <<- 0;
      if (class(graph) == 'igraph') {
        V(graph)$isInfected <<- FALSE;
        V(graph)$infectedAtIteration <<- getCurrentIteration();
        V(graph)$isUsed <<- FALSE;
      }
    },
    
    isInitialized = function() {
      if (class(graph) == 'igraph') {
        return(TRUE);
      }
      return(FALSE);
    },
    
    # reads graph from CSV file with two fieds: To, From (integer values)
    readFromEdgesCsv = function(path, directed = FALSE)
    {
      edges <- read.csv(file = path, sep = ',', header = TRUE, as.is = TRUE);
      edgesMatrix <- as.matrix(edges);
      net <- graph_from_edgelist(edgesMatrix, directed = directed);
      setGraph(net);
      init();
    },
    
    # reads graph from TXT file with two fieds: To From (integer values)
    readFromEdgesTxt = function(path, directed = FALSE)
    {
      # browser();
      edges <- read.csv(file = path, sep = " ", header = FALSE, as.is = TRUE);
      edgesMatrix <- as.matrix(edges);
      if (ncol(edgesMatrix) != 2) {
        stop("Edges file should have two columns.");
      }
      if (min(edgesMatrix[,1]) == 0) {
        edgesMatrix[,1] = edgesMatrix[,1] + 1;
        edgesMatrix[,2] = edgesMatrix[,2] + 1;
      }
      net <- graph_from_edgelist(edgesMatrix, directed = directed);
      setGraph(net);
      init();
    },
    
    getNumInfected = function() {
      if (! isInitialized()) {
        return(0);
      }
      numInfected <- length(V(graph)[isInfected == TRUE])
      return(numInfected);
    },
    
    getVertices = function() {
      if (! isInitialized()) {
        return(NULL);
      }
      lvertices <- V(.self$graph);
      return(lvertices);
    },
    
    getNumVertices = function() {
      if (! isInitialized()) {
        return(0);
      }
      return(length(getVertices()));
    },
    
    getEdges = function() {
      if (! isInitialized()) {
        return(NULL);
      }
      edges <- E(.self$graph);
      return(edges);
    },
    
    getCurrentIteration = function() {
      return(.self$currentIteration)
    },
    
    plotCurrentGraph = function() {
      if (! isInitialized()) {
        return ();
      }
      plot(.self$graph, 
           layout=layout.kamada.kawai,
           vertex.size=15,
           vertex.color = c('gray', 'green', 'blue')[1+(.self$getVertices()$isInfected == TRUE)+(.self$getVertices()$isUsed == TRUE)]
      )
    },
    
    # vertices - vector of vertices to infect
    infect = function(verticesToInfect) {
      if (is.numeric(verticesToInfect)) {
        verticesToInfect <- c(verticesToInfect);
      }
      V(graph)[verticesToInfect]$isInfected <<- TRUE;
      V(graph)[verticesToInfect]$infectedAtIteration <<- getCurrentIteration();
    },
    
    # vertices - vector of vertices to infect
    markAsUsed = function(verticesToMarkAsUsed) {
      # browser();
      if (is.numeric(verticesToMarkAsUsed)) {
        verticesToMarkAsUsed <- c(verticesToMarkAsUsed);
      }
      V(graph)[verticesToMarkAsUsed]$isUsed <<- TRUE;
    },
    
    run = function(numIterations, minIterations = 1) {
      for (i in 1: numIterations) {
        # browser();
        currentIteration <<- i;
        
        # seeding phase
        verticesToSeed <- infectionSeeder$seed(graph, currentIteration);
        if (length(verticesToSeed) > 0) {
          infect(verticesToSeed);
        }
        
        # contamination phase
        contaminationResultList <- contaminator$contaminate(graph, currentIteration);
        # browser();
        verticesToInfect <- contaminationResultList$verticesToInfect;
        if (length(verticesToInfect) > 0) {
          infect(verticesToInfect)
        }
        # browser();
        verticesUsed <- contaminationResultList$verticesUsed;
        if (length(verticesUsed) > 0) {
          markAsUsed(verticesUsed)
        }
        
        
        # if (currentIteration == 1) {
        #   plotCurrentGraph();
        #   # Sys.sleep(1);
        # }
        
        
        if (length(verticesToSeed) == 0 & length(verticesToInfect) == 0 && currentIteration > minIterations) {
          break;
        }
      }
      resultPrinter$print(graph, currentIteration);
      
    }
  )
)
