library(igraph)

ResultCsvPrinter <- setRefClass(
  "ResultCsvPrinter",
  fields = c('targetFileName', 'append', 'columnNames', 'tag'),
  methods = list(
    print = function(graph, iteration) {
      localAppend = append;
      
      # columns: tag, iteration, num vertices, num infected vertices, iteration of last infection, coverage
      if (columnNames == TRUE) {
        columnNamesVector <- c('Tag', 'Iteration', 'Number of Vertices', 'Number of Infected Vertices', 'Iteration of Last Infection', 'Coverage');
        columnNamesMatrix <- as.matrix(t(columnNamesVector));
        write.table(columnNamesMatrix, file = targetFileName, sep = ',', col.names = FALSE, row.names = FALSE, quote = FALSE, append = localAppend);
        localAppend = TRUE;
      }
      
      
      # browser();
      numVertices <- length(V(graph));
      atIteration <- vertex_attr(graph)$infectedAtIteration;
      lastInfectionIteration = max(atIteration);
      numInfected <- length(atIteration[atIteration>0]);
      coverage <- numInfected / numVertices;
      logRow <- c(tag, iteration, numVertices, numInfected, lastInfectionIteration, coverage);
      logRowMatrix <- as.matrix(t(logRow));
      write.table(logRowMatrix, file = targetFileName, sep = ',', col.names = FALSE, row.names = FALSE, quote = FALSE, append = localAppend);
    }
  )
)
