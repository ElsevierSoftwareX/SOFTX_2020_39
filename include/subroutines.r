loadContaminationProbabilityMatrix <- function (contaminationProbabilityPath)
{
  bidirectionalContaminationProbabilityMatrix = read.table(contaminationProbabilityPath, header=FALSE, sep=" ");
  if (ncol(bidirectionalContaminationProbabilityMatrix) != 4) {
    stop("Weights matrix should have 4 columns.");
  }
  
  # uncomment only if nodes numbers start with 0
  #bidirectionalContaminationProbabilityMatrix[,1] <- bidirectionalContaminationProbabilityMatrix[,1] + 1;
  #bidirectionalContaminationProbabilityMatrix[,2] <- bidirectionalContaminationProbabilityMatrix[,2] + 1;
  
  numRows <- nrow(bidirectionalContaminationProbabilityMatrix);
  contaminationProbabilityMatrix <- matrix(nrow = numRows * 2, ncol = 3, byrow = TRUE);
  for (i in 1:numRows) {
    contaminationProbabilityMatrix[i,] <- c(bidirectionalContaminationProbabilityMatrix[i,1], bidirectionalContaminationProbabilityMatrix[i,2], bidirectionalContaminationProbabilityMatrix[i,3]);
    contaminationProbabilityMatrix[numRows + i,] <- c(bidirectionalContaminationProbabilityMatrix[i,2], bidirectionalContaminationProbabilityMatrix[i,1], bidirectionalContaminationProbabilityMatrix[i,4]);
  }
  
  return (contaminationProbabilityMatrix);
}
