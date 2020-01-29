library(igraph)

# set working directory to the location of this file
WORKING_DIRECTORY <- "C:/Users/.../oonis/scenarios/example";
setwd(WORKING_DIRECTORY);

source("../../include/autoload.r")
source("../../include/subroutines.r")

enableLog <- TRUE
inputNetworkDirectory <- '../../networks/tribes';
outputDirectory <- '../../output/02-tribes-sequential';
logDirectory <- '../../logs/02-tribes-sequential';
networkPrefix <- 'tribes';

minIterations <- 1;
maxIterations <- 120;

seedingFraction <- 0.125;
seedingIterations <- c(1,2);
averagePropagationProbability <- 0.2;
numberOfNetworkNodes <- 16;

# load ranking from /rankings/ directory. See utils/ranks-weights-generation.r
rankingPath <- paste(inputNetworkDirectory, '/rankings/', networkPrefix, '_ranking_dg.txt', sep='');
ranks <- loadNodeRanksFromFile(rankingPath);

# add experiment label for logs, in case multiple outputs are logged to a single file
experimentLabel <- paste("Tribes Single Run - Sequential", date());

# SEEDER

# choose and configure the simulation seeder component
seeder <- MultipleRankBasedInfectionSeeder(
  fractionOfVerticesToInfect = seedingFraction,
  seedingIterations = seedingIterations,
  nodeRanks = ranks,
  logFileName = paste(logDirectory, 'seeder.log', sep='/'),
  enableLog = enableLog,
  logLabel = experimentLabel
);

# CONTAMINATOR

# the contaminator is able to simulate incentives' impact on affecting propagation probability on network nodes
averageProbabilitiesOfContamination <- rep(averagePropagationProbability, numberOfNetworkNodes);

# In this example only 1 set of weights is simulated, therefore the file with the drawn weights needs to be chosen
# The loaded probabilities are the actual probabilities drawn for this experiment. See utils/ranks-weights-generation.r
weight <- 1;
actualContaminationProbabilityPath <- paste(inputNetworkDirectory, '/weights/', networkPrefix, '_', weight, '.txt', sep='');
actualContaminationProbabilityMatrix <- loadContaminationProbabilityMatrix(actualContaminationProbabilityPath);

# choose and configure the simulation contamination component
contaminator <- PreSetWeightsVectorContaminator(
  nodeContaminationStrategyVector = averageProbabilitiesOfContamination,
  contaminationProbabilityMatrix = actualContaminationProbabilityMatrix,
  nodeRanks = ranks,
  logFileName = paste(logDirectory, 'contaminator.log', sep='/'),
  enableLog = enableLog,
  logLabel = experimentLabel
)


# RESULT PRINTER
resultPrinter <- ResultCsvPrinter(
  targetFileName = paste(outputDirectory, 'tribes.csv', sep='/'),
  append = TRUE,
  columnNames = FALSE,
  tag = experimentLabel
);


# SIMULATION
ir <- InfectionRunner(infectionSeeder = seeder, contaminator = contaminator, resultPrinter = resultPrinter)
ir$readFromEdgesTxt(paste(inputNetworkDirectory, '/', networkPrefix, '.txt', sep=''), FALSE)
ir$run(maxIterations, minIterations)

