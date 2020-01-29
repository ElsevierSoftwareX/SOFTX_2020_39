library(igraph)

# set working directory to the location of this file
WORKING_DIRECTORY <- "C:/Users/.../oonis/scenarios/example";
setwd(WORKING_DIRECTORY);

source("../../include/autoload.r")
source("../../include/subroutines.r")

enableLog <- TRUE
inputNetworkDirectory <- '../../networks/email';
outputDirectory <- '../../output/03-email-multiple-simulations';
logDirectory <- '../../logs/03-email-multiple-simulations';
networkPrefix <- 'email';

minIterations <- 1;
maxIterations <- 120;

seedingFractions <- c(0.1, 0.2);
seedingIterations <- c(1);
propagationProbabilities <- c(0.1, 0.2);
numberOfNetworkNodes <- 143;

# load ranking from /rankings/ directory. See utils/ranks-weights-generation.r
rankingPath <- paste(inputNetworkDirectory, '/rankings/', networkPrefix, '_ranking_dg.txt', sep='');
ranks <- loadNodeRanksFromFile(rankingPath);

for (seedingFraction in seedingFractions) {
  print(paste('seeding fraction: ', seedingFraction, sep=''));
  for (propagationProbability in propagationProbabilities) {
    print(paste('propagation probability: ', propagationProbability, sep=''));
    
      # The loaded probabilities are the actual probabilities drawn for this experiment. See utils/ranks-weights-generation.r
      for (weight in 1:10) {
        print(paste('weight: ', weight, sep=''));

        
        # add experiment label for logs, in case multiple outputs are logged to a single file
        experimentLabel <- paste("Emails 10 Runs", date(), "sf:", seedingFraction, "pp:", propagationProbability, "w:", weight);

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
        averageProbabilitiesOfContamination <- rep(propagationProbability, numberOfNetworkNodes);

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
        targetFileName = paste(outputDirectory, 'email.csv', sep='/'),
        append = TRUE,
        columnNames = FALSE,
        tag = experimentLabel
        );


        # SIMULATION
        ir <- InfectionRunner(infectionSeeder = seeder, contaminator = contaminator, resultPrinter = resultPrinter)
        ir$readFromEdgesTxt(paste(inputNetworkDirectory, '/', networkPrefix, '.txt', sep=''), FALSE)
        ir$run(maxIterations, minIterations)




      }
  }
}



