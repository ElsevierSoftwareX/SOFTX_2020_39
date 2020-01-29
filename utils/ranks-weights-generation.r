# CONFIGURATION

# provide path to the network directory, e.g. /oonis/networks/tribes
SOURCE_NETWORK_DIR <- "C:/Users/.../oonis/networks/tribes";

# provide network filename (without .txt), e.g. tribes
SOURCE_NETWORK_NAME = 'tribes'

# possibility to generate weights, measures or both
GENERATE_WEIGHTS <- TRUE; # draw contamination probabilities used later in IC model
GENERATE_MEASURES <- TRUE; # obtain degree, eigenvector and betweenness for all nodes of the network

# END OF CONFIGURATION

library(netdep)
library(igraph)
library(tictoc)
library(plyr)

setwd(SOURCE_NETWORK_DIR)


weights <- function(networkName){
    for (idx in 1:10) {
        networkFileName = paste(networkName,".txt",sep = "")
        dat1 <- read.csv(networkFileName,header=FALSE,sep=" ")
        colnames(dat1)[1] <- "a"
        colnames(dat1)[2] <- "b"
        dat1$rnd1 <- sample(100, size = nrow(dat1), replace = TRUE)
        dat1$rnd2 <- sample(100, size = nrow(dat1), replace = TRUE)
        dat1$rnd1 <- dat1$rnd1/100
        dat1$rnd2 <- dat1$rnd2/100
        write.table(dat1, paste("weights/",networkName,"_",idx,".txt",sep=""), sep=" ", row.names=FALSE, col.names=FALSE)
    }
}

measures <- function(net){

    g1 <- read.graph(paste(net,".txt",sep=""), format="edgelist")
    numofnodes = vcount(g1)
    numofedges = ecount(g1)

    measure1 <- degree(g1)
    m2 <- evcent(g1, directed = FALSE,weights = NULL)
    measure2 <- data.frame(m2[1])
    measure3 <- betweenness(g1, v=V(g1), directed = FALSE, weights = NULL,  nobigint = TRUE, normalized = FALSE)

    nodeid<-measure1

    for(j in 1:numofnodes)
    {
        nodeid[ j ] = j - 1
    }

    write.csv(measure1,    file = paste("measures/",net,"_dg.txt",sep=""),row.names=FALSE)
    write.csv(measure2,    file = paste("measures/",net,"_ev.txt",sep=""),row.names=FALSE)
    write.csv(measure3,    file = paste("measures/",net,"_bt.txt",sep=""),row.names=FALSE)

    file = paste("measures/",net,"_dg.txt",sep="");
    fx = read.csv(file, header = TRUE, sep = "")
    nr = nrow(fx)
    nr = nr - 1
    node <- rep(0:nr)
    z <- cbind(node,fx)
    z <- z[order(z$x, decreasing = TRUE),]
    nr = nr + 1
    rank <- rep(1:nr)
    z <- cbind(z,rank)
    write.table(z, file = paste("rankings/",net,"_ranking_dg.txt",sep=""),row.names=FALSE,col.names=FALSE,sep=" ")

    file = paste("measures/",net,"_ev.txt",sep="");
    fx = read.csv(file, header = TRUE, sep = "")
    nr = nrow(fx)
    nr = nr - 1
    node <- rep(0:nr)
    z <- cbind(node,fx)
    z <- z[order(z$vector, decreasing = TRUE),]
    nr = nr + 1
    rank <- rep(1:nr)
    z <- cbind(z,rank)
    write.table(z, file = paste("rankings/",net,"_ranking_ev.txt",sep=""),row.names=FALSE,col.names=FALSE,sep=" ")

    file = paste("measures/",net,"_bt.txt",sep="");
    fx = read.csv(file, header = TRUE, sep = "")
    nr = nrow(fx)
    nr = nr - 1
    node <- rep(0:nr)
    z <- cbind(node,fx)
    z <- z[order(z$x, decreasing = TRUE),]
    nr = nr + 1
    rank <- rep(1:nr)
    z <- cbind(z,rank)
    write.table(z, file = paste("rankings/",net,"_ranking_bt.txt",sep=""),row.names=FALSE,col.names=FALSE,sep=" ")

    file = paste("measures/",net,"_dg.txt",sep="");
    fx = read.csv(file, header = TRUE, sep = "")
    nr = nrow(fx)
    nr = nr - 1
    node <- rep(0:nr)
    z <- cbind(node,fx)
    z$rnd <- sample(10000, size = nrow(z), replace = TRUE)
    z$rnd <- z$rnd/100
    z <- z[order(z$rnd, decreasing = TRUE),]
    nr = nr + 1
    rank <- rep(1:nr)
    z <- cbind(z,rank)
    z  <- subset(z, select = c(node, rnd,rank))
    write.table(z, file = paste("rankings/",net,"_ranking_rn.txt",sep=""),row.names=FALSE,col.names=FALSE,sep=" ")
}


if (GENERATE_WEIGHTS == TRUE) {
    weights(SOURCE_NETWORK_NAME);
}

if (GENERATE_MEASURES == TRUE) {
    measures(SOURCE_NETWORK_NAME);
}
