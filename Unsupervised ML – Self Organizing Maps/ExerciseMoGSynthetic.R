# MoG exercise, synthetic dataset

# Clear the workspace
rm(list = ls())
# Print line numbers in case of error
options(error=traceback)

# Load required libraries
library("mclust")
library("fpc")

# Generate the synthetic dataset
RealNumClusters <- 5
ClusterSize <- 100
RealClusterCenters <- runif(2*RealNumClusters,min=-5,max=5)
dim(RealClusterCenters) <- c(RealNumClusters,2)
RealStdDevs <- runif(RealNumClusters,min=0.05, max=0.2)
patterns <- rep(0,2*RealNumClusters*ClusterSize)
dim(patterns) <- c(RealNumClusters*ClusterSize,2)
for(NdxCluster in 1:RealNumClusters)
{
  mypatterns <- rnorm(2*ClusterSize, sd=RealStdDevs[NdxCluster])
  dim(mypatterns) <- c(ClusterSize,2)
  mypatterns[,1] <- rep(RealClusterCenters[NdxCluster,1],ClusterSize)+mypatterns[,1]
  mypatterns[,2] <- rep(RealClusterCenters[NdxCluster,2],ClusterSize)+mypatterns[,2]
  patterns[((NdxCluster-1)*ClusterSize+1):(NdxCluster*ClusterSize),] <- mypatterns
}
plot(patterns[,1],patterns[,2])

# Choose the optimal MoG according to BIC
syntheticMclust <- Mclust(patterns)
summary(syntheticMclust)
plot(syntheticMclust)


# PROCESSING WITH fpc PACKAGE
bic_info <- mclustBIC(patterns)
mysummary <- summary(bic_info,patterns)
zc <- mergenormals(patterns,mysummary,method="demp")
plotcluster(patterns,zc$clustering)
