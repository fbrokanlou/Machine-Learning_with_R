# Kmeans exercise, synthetic dataset

# Clear the workspace
rm(list = ls())
# Print line numbers in case of error
options(error=traceback)

# Load the required libraries
library(clValid)
library(fpc)

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

# PROCESSING WITH clValid PACKAGE
# Study the validity of the obtained clusters for clusterings from 2 to 10 clusters
internal_val <- clValid(patterns, 2:10, clMethods=c("kmeans"),
                  validation="internal")
optimal_results <- optimalScores(internal_val)
# Plot the results of the cluster validation
summary(internal_val)
plot(internal_val)
# Execute k-means with the optimal number of clusters according to silhouette value
NumClusters <- as.integer(as.matrix(optimal_results['Silhouette','Clusters']))
kc <- kmeans(patterns, NumClusters)
# Plot the clustering results
plot(patterns[,1],patterns[,2], col=kc$cluster, pch=".")
points(kc$centers, col=1:NumClusters, pch=15, cex=0.8)

# PROCESSING WITH fpc PACKAGE
# Run the k-means algorithm with automatic selection of the number of clusters
zc <- kmeansruns(patterns)
# Plot the clustering on discriminant coordinates
plotcluster(patterns,zc$cluster)
# Plot the clustering results according to the original attributes
NumClusters2 <- zc$bestk
mycolors2 <- rainbow(NumClusters2)
pairs(rbind(patterns,zc$centers),col=c(mycolors2[zc$cluster],rainbow(NumClusters2)),
      pch=c(rep(1,nrow(patterns)),rep(15,NumClusters2)))
plot(zc$crit)