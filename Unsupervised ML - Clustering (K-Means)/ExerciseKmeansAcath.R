# Kmeans exercise, Acath dataset

# Clear the workspace
rm(list = ls())
# Print line numbers in case of error
options(error=traceback)

# Load required libraries
library(clValid)
library(fpc)

# Read the dataset
datos_completos <- read.csv('acath.csv',header=TRUE,sep=';',na.strings="")

# Select some attributes, and remove the samples with NAs
patterns <- na.omit(datos_completos[c("age","cad.dur","choleste")])

# PROCESSING WITH clValid PACKAGE
# Study the validity of the obtained clusters for clusterings from 2 to 10 clusters
internal_val <- clValid(patterns, 2:10, clMethods=c("kmeans"),
                  validation="internal")
optimal_results <- optimalScores(internal_val)
# Plot the results of the cluster validation
summary(internal_val)
plot(internal_val)
# Execute k-means with the optimal number of clusters according to silhouette index
NumClusters <- as.integer(as.matrix(optimal_results['Silhouette','Clusters']))
kc <- kmeans(patterns, NumClusters, 30, 10)
# Plot the clustering results according to the original attributes
mycolors <- rainbow(NumClusters)
plot(patterns[c("cad.dur","choleste")], col=mycolors[kc$cluster])
points(kc$centers[,c("cad.dur","choleste")], col=rainbow(NumClusters), pch=15, cex=1.5)
pairs(rbind(patterns,kc$centers),col=c(mycolors[kc$cluster],rainbow(NumClusters)),
      pch=c(rep(1,nrow(patterns)),rep(15,NumClusters)))

# PROCESSING WITH fpc PACKAGE
# Run the k-means algorithm with automatic selection of the number of clusters
zc <- kmeansruns(patterns)
# Plot the clustering on discriminant coordinates
plotcluster(patterns,zc$cluster)
# Plot the clustering results according to the original attributes
NumClusters2 <- zc$bestk
mycolors2 <- rainbow(NumClusters2)
plot(patterns[c("cad.dur","choleste")], col=mycolors2[zc$cluster])
points(zc$centers[,c("cad.dur","choleste")], col=rainbow(NumClusters2), pch=15, cex=1.5)
pairs(rbind(patterns,zc$centers),col=c(mycolors2[zc$cluster],rainbow(NumClusters2)),
      pch=c(rep(1,nrow(patterns)),rep(15,NumClusters2)))
plot(zc$crit)
