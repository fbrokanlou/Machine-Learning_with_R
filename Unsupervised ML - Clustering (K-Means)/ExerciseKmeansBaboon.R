# Kmeans exercise, segmentation of Baboon image

# Clear the workspace
rm(list = ls())
# Print line numbers in case of error
options(error=traceback)

# Load required libraries
library("tiff")
library("clValid")
library("fpc")

# Load the image and obtain its size
original_image = readTIFF("Baboon.tif")
#original_image = readTIFF("lenna.tif")
image_size <- dim(original_image)

# Get the training patterns
patterns <- original_image
dim(patterns) <- c(image_size[1]*image_size[2],image_size[3])
colnames(patterns) <- c("R", "G", "B")

# Show the original image (converted to grayscale)
image(matrix(apply(patterns,1,mean),image_size[1],image_size[2]),col=gray(0:255/255))

# Study the validity of the clusters obtained with 2 to 10 clusters over 
# a subset of the input image
patterns_small <- patterns[seq(1,image_size[1]*image_size[2],1000),]
internal_val <- clValid(patterns_small, 2:10, clMethods=c("kmeans"),
                  validation="internal")
optimal_scores <- optimalScores(internal_val)
# View results of cluster validation
summary(internal_val)
plot(internal_val)

# Run k-means with the optimal number of clusters according to Dunn index
NumClusters <- as.integer(as.matrix(optimal_scores['Dunn','Clusters']))
cl <- kmeans(patterns, NumClusters, 20, 5)

# Build and show the segmented image
segmented_image <- cl$cluster
dim(segmented_image) <- c(image_size[1],image_size[2])
image(segmented_image,col=rgb(cl$centers))

