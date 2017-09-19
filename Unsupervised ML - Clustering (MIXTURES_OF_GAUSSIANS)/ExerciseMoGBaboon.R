# MoG exercise, Baboon image segmentation

# Clear the workspace
rm(list = ls())
# Print line numbers in case of error
options(error=traceback)

# Load required libraries
library("tiff")
library("mclust")

# Load the image and obtain its size
original_image = readTIFF("Baboon.tif")
#original_image = readTIFF("peppers.tif")
image_size <- dim(original_image)

patterns <- original_image
dim(patterns) <- c(image_size[1]*image_size[2],image_size[3])
colnames(patterns) <- c("R", "G", "B")

# Show the original image (converted to grayscale)
image(matrix(apply(patterns,1,mean),image_size[1],image_size[2]),col=gray(0:255/255))

# Choose the optimal MoG on a reduced dataset according to BIC, and apply the
# obtained MoG to the overall dataset
less_patterns <- patterns[seq(1,image_size[1]*image_size[2],100),]
baboonMclust <- Mclust(less_patterns,G=4,prior=priorControl())
overall_prediction <- predict.Mclust(baboonMclust,patterns)
#summary(baboonMclust)
plot(baboonMclust)

# Build and show the segmented image
segmented_image <- overall_prediction$classification
dim(segmented_image) <- c(image_size[1],image_size[2])
image(segmented_image,col=rgb(t(baboonMclust$parameters$mean)))

# Write the segmented image
segmented_colors <- t(baboonMclust$parameters$mean[,overall_prediction$classification])
dim(segmented_colors) <- image_size
writeTIFF(segmented_colors,"segmented.tiff")
