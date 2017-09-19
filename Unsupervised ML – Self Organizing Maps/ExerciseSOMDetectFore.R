# SOM exercise, foreground detection

# Clear the workspace
rm(list = ls())
# Print line numbers in case of error
options(error=traceback)

# Load required libraries
library("kohonen")
library("tiff")


# Load the images and obtain their size
reference_image = readTIFF("f0001000.tif")
incoming_image = readTIFF("f0001664.tif")
image_size <- dim(reference_image)

patterns <- reference_image
dim(patterns) <- c(image_size[1]*image_size[2],image_size[3])
patterns_test <- incoming_image
dim(patterns_test) <- c(image_size[1]*image_size[2],image_size[3])

colnames(patterns) <- c("R", "G", "B")

# Train the SOM
m <- som(patterns[seq(1,image_size[1]*image_size[2],10),], grid=somgrid(16,16,"hexagonal"))

# Show the SOM as colors
color_palette <- rgb(m$codes)
image(matrix(1:256,nrow=16,ncol=16),col=color_palette)

# Get the quantization errors for all the pixels
quantization_errors <- kohonen::map(m,patterns_test)$distances
dim(quantization_errors) <- c(image_size[1],image_size[2])
image(quantization_errors)




