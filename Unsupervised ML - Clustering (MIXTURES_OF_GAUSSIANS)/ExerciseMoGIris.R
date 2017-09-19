# MoG exercise, Iris dataset

# Clear the workspace
rm(list = ls())
# Print line numbers in case of error
options(error=traceback)

# Load required library
library("mclust")


# Read the dataset
newiris <- iris
patterns <- newiris[c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]


# Choose the optimal MoG according to BIC
modelMclust <- Mclust(patterns,prior=priorControl())
plot(modelMclust)
summary(modelMclust)


