require(ggplot2)
require(tidyverse)

require("mongolite")
# 
m <- mongo(collection = "Experiments", db = "ShallowMind", url = "mongodb://rkapur2021:okay4kench635shallowmind.pingry.org:27017")

# datasets <- read.csv(file="C:/Users/emmet/Desktop/NeuralNetworks/datasets.csv")
experiments <- read.csv(file="C:/Users/emmet/Desktop/NeuralNetworks/experiments.csv")
# neural_nets <- read.csv(file="C:/Users/emmet/Desktop/NeuralNetworks/neuralNets.csv")

# example. structure [1,1,1] so cubic

# structure = experiments[7,3]
# training_over_time = as.character(experiments[7,8])
# training_over_time = gsub("\\[|\\]", "", training_over_time)
# training_over_time = 1 - (as.numeric(strsplit(training_over_time, ",")[[1]]))

convert <- function(x) {
  training_over_time = as.character(x)
  training_over_time = gsub("\\[|\\]", "", training_over_time)
  training_over_time = 1 - (as.numeric(strsplit(training_over_time, ",")[[1]]))
  abs(training_over_time)
}

convertShapes <- function(x) {
  training_over_time = as.character(x)
  training_over_time = gsub("\\[|\\]", "", training_over_time)
  training_over_time = as.numeric(strsplit(training_over_time, ",")[[1]])
  abs(training_over_time)
}

experiments$trainingAccuracyOverTime <- lapply(experiments$trainingLossOverTime, convert)
experiments$validationAccuracyOverTime <- lapply(experiments$validationLossOverTime, convert)
experiments$neuralNetHiddenStructure <- lapply(experiments$neuralNetHiddenStructure, convertShapes)
experiments$inputShape <- as.numeric(experiments$inputShape)
experiments$outputShape <- as.numeric(experiments$outputShape)

row <- 1110
training <- data.frame(epoch = seq(1,length(experiments$trainingAccuracyOverTime[[row]]), 1), trainAcc = experiments$trainingAccuracyOverTime[[row]]) 
validation <- data.frame(epoch = seq(1,length(experiments$validationAccuracyOverTime[[row]]), 1), valAcc = experiments$validationAccuracyOverTime[[row]]) 
graph <- ggplot() + 
  geom_line(data = training, aes(x = epoch, y = trainAcc), color = "blue") +
  geom_line(data = validation, aes(x = epoch, y = valAcc), color = "red") +
  labs(title=, x="Epoch", y="Accuracy") # Adding scatterplot geom (layer1) and smoothing geom (layer2).
graph

epochToAccuracy <- function(epoch, shape) {
  rowIndex <- Position(function(x) identical(x, shape), experiments$neuralNetHiddenStructure)
  accuracyList <- experiments$trainingAccuracyOverTime[[rowIndex]]
  if (epoch > 0 & epoch <= length(accuracyList)) { 
    return(accuracyList[epoch])
  }
  else { 
    return(-1)
  }
}

mean_connectivity <- function(struct) {
  copy <- sapply(struct, function(i) i)
  struct <- struct[-1]
  copy <- copy[-length(copy)]
  a <- mean(copy*struct)
  return(a)
}

mean_diff_over_time <- function(vector) {
  vector <- unlist(vector)
  diffs <- vector[-1] - head(vector, -1)
  mean(diffs)
}

std_diff_over_time <- function(vector){
  vector <- unlist(vector)
  diffs <- vector[-1] - head(vector, -1)
  sd(diffs)
}

experiments$avg_training_rate <- lapply(experiments$trainingAccuracyOverTime, mean_diff_over_time)
experiments$training_volatility <- lapply(experiments$trainingAccuracyOverTime, std_diff_over_time)

# Coefficient of overfitting for an entire neural net 
# Returns list of 2 elements: first, coefficient of overfitting for neural net over the course of the epochs, and then, average coefficient of overfitting (so average over the epochs)
coeff_overfit_nn <- function(shape) {
  rowIndex <- Position(function(x) identical(x, shape), experiments$neuralNetHiddenStructure)
  
  training_accuracy_per_row <- experiments$trainingAccuracyOverTime[[rowIndex]]
  validation_accuracy_per_row <- experiments$validationAccuracyOverTime[[rowIndex]]
  coeff_over_epochs <- training_accuracy_per_row - validation_accuracy_per_row
  return(list(coeff_over_epochs, mean(coeff_over_epochs)))
}

coeff_overfit_epoch <- function(epoch, shape) {
  rowIndex <- Position(function(x) identical(x, shape), experiments$neuralNetHiddenStructure)
  training_accuracy_per_row <- experiments$trainingAccuracyOverTime[[rowIndex]]
  validation_accuracy_per_row <- experiments$validationAccuracyOverTime[[rowIndex]]
  overfit = training_accuracy_per_row - validation_accuracy_per_row
  abs(overfit)
}

# coeff_overfit_nn(c(1,0))
experiments$overfitting <- lapply(experiments$neuralNetHiddenStructure, coeff_overfit_nn)

# graph overfitting vs. complexity of neural net (increasing structures)

# mean training rate vs. connectivity
# plot different functions vs. connectivity
