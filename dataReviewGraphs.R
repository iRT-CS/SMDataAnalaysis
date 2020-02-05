require(ggplot2)
require(tidyverse)
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

experiments <- read.csv(file="C:/Users/emmet/Desktop/experiments1-30.csv")
experiments$trainingAccuracyOverTime <- lapply(experiments$trainingLossOverTime, convert)
experiments$validationAccuracyOverTime <- lapply(experiments$validationLossOverTime, convert)
experiments$neuralNetHiddenStructure <- lapply(experiments$neuralNetHiddenStructure, convertShapes)
experiments$inputShape <- as.numeric(experiments$inputShape)
experiments$outputShape <- as.numeric(experiments$outputShape)

hiddenStructures <- lapply(experiments$neuralNetHiddenStructure, convertShapes)
hiddenStructuresBroken <- lapply(experiments$neuralNetHiddenStructure, convert)

getRowIndex <- function(shape) {
  rowIndex <- Position(function(x) identical(x, shape), experiments$neuralNetHiddenStructure)
  if (is.na(rowIndex)) {return(-1)}
  return(rowIndex)
}

shapeToAccuracyList <- function(shape) {
  rowIndex <- getRowIndex(shape)
  if (rowIndex == -1) {return(-1)}
  return(accuracyList <- experiments$trainingAccuracyOverTime[[rowIndex]])
}

epochToAccuracy <- function(epoch, shape) {
  accuracyList <- shapeToAccuracyList(shape)
  if (epoch > 0 & epoch <= length(accuracyList)) { 
    return(accuracyList[epoch])
  }
  else { 
    return(-1)
  }
}

averageTrainingRate <- function(shape) {
  initialAccuracy = epochToAccuracy(1, shape)
  if (initialAccuracy == -1) {
    return(-1)
  }
  length <- length(shapeToAccuracyList(shape))
  finalAccuracy <- epochToAccuracy(length, shape)
  difference <- finalAccuracy - initialAccuracy
  return(difference / length)
}

shapeToFinalAccuracy <- function(shape) {
  rowIndex <- getRowIndex(shape)
  validations <- experiments$validationAccuracyOverTime[[rowIndex]]
  return (validations[length(validations)])
}

# GGPLOT                    
options(scipen=999)
library(ggplot2)

allShapes = experiments$neuralNetHiddenStructure
averageRates = as.numeric(lapply(allShapes, averageTrainingRate)) * 100
finalRates = as.numeric(lapply(allShapes, shapeToFinalAccuracy)) * 100


# GRAPH TOTAL NUMBER OF NEURONS VS. TRAINING RATE
neuronCounts = as.numeric(lapply(allShapes, sum))
neuronsRateTable = data.frame(neuronCounts, averageRates)
neuronCountToTrainingRate <- ggplot(neuronsRateTable, aes(x=neuronCounts, y=averageRates)) + geom_point() + geom_smooth(method="lm") + xlab("Total Neurons") + ylab("Training Rate (%)")

# GRAPH TOTAL NUMBER OF LAYERS VS. TRAINING RATE
layerCounts = as.numeric(lapply(allShapes, length))
layersRateTable = data.frame(layerCounts, averageRates)
layerCountToTrainingRate <- ggplot(layersRateTable, aes(x=layerCounts, y=averageRates)) + geom_point() + xlab("Total Layers in Structure") + ylab("Training Rate (%)")

# GRAPH WEIGHT COUNT VS. TRAINING RATE
# Function that gives weight count based on shape
shapeToWeightCount <- function(shape) {
  shape <- list(shape)
  if (length(shape[[1]]) == 1) {
    return (3 * shape[[1]][1])
  }
  accumulator = 2 * shape[[1]][1] + shape[[1]][length(shape[[1]])]
  for (val in c(1:(length(shape[[1]]) - 1))) {
    accumulator = accumulator + (shape[[1]][val] * shape[[1]][val + 1])
  }
  return(accumulator)
}
# weight count vs training rate
weightCounts <- as.numeric(lapply(allShapes, shapeToWeightCount))
weightsRateTable = data.frame(weightCounts, averageRates)
weightCountToTrainingRate <- ggplot(weightsRateTable, aes(x=weightCounts, y=averageRates)) + geom_point() + geom_smooth(method="lm") + xlab("Total Weights in Structure") + ylab("Training Rate (%)")
# weight count vs. final accuracy
weightsFinalTable = data.frame(weightCounts, finalRates)
# TRIPLE BAND GRAPH!!!!!!!!!!!!! compares total number of weghts to the final accuracy of the structure
weightCountToFinalRate <- ggplot(weightsFinalTable, aes(x=weightCounts, y=finalRates)) + geom_point() + xlab("Total Weights in Structure") + ylab("FinalAccuracy (%)")

# GRAPH AVERAGE LAYER SIZE VS. TRAINING RATE
# averageLayerSize = as.numeric(lapply(allShapes, mean))
# layerSizeRateTable = data.frame(averageLayerSize, averageRates)
# layerSizeToTrainingRate <- ggplot(layerSizeRateTable, aes(x=averageLayerSize, y=averageRates)) + geom_point() + geom_smooth(method="lm") + xlab("Average Layer Size") + ylab("Training Rate (%)")


# MAKE TABLES OF TOP 10 and WORST 10 STRUCTURES
library(plyr)
unlistedShapes <- lapply(allShapes, unlist)
shapesTable <- data.frame(allShapes, finalRates, ncol = 2)
shapesRatesTable <- do.call(rbind.fill, lapply(lapply(lapply(unlistedShapes, data.frame), t), data.frame))
shapesRatesTable <- cbind(shapesRatesTable, averageRates, finalRates)

order.training <- order(shapesRatesTable$averageRates, decreasing = TRUE)
shapesRatesbyTraining <- shapesRatesTable[order.training,]
order.final <- order(shapesRatesTable$finalRates, decreasing = TRUE)
shapesRatesbyFinal <- shapesRatesTable[order.final, ]
