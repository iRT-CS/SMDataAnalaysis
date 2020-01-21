experiments <- read.csv(file="C:/Users/emmet/Desktop/experiments.csv")
experiments$trainingAccuracyOverTime <- lapply(experiments$trainingLossOverTime, convert)
experiments$validationAccuracyOverTime <- lapply(experiments$validationLossOverTime, convert)
experiments$neuralNetHiddenStructure <- lapply(experiments$neuralNetHiddenStructure, convert)
experiments$inputShape <- as.numeric(experiments$inputShape)
experiments$outputShape <- as.numeric(experiments$outputShape)

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

finalAccuracy <- function(shape) {
  return epochToAccuracy(length, shape)
}

# GGPLOT STUFF
# GRAPH NAMES:
# neuronCountToTrainingRate
# layerCountToTrainingRate
# weightCountToTrainingRate
# layerSizeToTrainingRate

options(scipen=999)
library(ggplot2)

# GRAPH TOTAL NUMBER OF NEURONS VS. TRAINING RATE
allShapes = experiments$neuralNetHiddenStructure
neuronCounts = as.numeric(lapply(allShapes, sum))
averageRates = as.numeric(lapply(allShapes, averageTrainingRate)) * 100
neuronsRateTable = data.frame(neuronCounts, averageRates)
neuronCountToTrainingRate <- ggplot(neuronsRateTable, aes(x=neuronCounts, y=averageRates)) + geom_point() + geom_smooth(method="lm") + xlab("Total Neurons") + ylab("Training Rate (%)")

# GRAPH TOTAL NUMBER OF LAYERS VS. TRAINING RATE
layerCounts = as.numeric(lapply(allShapes, length))
layersRateTable = data.frame(layerCounts, averageRates)
layerCountToTrainingRate <- ggplot(layersRateTable, aes(x=layerCounts, y=averageRates)) + geom_point() + geom_smooth(method="lm") + xlab("Total Layers in Structure") + ylab("Training Rate (%)")

# GRAPH WEIGHT COUNT VS. TRAINING RATE
# NOTE: This only includes HIDDEN WEIGHTS (input and output weights not included...)
shapeToWeightCount <- function(shape) {
  accumulator = shape[1] + shape[length(shape)]
  for (val in c(1:(length(shape) - 1))) {
    accumulator = accumulator + (shape[val] * shape[val + 1])
  }
  return(accumulator)
}

weightCounts = as.numeric(lapply(allShapes, shapeToWeightCount))
weightsRateTable = data.frame(weightCounts, averageRates)
weightCountToTrainingRate <- ggplot(weightsRateTable, aes(x=weightCounts, y=averageRates)) + geom_point() + geom_smooth(method="lm") + xlab("Total Weights in Structure") + ylab("Training Rate (%)")

# GRAPH AVERAGE LAYER SIZE VS. TRAINING RATE
averageLayerSize = as.numeric(lapply(allShapes, mean))
layerSizeRateTable = data.frame(averageLayerSize, averageRates)
layerSizeToTrainingRate <- ggplot(layerSizeRateTable, aes(x=averageLayerSize, y=averageRates)) + geom_point() + geom_smooth(method="lm") + xlab("Average Layer Size") + ylab("Training Rate (%)")

