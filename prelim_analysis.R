require(ggplot2)
require(tidyverse)

require("mongolite")
connection <- mongo(collection = "Experiments", db = "ShallowMind", url = "mongodb://localhost")
mongo

datasets <- read.csv(file="/Users/RheaMacBook/Desktop/shallowmind_csvs/datasets.csv")
experiments <- read.csv(file="/Users/RheaMacBook/Desktop/shallowmind_csvs/experiments.csv")
neural_nets <- read.csv(file="/Users/RheaMacBook/Desktop/shallowmind_csvs/neuralNets.csv")

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

experiments$trainingAccuracyOverTime <- lapply(experiments$trainingAccuracyOverTime, convert)
experiments$validationAccuracyOverTime <- lapply(experiments$validationAccuracyOverTime, convert)
# PROBLEM WITH THIS ONE
experiments$neuralNetHiddenStructure <- lapply(experiments$neuralNetHiddenStructure, convert)
experiments$inputShape <- as.numeric(experiments$inputShape)
experiments$outputShape <- as.numeric(experiments$outputShape)
