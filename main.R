# Andrew Wang
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("~/GitHub/2020-demographic-indicator-ml") # go to this folder

#load up myfunctions.R
source("~/GitHub/2020-demographic-indicator-ml/myfunctions.R")

#library import
library(maps)
library(tidyverse)
library(usmap)
library(gridExtra)
library(caret)
library(class)
library(ggvis)
library(gmodels)
library(GGally)

set.seed(1234)

data <- read.csv("data/dataFresh.csv")
data <- na.omit(data)
dim(data)
glimpse(data)
str(data)
print(sum(is.na(data)))
data$outcome <- ifelse(data$percentage16_Donald_Trump>data$percentage16_Hillary_Clinton,-1,1) # -1 = republican, 1 = democrat
data %>% count(outcome)

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
trainingLabels <- data[ind == 1, 52]
table(trainingLabels)
testingLabels <- data[ind == 2, 52]
table(testingLabels)

populationTraining <- data[ind == 1, c(16:20,27)]
populationTesting <- data[ind == 2, c(16:20,27)]
set.seed(1234)
populationPrediction <- knn(train = populationTraining, test = populationTesting, cl = trainingLabels, k = 3)
CrossTable(x = testingLabels, y = populationPrediction, prop.chisq = FALSE)

raceTraining <- data[ind == 1, 21:26]
raceTesting <- data[ind == 2, 21:26]
set.seed(1234)
racePrediction <- knn(train = raceTraining, test = raceTesting, cl = trainingLabels, k = 3)
CrossTable(x = testingLabels, y = racePrediction, prop.chisq = FALSE)

transportationTraining <- data[ind == 1, c(38:43,45)]
transportationTesting <- data[ind == 2, c(38:43,45)]
set.seed(1234)
transportationPrediction <- knn(train = transportationTraining, test = transportationTesting, cl = trainingLabels, k = 3)
CrossTable(x = testingLabels, y = transportationPrediction, prop.chisq = FALSE)

incomeTraining <- data[ind == 1, c(28,30,31:32)]
incomeTesting <- data[ind == 2, c(28,30,31:32)]
set.seed(1234)
incomePrediction <- knn(train = incomeTraining, test = incomeTesting, cl = trainingLabels, k = 3)
CrossTable(x = testingLabels, y = incomePrediction, prop.chisq = FALSE)

employmentTraining <- data[ind == 1, c(33:37,44)]
employmentTesting <- data[ind == 2, c(33:37,44)]
set.seed(1234)
employmentPrediction <- knn(train = employmentTraining, test = employmentTesting, cl = trainingLabels, k = 3)
CrossTable(x = testingLabels, y = employmentPrediction, prop.chisq = FALSE)

sectorTraining <- data[ind == 1, 47:50]
sectorTesting <- data[ind == 2, 47:50]
set.seed(1234)
sectorPrediction <- knn(train = sectorTraining, test = sectorTesting, cl = trainingLabels, k = 3)
CrossTable(x = testingLabels, y = sectorPrediction, prop.chisq = FALSE)
