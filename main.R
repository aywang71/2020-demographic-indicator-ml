# Andrew Wang
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("~/GitHub/2020-demographic-indicator-ml") # go to this folder

#load up myfunctions.R
source("~/GitHub/2020-demographic-indicator-ml")

#library import
library(maps)
library(tidyverse)
library(usmap)
library(gridExtra)

set.seed(1234)

data <- read.csv("data/dataFresh.csv")
data <- na.omit(data)
dim(data)
glimpse(data)
str(data)
print(sum(is.na(data)))
data$outcome <- ifelse(data$percentage16_Donald_Trump>data$percentage16_Hillary_Clinton,-1,1) # -1 = republican, 1 = democrat

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
populationTraining <- data[ind == 1, c(16:20,27)]
populationTesting <- data[ind == 2, c(16:20,27)]
raceTraining <- data[ind == 1, 21:26]
raceTesting <- data[ind == 2, 21:26]
transportationTraining <- data[ind == 1, c(38:43,45)]
transportationTesting <- data[ind == 2, c(38:43,45)]
incomeTraining <- data[ind == 1, c(28,30,31:32)]
incomeTesting <- data[ind == 2, c(28,30,31:32)]
employmentTraining <- data[ind == 1, c(33:37,44)]
employmentTesting <- data[ind == 1, c(33:37,44)]
sectorTraining <- data[ind == 1, 47:50]
sectorTesting <- data[ind == 2, 47:50]
trainingLabels <- data[ind == 1, 52]
testingLabels <- data[ind == 2, 52]

#cluster size is 3
