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
data <- mutate(
  data,
  margin2016 = percentage16_Hillary_Clinton - percentage16_Donald_Trump,
  margin2020 = percentage20_Joe_Biden - percentage20_Donald_Trump,
  shift = margin2020 - margin2016
)
data$outcome <- ifelse(data$shift>0,1,-1) # -1 = republican, 1 = democrat
data %>% count(outcome)

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
trainingLabels <- data[ind == 1, 55]
#table(trainingLabels)
testingLabels <- data[ind == 2, 55]
#table(testingLabels)

#list of flipped states
changed <- c("AZ","WI","MI","GA","PA") #only from R --> D

#define party colors for coloring of geographic plots
partyColors <- c("#2E74C0", "#CB454A","#0000FF")

customPCA <- function(data, description){
  pca <- prcomp(t(data))
  View(pca$x)
  #screeplot + plot
  plot(pca$x[,1],pca$x[,2])
  #variance calculation
  pcaV <- pca$sdev^2
  pcaV <- round(pcaV/sum(pcaV)*100,1)
  pcaV
  barplot(pcaV)
  #contributing variable plot
  pcaDF <- data.frame(Sample=rownames(pca$x), X=pca$x[,1], Y=pca$x[,2])
  pcaP <- ggplot(data=pcaDF, aes(x=X, y=Y, label=Sample)) +
    geom_text() +
    xlab(paste("PC1 - ", pcaV[1], "%", sep="")) +
    ylab(paste("PC2 - ", pcaV[2], "%", sep="")) + 
    ggtitle(paste("Demographic indicators in PCA analysis -", sep="\n"))
  pcaP
}

populationTraining <- data[ind == 1, c(16:20,27)]
populationTesting <- data[ind == 2, c(16:20,27)]
set.seed(1234)
populationPrediction <- knn(train = populationTraining, test = populationTesting, cl = trainingLabels, k = 3)
CrossTable(x = testingLabels, y = populationPrediction, prop.chisq = FALSE)

raceTraining <- data[ind == 1, 21:26]
raceTesting <- data[ind == 2, 21:26]
raceTraining <- raceTraining
raceTraining$sum <- rowSums(raceTraining)
raceTraining$Hispanic <- raceTraining$Hispanic / raceTraining$sum
raceTraining$White <- raceTraining$White / raceTraining$sum
raceTraining$Black <- raceTraining$Black / raceTraining$sum
raceTraining$Asian <- raceTraining$Asian / raceTraining$sum
raceTraining$Native <- raceTraining$Native / raceTraining$sum
raceTraining$Pacific <- raceTraining$Pacific / raceTraining$sum
raceTraining$sum <- NULL
raceTesting <- raceTesting
raceTesting$sum <- rowSums(raceTesting)
raceTesting$Hispanic <- raceTesting$Hispanic / raceTesting$sum
raceTesting$White <- raceTesting$White / raceTesting$sum
raceTesting$Black <- raceTesting$Black / raceTesting$sum
raceTesting$Asian <- raceTesting$Asian / raceTesting$sum
raceTesting$Native <- raceTesting$Native / raceTesting$sum
raceTesting$Pacific <- raceTesting$Pacific / raceTesting$sum
raceTesting$sum <- NULL
set.seed(1234)
racePrediction <- knn(train = raceTraining, test = raceTesting, cl = trainingLabels, k = 3)
CrossTable(x = testingLabels, y = racePrediction, prop.chisq = FALSE)

incomeTraining <- data[ind == 1, c(28,30,31:33)]
incomeTesting <- data[ind == 2, c(28,30,31:33)]
incomeTraining$IncomePerCapErr <- NULL
incomeTesting$IncomePerCapErr <- NULL
set.seed(1234)
incomePrediction <- knn(train = incomeTraining, test = incomeTesting, cl = trainingLabels, k = 3)
CrossTable(x = testingLabels, y = incomePrediction, prop.chisq = FALSE)

sectorTraining <- data[ind == 1, c(34:38,44)]
sectorTesting <- data[ind == 2, c(34:38,44)]
set.seed(1234)
sectorPrediction <- knn(train = sectorTraining, test = sectorTesting, cl = trainingLabels, k = 3)
CrossTable(x = testingLabels, y = sectorPrediction, prop.chisq = FALSE)

transportationTraining <- data[ind == 1, c(39:43,45)]
transportationTesting <- data[ind == 2, c(39:43,45)]
set.seed(1234)
transportationPrediction <- knn(train = transportationTraining, test = transportationTesting, cl = trainingLabels, k = 3)
CrossTable(x = testingLabels, y = transportationPrediction, prop.chisq = FALSE)

workTraining <- data[ind == 1, 47:50]
workTesting <- data[ind == 2, 47:50]
set.seed(1234)
workPrediction <- knn(train = workTraining, test = workTesting, cl = trainingLabels, k = 3)
CrossTable(x = testingLabels, y = workPrediction, prop.chisq = FALSE)

#adding FIPS numbers to county dataset 
localCounty <- data.frame(countypop)
#a bunch of replacement for matching stuff
localCounty$county <- gsub(" county", "", localCounty$county, ignore.case=TRUE)
localCounty$county <- gsub(" parish", "", localCounty$county, ignore.case=TRUE)
localCounty$county <- gsub(" borough", "", localCounty$county, ignore.case=TRUE)
localCounty$county <- gsub(" census area", "", localCounty$county, ignore.case=TRUE)
localCounty$state <- localCounty$abbr
localCounty$abbr <- NULL
data <- inner_join(localCounty, data, by = c("county","state"))
data$pop_2015 <- NULL

#main triple plot
main2020 <- plot_usmap(data = data, values = "margin2020", color = "white") + 
  scale_fill_gradient2(low = partyColors[2], mid = "white", high = partyColors[1], na.value = "white", name = "Electoral shift", label = scales::comma) +
  theme(legend.position = "none")
main2020
mainShift <- plot_usmap(data = data, values = "shift", color = "white") + 
  scale_fill_gradient2(low = partyColors[2], mid = "white", high = partyColors[1], na.value = "white", name = "Electoral shift", label = scales::comma) +
  theme(legend.position = "none")
mainShift
main2016 <- plot_usmap(data = data, values = "margin2016", color = "white") + 
  scale_fill_gradient2(low = partyColors[2], mid = "white", high = partyColors[1], na.value = "white", name = "Electoral shift", label = scales::comma) +
  theme(legend.position = "none")
main2016
grid.arrange(main2016, mainShift, main2020, nrow = 1)

#pipe for group sorting
stateSet <- data %>% 
  group_by(state) %>% 
  summarize(
    stateMargin2016 = (sum(votes16_Hillary_Clinton)/sum(total_votes16)) - (sum(votes16_Donald_Trump)/sum(total_votes16)),
    stateMargin2020 = (sum(votes20_Joe_Biden)/sum(total_votes20)) - (sum(votes20_Donald_Trump)/sum(total_votes20)),
    totalPop = sum(TotalPop)
  )
stateSet$stateShift <- stateSet$stateMargin2020 - stateSet$stateMargin2016
stateSet$abbr <- stateSet$state
stateSet$state <- NULL
#formatting and join stuff for FIPS codes
stateSet <- inner_join(stateSet,statepop)
stateSet$pop_2015 <- NULL
#plot
stateShift2 <- plot_usmap(data = stateSet, values = "stateShift", color = "white") + 
  scale_fill_gradient2(low = "black", mid = "white", high = "black", na.value = "white") +
  theme(legend.position = "none")
statePop <- plot_usmap(data = stateSet, values = "totalPop", color = "white") + 
  scale_fill_gradient2(low = "white", mid = "lightgray", high = "black", na.value = "white") +
  theme(legend.position = "none")
statePop
grid.arrange(stateShift2,statePop,nrow=1)
#plot
state2020 <- plot_usmap(data = stateSet, values = "stateMargin2020", color = "gray") +
  scale_fill_gradient2(low = partyColors[2], mid = "white", high = partyColors[1], na.value = "white") +
  theme(legend.position = "none")
#state2020
stateShift <- plot_usmap(data = stateSet, values = "stateShift", color = "gray") +
  scale_fill_gradient2(low = partyColors[2], mid = "white", high = partyColors[1], na.value = "white") +
  theme(legend.position = "bottom")
stateShift
#plot
state2016 <- plot_usmap(data = stateSet, values = "stateMargin2016", color = "gray") +
  scale_fill_gradient2(low = partyColors[2], mid = "white", high = partyColors[1], na.value = "white") +
  theme(legend.position = "none")
#state2016
grid.arrange(state2016,stateShift,state2020, nrow = 1)

#PCA work on Sector
sector <- rbind(sectorTesting, sectorTraining)
customPCA(sector)

#professional is the best
professionalPlot <- plot_usmap(data = data, values = "Professional", color = "lightgray") + 
  scale_fill_gradient2(low = "black", mid = "gray10", high = "white", na.value = "white", name = "Professional sector", label = scales::comma) +
  theme(legend.position = "none")
professionalPlot


#TODO: set up graphics and maps 
#TODO: determine whether small scales need to be applied for longer results section, and what kind of graphics need to be made
