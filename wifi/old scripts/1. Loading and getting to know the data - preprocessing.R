###############################################################################
#                                                                             #
#   WIFI | PACKAGES & LOADING DATA | VERSION 1.0 | by ELSE                    #
#                                                                             #
#   .............................<description>................................#
#                                                                             #
###############################################################################

# libraries ----
if (require(pacman) == FALSE) {
  install.packages('pacman')
}

pacman::p_load(readr, psych, ggplot2)



# set working directory
setwd("~/Google Drive/BSSA - data analytics/Module 3/IoT - wifi/wifi")



# load training data file
trainingData <- read.csv("data/trainingData.csv")
validationData <- read.csv("data/validationData.csv")



# more basic statistics with
describe(trainingData)


# initial exploration
head(trainingData)
tail(trainingData)
summary(trainingData)
str(trainingData)
colnames(trainingData)



# check for missing data
trainingData[!complete.cases(trainingData),] 



# change structure according to attribute information in the resources:
trainingData$FLOOR <- as.integer(trainingData$FLOOR)
trainingData$BUILDINGID <- as.integer(trainingData$BUILDINGID)
trainingData$SPACEID <- as.integer(trainingData$SPACEID)
trainingData$RELATIVEPOSITION <- as.integer(trainingData$RELATIVEPOSITION)
trainingData$USERID <- as.integer(trainingData$USERID)
trainingData$PHONEID <- as.integer(trainingData$PHONEID)
trainingData$TIMESTAMP <- as.POSIXct(trainingData$TIMESTAMP, origin = '1970-01-01', tz = 'GMT')

# do the same for the validationData set
validationData$FLOOR <- as.integer(validationData$FLOOR)
validationData$BUILDINGID <- as.integer(validationData$BUILDINGID)
validationData$SPACEID <- as.integer(validationData$SPACEID)
validationData$RELATIVEPOSITION <- as.integer(validationData$RELATIVEPOSITION)
validationData$USERID <- as.integer(validationData$USERID)
validationData$PHONEID <- as.integer(validationData$PHONEID)
validationData$TIMESTAMP <- as.POSIXct(validationData$TIMESTAMP, origin = '1970-01-01', tz = 'GMT')


# preprocessing ----
# turn +100 values into -106
trainingData[trainingData == 100] <- -106
validationData[validationData == 100] <- -106



