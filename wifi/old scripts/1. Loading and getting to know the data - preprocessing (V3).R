###############################################################################
#                                                                             #
#   WIFI | PACKAGES & LOADING DATA | VERSION 3.0 | by ELSE                    #
#                                                                             #
#   connected to results V3, preprocessing step 1: turn +100 into -106 dBm    #
#                                                                             #
###############################################################################


# visualize the distribution of dBm, use facet wrap to divide floors and buildings
# first gather all WAPS in 1 column, then you are able to see all the values of the waps.

# libraries ----
if (require(pacman) == FALSE) {
  install.packages('pacman')
}

pacman::p_load(readr, psych, ggplot2, dplyr)



# set working directory
setwd("~/Google Drive/BSSA - data analytics/Module 3/IoT - wifi/wifi")



# load training data file
trainingData <- read.csv("data/trainingData.csv")
validationData <- read.csv("data/validationData.csv")



# check for missing data
trainingData[!complete.cases(trainingData),] 


# change structure according to attribute information in the resources:
trainingData$LONGITUDE <- as.numeric(trainingData$LONGITUDE)
trainingData$LATITUDE <- as.numeric(trainingData$LATITUDE)
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)
trainingData$SPACEID <- as.integer(trainingData$SPACEID)
trainingData$RELATIVEPOSITION <- as.integer(trainingData$RELATIVEPOSITION)
trainingData$USERID <- as.integer(trainingData$USERID)
trainingData$PHONEID <- as.integer(trainingData$PHONEID)
trainingData$TIMESTAMP <- as.POSIXct(trainingData$TIMESTAMP, origin = '1970-01-01', tz = 'GMT')



# do the same for the validationData set
validationData$LONGITUDE <- as.numeric(validationData$LONGITUDE)
validationData$LATITUDE <- as.numeric(validationData$LATITUDE)
validationData$FLOOR <- as.factor(validationData$FLOOR)
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
validationData$SPACEID <- as.integer(validationData$SPACEID)
validationData$RELATIVEPOSITION <- as.integer(validationData$RELATIVEPOSITION)
validationData$USERID <- as.integer(validationData$USERID)
validationData$PHONEID <- as.integer(validationData$PHONEID)
validationData$TIMESTAMP <- as.POSIXct(validationData$TIMESTAMP, origin = '1970-01-01', tz = 'GMT')

# preprocessing part 1: turn +100 values into -106 ----
trainingData[trainingData == 100] <- -106
validationData[validationData == 100] <- -106


# preprocessing part 2: remove outliers and zero variance columns and rows ----
# split the dataframe in WAPS and INFO
trainingDataWAPS <- trainingData[,1:520]
trainingDataINFO <- trainingData[,521:529]


# Remove the rows that have measurements between -29 and 99
trainingDataWAPS[trainingDataWAPS > -30]  <- -106 




# identify rows and cols with zero variance in the trainingdata
ZeroVarRows <- trainingDataWAPS[apply(trainingDataWAPS, 1, var) == 0, ]
ZeroVarCols <- trainingDataWAPS[,apply(trainingDataWAPS, 2, var) == 0 ]

# identify columns with zero variance in the validationData
# there are no rows with zero variance in the validationData
# ValZeroVarCols <- validationDataWAPS[,apply(validationDataWAPS, 2, var) == 0 ]

# Remove the rows and cols with zero variance from the trainingData
trainingDataProc <- trainingData[,-which(apply(trainingDataWAPS, 2, var) == 0)]
trainingDataProc <- trainingDataProc[-which(apply(trainingDataWAPS, 1, var) == 0), ]

# remove the same columns from the validationData otherwise the model will make
# guesses on the WAPs that are excluded, because they have variance in the validationData
validationDataProc <- validationData[,-which(apply(trainingDataWAPS, 2, var) == 0)]










# preprocess part 4: turn values into mW
# split WAPS from other columns
# WAPS <- select(trainingData, starts_with("WAP"))
# REST <- select(trainingData, LONGITUDE:TIMESTAMP)

# WAPS2 <- select(validationData, starts_with("WAP"))
# REST2 <- select(validationData, LONGITUDE:TIMESTAMP)



# turn dBm values into mW
# mW_WAPS <- 10^(WAPS/10)
# mW_WAPS2 <- 10^(WAPS2/10)

# bind them back together
# trainingDataProc <- bind_cols(mW_WAPS, REST)
# validationDataProc <- bind_cols(mW_WAPS2, REST2)



saveRDS(trainingDataProc, file = "data/trainingDataProc.rds")
saveRDS(validationDataProc, file = "data/validationDataProc.rds")


rm(trainingData)
rm(validationData)
rm(trainingDataINFO)
rm(trainingDataWAPS)
rm(validationDataINFO)
rm(validationDataWAPS)
