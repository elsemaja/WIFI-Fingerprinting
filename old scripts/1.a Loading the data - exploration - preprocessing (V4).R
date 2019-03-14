###############################################################################
#                                                                             #
#   WIFI | PACKAGES & LOADING DATA | VERSION 4.0 | by ELSE                    #
#                                                                             #
#   connected to results V4, preprocessed: values between -30-0, +100         #
#                                                                             #
###############################################################################


# visualize the distribution of dBm, use facet wrap to divide floors and buildings
# first gather all WAPS in 1 column, then you are able to see all the values of the waps.
# gather all WAPS in one column
# GatherWAPS <- trainingDataWAPS %>% 
#  gather(key = 'ALLWAPS', value = 'VALUE', 
#         1:520)



# libraries ----
if (require(pacman) == FALSE) {
  install.packages('pacman')
}

pacman::p_load(readr, psych, ggplot2, dplyr, tidyverse)

library(tidyverse)

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
trainingData$FLOOR <- as.integer(trainingData$FLOOR)
trainingData$BUILDINGID <- as.integer(trainingData$BUILDINGID)
trainingData$SPACEID <- as.integer(trainingData$SPACEID)
trainingData$RELATIVEPOSITION <- as.integer(trainingData$RELATIVEPOSITION)
trainingData$USERID <- as.integer(trainingData$USERID)
trainingData$PHONEID <- as.integer(trainingData$PHONEID)




# do the same for the validationData set
validationData$LONGITUDE <- as.numeric(validationData$LONGITUDE)
validationData$LATITUDE <- as.numeric(validationData$LATITUDE)
validationData$FLOOR <- as.integer(validationData$FLOOR)
validationData$BUILDINGID <- as.integer(validationData$BUILDINGID)
validationData$SPACEID <- as.integer(validationData$SPACEID)
validationData$RELATIVEPOSITION <- as.integer(validationData$RELATIVEPOSITION)
validationData$USERID <- as.integer(validationData$USERID)
validationData$PHONEID <- as.integer(validationData$PHONEID)


# preprocessing part 1: turn +100 values into -106 ----
trainingData[trainingData == 100] <- -106
validationData[validationData == 100] <- -106


# preprocessing part 2: remove outliers and zero variance columns and rows ----
# split the dataframe in WAPS and INFO
#trainingDataWAPS <- trainingData[,1:520]
#trainingDataINFO <- trainingData[,521:529]
#validationDataWAPS <- validationData[,1:520]
#validationDataINFO <- validationData[,521:529]


# dBm values above -30 ----
# make a list of the rows that include values of more than -30
list30 <- apply(trainingData %>% select(starts_with("WAP")),1, max ) > -30
sum(list30) / length(list30)


# select the rows (as specified in the list above) from the dataset
Outliers30 <- trainingData[list30,]

# store the outliers for further investifation (see script 1.c)
saveRDS(Outliers30, file = "data/Outliers.rds")


# Remove the rows that have values between -30 and 0 from the dataset
trainingData <- trainingData[!list30,]



# Transform the rows that have measurements between -29 and 99 to -106
# trainingDataWAPS[trainingDataWAPS > -30]  <- -30

# identify rows with zero variance in the trainingdata (V2) ----
listZVRows <- apply(trainingData %>% select(starts_with("WAP")),1, var ) == 0
sum(listZVRows) / length(listZVRows)

# store the rows in a DF for further analysis
ZeroVarRows <- trainingData[listZVRows,]

# Save the DF in order to explore these measurements further (see script 1.b)
saveRDS(ZeroVarRows, file = "data/ZeroVarRows.rds")

# now remove them from the DF
trainingData <- trainingData[!listZVRows,]


# identify cols with zero variance in the trainingdata (V2) ----
listVcols <- apply(trainingData %>% select(-TIMESTAMP),2, var ) == 0
sum(listVcols) / length(listVcols)



# now remove them from the DF
trainingData <- trainingData[,!listVcols] 
names(trainingData)




# remove the same columns from the validationData otherwise the model will make
# guesses on the WAPs that are excluded, because they have variance in the validationData??
# validationDataProc <- validationData[,-which(apply(trainingDataWAPS, 2, var) == 0)]




 # preprocess part 4: turn values into mW ----
 #split WAPS from other columns
# WAPS <- select(trainingDataProc, starts_with("WAP"))
# REST <- select(trainingDataProc, LONGITUDE:TIMESTAMP)

# WAPS2 <- select(validationDataProc, starts_with("WAP"))
# REST2 <- select(validationDataProc, LONGITUDE:TIMESTAMP)



# turn dBm values into mW
# mW_WAPS <- 10^(WAPS/10)
# mW_WAPS2 <- 10^(WAPS2/10)

# bind them back together
# trainingDataProc <- bind_cols(mW_WAPS, REST)
# validationDataProc <- bind_cols(mW_WAPS2, REST2)



# store the files in order to load them in another scirpt
saveRDS(trainingData, file = "data/trainingDataProc(V4).rds")
saveRDS(validationData, file = "data/validationDataProc(V4).rds")


# clean your global environment
rm(Outliers30)
rm(ZeroVarRows)
