###############################################################################
#                                                                             #
#   WIFI | PACKAGES & LOADING DATA | VERSION 5.0 | by ELSE                    #
#                                                                             #
#   connected to results V5, preprocessed: values between -30-0, +100         #
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
trainingData$TIMESTAMP <- as.POSIXct(trainingData$TIMESTAMP, origin = '1970-01-01', tz = 'GMT')




# do the same for the validationData set
validationData$LONGITUDE <- as.numeric(validationData$LONGITUDE)
validationData$LATITUDE <- as.numeric(validationData$LATITUDE)
validationData$FLOOR <- as.integer(validationData$FLOOR)
validationData$BUILDINGID <- as.integer(validationData$BUILDINGID)
validationData$SPACEID <- as.integer(validationData$SPACEID)
validationData$RELATIVEPOSITION <- as.integer(validationData$RELATIVEPOSITION)
validationData$USERID <- as.integer(validationData$USERID)
validationData$PHONEID <- as.integer(validationData$PHONEID)
validationData$TIMESTAMP <- as.POSIXct(validationData$TIMESTAMP, origin = '1970-01-01', tz = 'GMT')



# preprocessing part 2: remove outliers and zero variance columns and rows ----
# split the dataframe in WAPS and INFO
#trainingDataWAPS <- trainingData[,1:520]
#trainingDataINFO <- trainingData[,521:529]
#validationDataWAPS <- validationData[,1:520]
#validationDataINFO <- validationData[,521:529]



# preprocessing part 1: turn +100 values into -106 ----
trainingData[trainingData == 100] <- -120
validationData[validationData == 100] <- -120




# dBm values above -30 ----
# make a list of the rows that include values of more than -30
list30 <- apply(trainingData %>% select(starts_with("WAP")),1, max ) > -30
sum(list30) / length(list30)


# select the rows (as specified in the list above) from the dataset
Outliers30 <- trainingData[list30,]

# store the outliers for further investifation (see script 1.c)
saveRDS(Outliers30, file = "data/Outliers.rds")





#trainingDataWAPS <- trainingData[,1:520]
#trainingDataINFO <- trainingData[,521:529]
#validationDataWAPS <- validationData[,1:520]
#validationDataINFO <- validationData[,521:529]

#trainingData <- cbind(trainingDataWAPS,trainingDataINFO)
#validationData <- cbind(validationDataWAPS,validationDataINFO)





# the outliers were mainly produced by USERID 6 in BUILDINGID 2 on FLOOR 6
# the measurements made were connected to multiple SPACEID's.
#USERID6 <- trainingData %>% 
#  filter(USERID == 6 & FLOOR ==4)
#USERID5BUILDING2FLOOR3 <- trainingData %>%
#  filter(USERID == 5 & BUILDINGID == 2 & FLOOR == 3)
#USERID1BUILDING0FLOOR2 <- trainingData %>%
#  filter(USERID == 1 & BUILDINGID == 0 & FLOOR == 2)
#USERID11BUILDING0FLOOR2 <- trainingData %>%
#  filter(USERID == 11 & BUILDINGID == 0 & FLOOR == 2)
#PHONEID13 <- trainingData %>%
#  filter(PHONEID == 13)
#PHONEID14 <- trainingData %>%
#  filter(PHONEID == 14)
PHONEID17 <- trainingData %>%
  filter(PHONEID == 17, BUILDINGID == 1 & FLOOR == 1)

PHONEID17[520:529]




trainingData <- trainingData %>% 
  filter(!(USERID == 6 & FLOOR ==4))

trainingData <- trainingData %>%
  filter((!(USERID == 5 & BUILDINGID == 2 & FLOOR == 3)))


list30 <- apply(trainingData %>% select(starts_with("WAP")),1, max ) > -30
sum(list30) / length(list30)



# Transform the rows that have measurements between -29 and 99 to -30
# trainingDataWAPS[trainingDataWAPS > -30]  <- -30
# trainingDataWAPS <- select(trainingData, starts_with("WAP"))
# trainingDataWAPS[trainingDataWAPS >= -30] <- trainingDataWAPS[trainingDataWAPS[1:520] >= -30] -30
# for(i in junk$nm) if(i %in% "B") junk$nm <- "b"



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
trainingDataWAPS <- trainingData[,1:520]
trainingDataINFO <- trainingData[,521:529]

listVcols <- apply(trainingDataWAPS %>% select(starts_with("WAP")),2, var ) == 0
sum(listVcols) / length(listVcols)



# now remove them from the DF
trainingDataWAPS <- trainingDataWAPS[,!listVcols] 

trainingData <- cbind(trainingDataWAPS,trainingDataINFO)


names(trainingData)






# store the files in order to load them in another scirpt
saveRDS(trainingData, file = "data/trainingDataProc(V5).rds")
saveRDS(validationData, file = "data/validationDataProc(V5).rds")


# clean your global environment
#rm(list = ls())
