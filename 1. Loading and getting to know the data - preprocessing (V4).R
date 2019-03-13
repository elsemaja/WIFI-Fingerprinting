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

pacman::p_load(readr, psych, ggplot2, dplyr, tidyr)



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
# trainingDataWAPS <- select(trainingData, starts_with('WAP'))

trainingDataINFO <- trainingData[,521:529]
validationDataWAPS <- validationData[,1:520]
validationDataINFO <- validationData[,521:529]


trainingData

# identity the values between -30 and 0
trainingData[trainingDataWAPS > -30]

list30_signal <- apply(trainingData %>% select(starts_with("WAP")),1, max ) > -30

list10_signal <- apply(trainingData %>% select(starts_with("WAP")),1, max ) > -10

listhatwant <- list30_signal %in% list10_signal



trainingData30 <- trainingData[list30_signal,]


Outliers <- trainingData[trainingDataWAPS > -30]


temp <- apply(trainingDataWAPS, MARGIN = 1, function(x)
                ifelse(max(x) <= 0 & max(x) > -30 ,
                        0,
                        1))

sum(temp) / length(temp)
trainingData$problems <- temp

trainingData %>% 
  filter(problems == 1) -> Outliers

# store the outliers for further investifation (see script 1.c)
saveRDS(Outliers, file = "Outliers.rds")

# Transform the rows that have measurements between -29 and 99 to -106
# the validationData does not have measurements between -30 and 0
# trainingDataWAPS[trainingDataWAPS > -30]  <- -106 

# identify rows and cols with zero variance in the trainingdata (V2)
ZeroVarRows <- trainingData[apply(trainingDataWAPS, 1, var) == 0, ]
ZeroVarCols <- trainingDataWAPS[,apply(trainingDataWAPS, 2, var) == 0 ]

# Save the DF in order to explore these measurements further (see script 1.b)
saveRDS(ZeroVarRows, file = "ZeroVarRows.rds")

# Remove the rows and cols with zero variance from the trainingData to minimize computing
trainingDataProc <- trainingData[,-which(apply(trainingDataWAPS, 2, var) == 0)]
trainingDataProc <- trainingDataProc[-which(apply(trainingDataWAPS, 1, var) == 0), ]



# remove the same columns from the validationData otherwise the model will make
# guesses on the WAPs that are excluded, because they have variance in the validationData
validationDataProc <- validationData[,-which(apply(trainingDataWAPS, 2, var) == 0)]





# preprocess part 4: turn values into mW ----
# split WAPS from other columns
# WAPS <- select(trainingDataProc, starts_with("WAP"))
# REST <- select(trainingDataProc, LONGITUDE:TIMESTAMP)

# WAPS2 <- select(validationDataProc, starts_with("WAP"))
# REST2 <- select(validationDataProc, LONGITUDE:TIMESTAMP)



# turn dBm values into mW
# mW_WAPS <- 10^(WAPS/10)
# mW_WAPS2 <- 10^(WAPS2/10)

# bind them back together
 trainingDataProc <- bind_cols(mW_WAPS, REST)
 validationDataProc <- bind_cols(mW_WAPS2, REST2)


# store the files in order to load them in another scirpt
saveRDS(trainingDataProc, file = "data/trainingDataProc(V4).rds")
saveRDS(validationDataProc, file = "data/validationDataProc(V4).rds")


# clean your global environment
rm(validationData)
rm(trainingDataINFO)
rm(trainingDataWAPS)
rm(validationDataINFO)
rm(validationDataWAPS)
rm(mW_WAPS)
rm(mW_WAPS2)
rm(REST)
rm(REST2)
rm(WAPS)
rm(WAPS2)
