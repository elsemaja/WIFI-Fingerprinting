###############################################################################
#                                                                             #
#   WIFI | PACKAGES & LOADING DATA | VERSION 7.0 | by ELSE                    #
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




# transform all values (2% of the traininData & 1,8% van de validationData) that are -80 or less to -100
#trainingData[trainingData <= -80] <- -100
#validationData[validationData <= -80] <- -100 # dubble check the results

trainingData <- trainingData %>%
  filter(!(USERID == 6 & FLOOR ==4))


# turn +100 values into -106 ----
trainingData[trainingData == 100] <- -120
validationData[validationData == 100] <- -120


# remove values of more than -30 ---
list30 <- apply(trainingData %>% select(starts_with("WAP")),1, max ) > -30 # creates list of rows 
sum(list30) / length(list30) # to see how often they occur are in the trainingData
trainingData <- trainingData[!list30,] # remove


# save the rows for further investigation (script 1.c)
Outliers30 <- trainingData[list30,]
saveRDS(Outliers30, file = "data/Outliers.rds")
rm(Outliers30)

# dubbel check if they occur in validationData, if so: remove.
list30 <- apply(validationData %>% select(starts_with("WAP")),1, max ) > -30
sum(list30) / length(list30)





# split and bind datasets to identify WAPS with zero variance:
trainingDataWAPS <- trainingData %>% select(starts_with("WAP"))
trainingDataINFO <- trainingData[,c((ncol(trainingData)-8):(ncol(trainingData)))]
validationDataWAPS <- validationData%>% select(starts_with("WAP"))
validationDataINFO <- validationData[,c((ncol(trainingData)-8):(ncol(trainingData)))]


# remoce rows (1) and columns (2) with zero variance in the trainingdata (V2) ----
listZVRows <- apply(trainingDataWAPS %>% select(starts_with("WAP")),1, var ) == 0
listZVCols <- apply(trainingDataWAPS %>% select(starts_with("WAP")),2, var ) == 0
sum(listZVRows) / length(listZVRows) # see how much % it is from the data
sum(listZVCols) / length(listZVCols) # see how much % it is from the data
trainingDataWAPS <- trainingDataWAPS[,!listZVCols] # remove
trainingDataWAPS <- trainingDataWAPS[!listZVRows,] # remove
trainingDataINFO <- trainingDataINFO[!listZVRows,] # remove



# identify rows (1) and columns (1) with zero variance in the validationdata (V2) ----
listZVRows2 <- apply(validationDataWAPS %>% select(starts_with("WAP")),1, var ) == 0
listZVCols2 <- apply(validationDataWAPS %>% select(starts_with("WAP")),2, var ) == 0
sum(listZVRows2) / length(listZVRows2) # see how much % it is from the data
sum(listZVCols2) / length(listZVCols2) # see how much % it is from the data
validationDataWAPS <- validationDataWAPS[!listZVRows2,] # remove
validationDataINFO <- validationDataINFO[!listZVRows2,] # remove




# select the columns of with ZERO VAR and save them for further investigation ----
trainingZVCols <- trainingData[,listZVCols]
validationZVCols <- validationData[listZVCols2] 
saveRDS(trainingZVCols, file = "data/train-zerovar-column")
saveRDS(validationZVCols, file = "data/validate-zerovar-column")
rm(trainingZVCols)
rm(validationZVCols)


# bind WAPS back to INFO for complete trainingData & validationData ----
trainingData <- cbind(trainingDataWAPS,trainingDataINFO)
validationData <- cbind(validationDataWAPS,validationDataINFO)
names(trainingData) # check if all columns are there
names(validationData) # check if all columns are there

rm(trainingDataWAPS)
rm(trainingDataINFO)
rm(validationDataWAPS)
rm(validationDataINFO)



# check validationData for duplicates!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#trainingDataDUP <- trainingData[,c((ncol(trainingData)-8):(ncol(trainingData)-1))]
#validationDataDUP <-  validationData[,c((ncol(validationData)-8):(ncol(validationData)-1))]

 
# check the duplicates
#trainingDataDUP <- duplicated(trainingDataDUP)
#sum(trainingDataDUP) / length(trainingDataDUP)
#validationDataDUP <- duplicated(validationDataDUP)
#sum(validationDataDUP) / length(validationDataDUP)

# remove them from the dataSETS

#listUniqueTrain <- apply(trainingDataNOTIME, 1, var ) == 0
#listUniqueVal <- apply(validationDataNOTIME, 1, var ) == 0
#sum(listUniqueVal) / length(listUniqueVal)
#listUniqueVal <- apply(validationDataNOTIME, 1, var ) == 0
#validationDataUnique <- validationDataNOTIME[!listZVRows2,] # remove

# remove duplicates 
#trainingDataUNIQUE <- unique(trainingDataDUP)
#validationDataUNIQUE <- unique(validationDataDUP)



# store the files in order to load them in another scirpt
saveRDS(trainingData, file = "data/trainingDataProc(V7).rds")
saveRDS(validationData, file = "data/validationDataProc(V7).rds")

# clean your global environment
rm(list = ls())




# Transform the rows that have measurements between -29 and 99 to -30
# trainingDataWAPS[trainingDataWAPS > -30]  <- -30
# trainingDataWAPS <- select(trainingData, starts_with("WAP"))
# trainingDataWAPS[trainingDataWAPS >= -30] <- trainingDataWAPS[trainingDataWAPS[1:520] >= -30] -30
# for(i in junk$nm) if(i %in% "B") junk$nm <- "b"

