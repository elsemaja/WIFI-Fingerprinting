###############################################################################
#                                                                             #
#   WIFI | CREATE TIBBLE/DATAFRAME WITH RESULTS | VERSION 1.0 | by ELSE       #
#                                                                             #
#   combine all the predicted values in one table or dataframe for comparison #
#                                                                             #
###############################################################################


# Load previous scripts
source(file = "1. Loading and getting to know the data.R")
source(file = "2. Prediction FLOOR by KNN (V2).R")
source(file = "2. Prediction BuildingID by SVM (V2).R")
source(file = "2. Prediction LATITUDE by KNN (V2).R")
source(file = "2. Prediction LONGITUDE by KNN (V2).R")

# combine the predicted results and the corresponding errors in a tibble or datafrme ---
resultsVSactual <- tibble(.rows = 1111)

# add FLOOR and its prediction to the tibble ---
resultsVSactual$PredictedFLOOR_KNN <- PredictedFLOOR_KNN
resultsVSactual$FLOOR <- validationData$FLOOR

# make numerical in order to mutate the errors and add them to the tibble
resultsVSactual$PredictedFLOOR_KNN <- as.integer(resultsVSactual$PredictedFLOOR_KNN)
resultsVSactual$FLOOR <- as.integer(resultsVSactual$FLOOR)
resultsVSactual  <- mutate(resultsVSactual, errorsFLOOR = PredictedFLOOR_KNN - FLOOR) 




# add BUILDING and its prediction to the tibble ----
resultsVSactual$PredictedBUIDLING_SVM <- predictedBUILDING_SVM
resultsVSactual$BUILDINGID <- validationData$BUILDINGID

# make numerical in order to mutate the errors and add them to the tibble
resultsVSactual$PredictedBUIDLING_SVM <- as.integer(resultsVSactual$PredictedBUIDLING_SVM)
resultsVSactual$BUILDINGID <- as.integer(resultsVSactual$BUILDINGID)

resultsVSactual  <- mutate(resultsVSactual, errorsBUILDING = PredictedBUIDLING_SVM - BUILDINGID) 




# add LATITUDE and its prediction to the tibble ----
resultsVSactual$predLATITUDE_KNN <- predLATITUDE_KNN
resultsVSactual$LATITUDE <- validationData$LATITUDE

# mutate the errors and add them to the tibble
resultsVSactual  <- mutate(resultsVSactual, errorsLATITUDE = predLATITUDE_KNN - LATITUDE) 




# add LONGITUDE and its prediction to the tibble ----
resultsVSactual$predLONGITUDE_KNN <- predLONGITUDE_KNN
resultsVSactual$LONGITUDE <- validationData$LONGITUDE

# mutate the errors and add them to the tibble
resultsVSactual  <- mutate(resultsVSactual, errorsLONGITUDE = predLONGITUDE_KNN - LONGITUDE) 
resultsVSactual$errorsLONGITUDE <- resultsVSactual$predLONGITUDE_KNN - resultsVSactual$LONGITUDE


# save tibble with all results in a file
saveRDS(resultsVSactual, file = "resultsVSactual(2).rds")



