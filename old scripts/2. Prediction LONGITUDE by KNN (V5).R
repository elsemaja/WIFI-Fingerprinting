###############################################################################
#                                                                             #
#   WIFI | TRAINING KNN MODEL FOR LONGITUDE | VERSION 3.0 | by ELSE           #
#                                                                             #
#   Sample & subset data with only WAPS as predictors, train model & predict  #
#                                                                             #
###############################################################################

# take a representable sample from the training dataset in order to train a model
# taking a sample will save time while running the first model(s)
# use only WAP columns to predict: LONGITUDE

# libraries & data----
library("caret")
library("dplyr")
library("class")
library("readr")
library("corrplot")



# load the preprocessed dataframes
trainingData <- readRDS(file = "data/trainingDataProc(V5).rds")
validationData <- readRDS(file = "data/validationDataProc(V5).rds")




# select dependent variable and sample your dataset ----
# remove other columns in order to predict LONGITUDE by the values of WAPs
trainingDataWAP <- select(trainingData, -FLOOR, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LATITUDE)




# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingDataWAP$LONGITUDE, p = .05, list = FALSE)
setTraining <- trainingDataWAP[indexTrain,]





# set cross validation parameters ----
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                                preProc = c("center", "scale", "range"), verboseIter = TRUE)


# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model ----
modelKNN <- train(LONGITUDE~., data = setTraining, method = "knn", trControl = CrossValidation)


# check the metrics ----
modelKNN



#see variable importance
varImp(modelKNN)


# make predictions with the model and predict the LONGITUDE of from the validationData ----
predLONGITUDE_KNN <- predict(modelKNN, validationData)


#create a new column with predicted data
validationData$predLONGITUDE_KNN <- predLONGITUDE_KNN


validationData$LONGITUDE <- as.numeric(validationData$LONGITUDE)
validationData$predLONGITUDE_KNN <- as.numeric(validationData$predLONGITUDE_KNN)

# check the metrics postResample() for regression and confusionMatrix() for classification ---
postResample(validationData$predLONGITUDE_KNN, validationData$LONGITUDE)




# add column with errors to the dataframe
validationData  <- mutate(validationData, errorsLONGITUDE = predLONGITUDE_KNN - LONGITUDE) 

# turn the errors back into factors to produce an easy to read plot
plot(validationData$errorsLONGITUDE,
     main = "LONGITUDE predictions",
     xlab = "meters",
     ylab = "count")


# combine the predicted results and the corresponding errors in a tibble or datafrme ---
resultsLONGITUDE <- tibble(.rows = 1111)


# add LONGITUDE and its prediction to the tibble ----
resultsLONGITUDE$predLONGITUDE_KNN <- predLONGITUDE_KNN
resultsLONGITUDE$LONGITUDE <- validationData$LONGITUDE


# mutate the errors and add them to the tibble
resultsLONGITUDE  <- mutate(resultsLONGITUDE, errorsLONGITUDE = predLONGITUDE_KNN - LONGITUDE) 
resultsLONGITUDE$errorsLONGITUDE <- resultsLONGITUDE$predLONGITUDE_KNN - resultsLONGITUDE$LONGITUDE



# store as RDS
saveRDS(resultsLONGITUDE, file = "resultsLONGITUDE(V5).rds")
