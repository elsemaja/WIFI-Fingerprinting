###############################################################################
#                                                                             #
#   WIFI | TRAINING KNN  FOR LATITUDE | VERSION 3.0 | by ELSE                 #
#                                                                             #
#   Sample & subset data with only WAPS as predictors, train model & predict  #
#                                                                             #
###############################################################################

# take a representable sample from the training dataset in order to train a model
# taking a sample will save time while running the first model(s)
# use only WAP columns to predict: LATITUDE

# libraries & data----
library("caret")
library("dplyr")
library("class")
library("readr")
library("corrplot")


# load the preprocessed dataframes
trainingData <- readRDS(file = "data/trainingDataProc.rds")
validationData <- readRDS(file = "data/validationDataProc.rds")




# select dependent variable and sample your dataset ----
# remove other columns in order to predict LATITUDE by the values of WAPs
trainingDataWAP <- select(trainingData, -FLOOR, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LONGITUDE)
validationData <- select(validationData, -FLOOR, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, -USERID, 
                         -PHONEID, -TIMESTAMP, -LONGITUDE)


# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingDataWAP$LATITUDE, p = .05, list = FALSE)
setTraining <- trainingDataWAP[indexTrain,]





# set cross validation parameters ----
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                                preProc = c("center", "scale"), verboseIter = TRUE)


# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model ----
modelKNN <- train(LATITUDE~., data = setTraining, method = "knn", trControl = CrossValidation)


# check the metrics ----
modelKNN



#see variable importance
varImp(modelKNN)


# make predictions with the model and predict the BuildingID of from the validationData ----
predLATITUDE_KNN <- predict(modelKNN, validationData)


#create a new column with predicted data
validationData$predLATITUDE_KNN <- predLATITUDE_KNN


# check the metrics postResample() for regression and confusionMatrix() for classification ---
postResample(validationData$predLATITUDE_KNN, validationData$LATITUDE)


# and actual values from that dataset, make sure column of validationData is also factor
validationData$LATITUDE <- as.numeric(validationData$LATITUDE)
validationData$predLATITUDE_KNN <- as.numeric(validationData$predLATITUDE_KNN)


# add column with errors to the dataframe
validationData  <- mutate(validationData, errorsLATITUDE = predLATITUDE_KNN - LATITUDE) 

# turn the errors back into factors to produce an easy to read plot
plot(validationData$errorsLATITUDE,
     main = "LATITUDE predictions",
     xlab = "meters",
     ylab = "count")

