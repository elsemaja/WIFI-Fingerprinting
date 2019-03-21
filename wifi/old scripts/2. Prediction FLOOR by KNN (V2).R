###############################################################################
#                                                                             #
#   WIFI | TRAINING KNN MODEL ONE FOR FLOOR | VERSION 2.0 | by ELSE           #
#                                                                             #
#   Sample & subset data with only WAPS as predictors, train model & predict  #
#                                                                             #
###############################################################################

library("caret")
library("dplyr")
library("class")

# take a representable sample from the training dataset in order to train a model
# taking a sample will save time while running the first model(s)
# use only WAP columns to predict FLOOR

# load training data file
trainingData <- read.csv("data/trainingData.csv")
validationData <- read.csv("data/validationData.csv")

# set FLOOR as factor for both data sets
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
validationData$FLOOR <- as.factor(validationData$FLOOR)


# I want to predict FLOOR only on the dBm measured by the WAPs, therefore I remove other columns
trainingDataWAP <- select(trainingData, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)



# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingDataWAP$FLOOR, p = .05, list = FALSE)
setTraining <- trainingDataWAP[indexTrain,]



# set cross validation parameters
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                                preProc = c("center", "scale"), verboseIter = TRUE)



# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model
modelKnn <- train(FLOOR~., data = setTraining, method = "knn", trControl = CrossValidation)


# check the metrics
modelKnn
plot(modelKnn)



#see variable importance
varImp(modelKnn)


# make predictions with the model and predict the FLOOR of from the validationData
PredictedFLOOR_KNN <- predict(modelKnn, validationData)
plot(PredictedFLOOR_KNN)



#create a new column with predicted data
validationData$PredictedFLOOR_KNN <- PredictedFLOOR_KNN


# make a confusion matrix between the column with predicted and actual values from that dataset, 
# make sure column of validationData is also factor
validationData$PredictedFLOOR_KNN <- as.factor(validationData$PredictedFLOOR_KNN)
confusionMatrix(validationData$PredictedFLOOR_KNN, validationData$FLOOR)



