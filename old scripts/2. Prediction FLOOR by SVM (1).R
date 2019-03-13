###############################################################################
#                                                                             #
#   WIFI | TRAINING SVM FOR FLOOR | VERSION 1.0 | by ELSE                     #
#                                                                             #
#   Sample & subset data with only WAPS as predictors, train model & predict  #
#                                                                             #
###############################################################################


# this takes a lot of time!


# take a representable sample from the training dataset in order to train a model
# taking a sample will save time while running the first model(s)
# use only WAP columns to predict FLOOR


# libraries & data----
library("caret")
library("dplyr")
library("class")
library("readr")


# load training data file
trainingData <- read.csv("data/trainingData.csv")
validationData <- read.csv("data/validationData.csv")



# select dependent variable and sample your dataset ----
# set FLOOR as factor for both data sets, this is your dependent variable
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
validationData$FLOOR <- as.factor(validationData$FLOOR)



# I want to predict building ID only on the dBm measured by the WAPs, 
# therefore remove other columns
trainingDataWAP <- select(trainingData, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)



# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingDataWAP$FLOOR, p = .25, list = FALSE)
setTraining <- trainingDataWAP[indexTrain,]





# set cross validation parameters ----
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                                preProc = c("center", "scale"), verboseIter = TRUE)


# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model ----
modelSVM <- train(FLOOR~., data = setTraining, method = "svmLinear", trControl = CrossValidation)


# check the metrics ----
modelSVM

plot(modelSVM)



#see variable importance
varImp(modelSVM)


# make predictions with the model and predict the FLOOR of from the validationData ----
predictedFLOOR_SVM <- predict(modelSVM, validationData)

plot(predictedFLOOR_SVM)


#create a new column with predicted data
validationData$predictedFLOOR_SVM <- predictedFLOOR_SVM


# make a confusion matrix between the column with predicted values ----
# and actual values from that dataset, make sure column of validationData is also factor
validationData$FLOOR <- as.factor(validationData$FLOOR)

validationData$predictedFLOOR_SVM <- as.factor(validationData$predictedFLOOR_SVM)

confusionMatrix(validationData$predictedFLOOR_SVM, validationData$FLOOR)



#make scatterpot to look at the errors of prediction on test data
#resultsFLOOR <- tibble(.rows = 1111)
#resultsFLOOR$predLONGITUDE_LM <- predLONGITUDE_LM
#resultsFLOOR$LONGITUDE <- validationData$LONGITUDE





