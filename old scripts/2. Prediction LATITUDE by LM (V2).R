###############################################################################
#                                                                             #
#   WIFI | TRAINING LINEAR MODEL FOR LATITUDE | VERSION 2.0 | by ELSE         #
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


# load training data file
trainingData <- read.csv("data/trainingData.csv")
validationData <- read.csv("data/validationData.csv")





# select dependent variable and sample your dataset ----
# make sure LATITUDE is numerical to run a regression
trainingData$LATITUDE <- as.numeric(trainingData$LATITUDE)



# remove other columns in order to predict LATITUDE by the values of WAPs
trainingDataWAP <- select(trainingData, -FLOOR, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, -USERID, 
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
modelLM <- train(LATITUDE~., data = setTraining, method = "lm", trControl = CrossValidation)


# check the metrics ----
modelLM



#see variable importance
varImp(modelLM)


# make predictions with the model and predict the BuildingID of from the validationData ----
predLATITUDE_LM <- predict(modelLM, validationData)


#create a new column with predicted data
validationData$predLATITUDE_LM <- predLATITUDE_LM


# check the metrics postResample() for regression and confusionMatrix() for classification ---
postResample(validationData$predLATITUDE_LM, validationData$LATITUDE)


# and actual values from that dataset, make sure column of validationData is also factor
validationData$LATITUDE <- as.numeric(validationData$LATITUDE)

validationData$predLATITUDE_LM <- as.numeric(validationData$predLATITUDE_LM)


