###############################################################################
#                                                                             #
#   WIFI | TRAINING LINEAR MODEL FOR LONGITUDE | VERSION 2.0 | by ELSE        #
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


# load training data file
trainingData <- read.csv("data/trainingData.csv")
validationData <- read.csv("data/validationData.csv")





# select dependent variable and sample your dataset ----
# make sure LONGITUDE is numerical to run a regression
trainingData$LONGITUDE <- as.numeric(trainingData$LONGITUDE)



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
                                preProc = c("center", "scale"), verboseIter = TRUE)


# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model ----
modelLM <- train(LONGITUDE~., data = setTraining, method = "lm", trControl = CrossValidation)


# check the metrics ----
modelLM



#see variable importance
varImp(modelLM)


# make predictions with the model and predict the LONGITUDE of from the validationData ----
predLONGITUDE_LM <- predict(modelLM, validationData)


# check the metrics postResample() for regression and confusionMatrix() for classification ---
postResample(validationData$predLONGITUDE_LM, validationData$LONGITUDE)

#create a new column with predicted data
validationData$predLONGITUDE_LM <- predLONGITUDE_LM

validationData$LONGITUDE <- as.numeric(validationData$LONGITUDE)
validationData$predLONGITUDE_LM <- as.numeric(validationData$predLONGITUDE_LM)

