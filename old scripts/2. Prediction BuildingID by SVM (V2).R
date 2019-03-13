###############################################################################
#                                                                             #
#   WIFI | TRAINING SVM FOR BUILDINGID | VERSION 2.0 | by ELSE                #
#                                                                             #
#   Sample & subset data with only WAPS as predictors, train model & predict  #
#                                                                             #
###############################################################################


# take a representable sample from the training dataset in order to train a model
# taking a sample will save time while running the first model(s)
# use only WAP columns to predict building ID

# libraries & data----
library("caret")
library("dplyr")
library("class")


# load training data file
trainingData <- read.csv("data/trainingData.csv")
validationData <- read.csv("data/validationData.csv")


# select dependent variable and sample your dataset ----
# set BUILDING ID as factor for both data sets, this is your dependent variable
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)



# I want to predict building ID only on the dBm measured by the WAPs, 
# therefore remove other columns
trainingDataWAP <- select(trainingData, -FLOOR, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)



# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingDataWAP$BUILDINGID, p = .05, list = FALSE)
setTraining <- trainingDataWAP[indexTrain,]





# set cross validation parameters ----
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                                preProc = c("center", "scale"), verboseIter = TRUE)


# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model ----
modelSVM <- train(BUILDINGID~., data = setTraining, method = "svmLinear", trControl = CrossValidation)


# check the metrics ----
modelSVM



#see variable importance
varImp(modelSVM)


# make predictions with the model and predict the BUILDING ID of from the validationData ----
predictedBUILDING_SVM <- predict(modelSVM, validationData)

plot(predictedBUILDING_SVM)


#create a new column with predicted data
validationData$predictedBUILDING_SVM <- predictedBUILDING_SVM

# and actual values from that dataset, make sure column of validationData is also factor
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
validationData$predictedBUILDING_SVM <- as.factor(validationData$predictedBUILDING_SVM)


# check the metrics postResample() for regression and confusionMatrix() for classification ---
confusionMatrix(validationData$predictedBUILDING_SVM, validationData$BUILDINGID)









