###############################################################################
#                                                                             #
#   WIFI | TRAINING BOOSTED TREE MODEL ONE FOR FLOOR | VERSION 1.0 | by ELSE  #
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
indexTrain <- createDataPartition(y = trainingDataWAP$FLOOR, p = .25, list = FALSE)
setTraining <- trainingDataWAP[indexTrain,]



# set cross validation parameters
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 5, repeats = 1, 
                                preProc = c("center", "scale"), verboseIter = TRUE)



# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model
modelbstTree <- train(FLOOR~., data = setTraining, method = "bstTree", trControl = CrossValidation)


# check the metrics
modelbstTree
plot(modelbstTree)



#see variable importance
varImp(modelbstTree)


# make predictions with the model and predict the FLOOR of from the validationData
PredictedFLOOR_bstTree <- predict(modelbstTree, validationData)
plot(PredictedFLOOR_bstTree)



#create a new column with predicted data
validationData$PredictedFLOOR_bstTree <- PredictedFLOOR_bstTree


# make a confusion matrix between the column with predicted and actual values from that dataset, 
# make sure column of validationData is also factor
validationData$PredictedFLOOR_bstTree <- as.factor(validationData$PredictedFLOOR_bstTree)
confusionMatrix(validationData$PredictedFLOOR_bstTree, validationData$FLOOR)



# make scatterpot to look at the errors of prediction on test data
# testing$errors <- testing$brand - testing$Prediction
# ggplot(testing) + geom_point(aes(x = age, 
#                                 y = salary, 
#                                 color = (errors != 0)))


