###############################################################################
#                                                                             #
#   WIFI | TRAINING CART FOR LONGITUDE | VERSION 1.0 | by ELSE                #
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


# load training data file
trainingData <- read.csv("data/trainingData.csv")
validationData <- read.csv("data/validationData.csv")



# select dependent variable and sample your dataset ----
# make sure LONGITUDE is numerical to run a regression
trainingData$LONGITUDE <- as.numeric(trainingData$LONGITUDE)



# remove other columns in order to predict LONGITUDE by the values of WAPs
trainingDataWAP <- select(trainingData, - FLOOR, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LATITUDE)



# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingDataWAP$LONGITUDE, p = .25, list = FALSE)
setTraining <- trainingDataWAP[indexTrain,]





# set cross validation parameters ----
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                                preProc = c("center", "scale"), verboseIter = TRUE)


# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model ----
modelCART <- train(LONGITUDE~., data = setTraining, method = "rpart", trControl = CrossValidation)


# check the metrics ----
modelCART

plot(modelCART)



#see variable importance
varImp(modelCART)


# make predictions with the model and predict the BuildingID of from the validationData ----
predictedCART <- predict(modelCART, validationData)

plot(predictedCART)


#create a new column with predicted data
validationData$Predicted <- predictedCART


# make a confusion matrix between the column with predicted values ----
# and actual values from that dataset, make sure column of validationData is also factor
validationData$LONGITUDE <- as.numeric(validationData$LONGITUDE)

validationData$Predicted <- as.numeric(validationData$Predicted)

confusionMatrix(validationData$Predicted, validationData$LONGITUDE)



#make scatterpot to look at the errors of prediction on test data
#testing$errors <- testing$brand - testing$Prediction
#ggplot(testing) + geom_point(aes(x = age, 
#                                 y = salary, 
#                                 color = (errors != 0)))





