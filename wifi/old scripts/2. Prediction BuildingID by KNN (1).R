###############################################################################
#                                                                             #
#   WIFI | TRAINING KNN MODEL ONE FOR BUILDINGID | VERSION 1.0 | by ELSE      #
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
indexTrain <- createDataPartition(y = trainingDataWAP$BUILDINGID, p = .25, list = FALSE)
setTraining <- trainingDataWAP[indexTrain,]




# set cross validation parameters ----
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                        preProc = c("center", "scale"), verboseIter = TRUE)


# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model ----
modelKnn <- train(BUILDINGID~., data = setTraining, method = "knn", trControl = CrossValidation)


# check the metrics ----
modelKnn

plot(modelKnn)



#see variable importance
varImp(modelKnn)


# make predictions with the model and predict the BuildingID of from the validationData ----
PredictedBuildingID_KNN <- predict(modelKnn, validationData)

plot(PredictedBuildingID_KNN)


#create a new column with predicted data
validationData$PredictedBuildingID_KNN <- PredictedBuildingID_KNN


# make a confusion matrix between the column with predicted values ----
# and actual values from that dataset, make sure column of validationData is also factor
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)

validationData$PredictedBuildingID_KNN <- as.factor(validationData$PredictedBuildingID_KNN)

confusionMatrix(validationData$PredictedBuildingID_KNN, validationData$BUILDINGID)



#make scatterpot to look at the errors of prediction on test data
#testing$errors <- testing$brand - testing$Prediction
#ggplot(testing) + geom_point(aes(x = age, 
#                                 y = salary, 
#                                 color = (errors != 0)))





