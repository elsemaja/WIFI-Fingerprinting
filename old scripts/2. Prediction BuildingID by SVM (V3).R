###############################################################################
#                                                                             #
#   WIFI | TRAINING SVM FOR BUILDINGID | VERSION 3.0 | by ELSE                #
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


# load the preprocessed dataframes
trainingData <- readRDS(file = "data/trainingDataProc(V4).rds")
validationData <- readRDS(file = "data/validationDataProc(V4).rds")

# select dependent variable and sample your dataset ----
# I want to predict building ID only on the dBm measured by the WAPs, 
# therefore remove other columns
trainingDataWAP <- select(trainingData, -FLOOR, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)
validationData <- select(validationData, -FLOOR, -SPACEID, -RELATIVEPOSITION, -USERID, 
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
predBUILDING_SVM <- predict(modelSVM, validationData)

plot(predBUILDING_SVM)


#create a new column with predicted data
validationData$predBUILDING_SVM <- predBUILDING_SVM

# and actual values from that dataset, make sure column of validationData is also factor
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
validationData$predBUILDING_SVM <- as.factor(validationData$predBUILDING_SVM)


# check the metrics postResample() for regression and confusionMatrix() for classification ---
confusionMatrix(validationData$predBUILDING_SVM, validationData$BUILDINGID)

# visualize the errors ----
# add column with errors to the dataframe
validationData$predBUILDING_SVM <- as.integer(validationData$predBUILDING_SVM)
validationData$BUILDINGID <- as.integer(validationData$BUILDINGID)
validationData  <- mutate(validationData, errorsBUILDING = predBUILDING_SVM - BUILDINGID) 

# turn the errors back into factors to produce an easy to read plot
validationData$errorsBUILDING <- as.factor(validationData$errorsBUILDING)

plot(validationData$errorsBUILDING,
     main = "BUILDING predictions",
     xlab = "correct = 0 | incorrect != 0",
     ylab = "count")


# store the predicted values, actual values and errors in a tibble ----
resultsBUILDING <- tibble(.rows = 1111)

resultsBUILDING$predBUILDING_SVM <- predBUILDING_SVM
resultsBUILDING$BUILDINGID <- validationData$BUILDINGID

# make numerical in order to mutate the errors and add them to the tibble
resultsBUILDING$predBUILDING_SVM <- as.integer(resultsBUILDING$predBUILDING_SVM)
resultsBUILDING$BUILDINGID <- as.integer(resultsBUILDING$BUILDINGID)

resultsBUILDING  <- mutate(resultsBUILDING, errorsBUILDING = predBUILDING_SVM - BUILDINGID) 
