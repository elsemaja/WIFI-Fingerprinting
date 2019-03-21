###############################################################################
#                                                                             #
#   WIFI | TRAINING SVM FOR BUILDINGID | VERSION 7.0 | by ELSE                #
#                                                                             #
#   Sample & subset data with only WAPS as predictors, train model & predict  #
#                                                                             #
###############################################################################

# remove WAP WAP058 and WAP057 from the DATASETS

# take a representable sample from the training dataset in order to train a model
# taking a sample will save time while running the first model(s)
# use only WAP columns to predict building ID

# libraries & data----
library("caret")
library("dplyr")
library("tidyverse")
library("class")
library("plotly")
library("tidyr")


# load the preprocessed dataframes and preprocess dependent variable
trainingData <- readRDS(file = "data/trainingDataProc(V7).rds")
validationData <- readRDS(file = "data/validationDataProc(V7).rds")
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)


# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingData$BUILDINGID, p = .05, list = FALSE)
setTraining <- trainingData[indexTrain,]
setTest <- trainingData[-indexTrain,]

# select dependent variable and sample your dataset ----
setTraining <- select(setTraining, -FLOOR, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LONGITUDE, -LATITUDE)


# set cross validation parameters ----
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", 
                                number = 10, 
                                repeats = 1, 
                                preProc = c("center", "scale", "range"), 
                                verboseIter = TRUE)


# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model ----
modelSVM <- train(BUILDINGID~., 
                  data = setTraining, 
                  method = "svmLinear", 
                  trControl = CrossValidation)


# check the metrics and variable importance----
modelSVM
varImp(modelSVM)



###############################################################################
#                                                                             #
#                         predicting training data                            #
#                                                                             #
###############################################################################

# make predictions with the model and predict the BUILDING ID of from the trainingData ----
predBUILDING_SVM <- predict(modelSVM, setTest)

#create a new column with predicted data
setTest$predBUILDING_SVM <- predBUILDING_SVM


# check the metrics postResample() for regression and confusionMatrix() for classification ---
setTest$BUILDINGID <- as.factor(setTest$BUILDINGID)
setTest$predBUILDING_SVM <- as.factor(setTest$predBUILDING_SVM)
confusionMatrix(setTest$predBUILDING_SVM, setTest$BUILDINGID)


###############################################################################
#                                                                             #
#                         predicting validation data                          #
#                                                                             #
###############################################################################

# make predictions with the model and predict the BUILDING ID of from the validationData ----
predBUILDING_SVM <- predict(modelSVM, validationData)

#create a new column with predicted data
validationData$predBUILDING_SVM <- predBUILDING_SVM

# check the metrics postResample() for regression and confusionMatrix() for classification ---
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
validationData$predBUILDING_SVM <- as.factor(validationData$predBUILDING_SVM)
confusionMatrix(validationData$predBUILDING_SVM, validationData$BUILDINGID)


###############################################################################
#                                                                             #
#                         visualizing errors                                  #
#                                                                             #
###############################################################################

# visualize the errors ----
# add column with errors to the dataframe
validationData$predBUILDING_SVM <- as.integer(validationData$predBUILDING_SVM)
validationData$BUILDINGID <- as.integer(validationData$BUILDINGID)
validationData  <- mutate(validationData, errorsBUILDING = predBUILDING_SVM - BUILDINGID) 


# find out where the errors occur exactly
plot_ly(validationData, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorsBUILDING == 0, size =1) %>%
  layout(title = "Wrongly predicted BUILDINGID's",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'))) 

# subset the errors  
wrongBUILDING <-validationData %>%
  filter(errorsBUILDING != 0)


# what do the errors have in common?
wrongBUILDING[,c((ncol(wrongBUILDING)-10):(ncol(wrongBUILDING)))]

#Move the info to the front
wrongBUILDING_gathered <- wrongBUILDING[, c((ncol(wrongBUILDING)-10):(ncol(wrongBUILDING)), 
                                            1:(ncol(wrongBUILDING)-11))]

###############################################################################
#                                                                             #
#                         store the results                                   #
#                                                                             #
###############################################################################

# store the predicted values, actual values and errors in a tibble ----
resultsBUILDING <- tibble(.rows = 1111)

resultsBUILDING$predBUILDING_SVM <- predBUILDING_SVM
resultsBUILDING$BUILDINGID <- validationData$BUILDINGID

# make numerical in order to mutate the errors and add them to the tibble
resultsBUILDING$predBUILDING_SVM <- as.integer(resultsBUILDING$predBUILDING_SVM)
resultsBUILDING$BUILDINGID <- as.integer(resultsBUILDING$BUILDINGID)

resultsBUILDING  <- mutate(resultsBUILDING, 
                           errorsBUILDING = predBUILDING_SVM - BUILDINGID) 
resultsBUILDING$errorsBUILDING <- resultsBUILDING$predBUILDING_SVM - resultsBUILDING$BUILDINGID

# store as RDS
saveRDS(resultsBUILDING, file = "data/resultsBUILDING(V7).rds")
