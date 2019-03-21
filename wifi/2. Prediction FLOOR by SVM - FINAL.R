###############################################################################
#                                                                             #
#   WIFI | TRAINING SVM MODEL ONE FOR FLOOR | FINAL VERSION | by ELSE         #
#                                                                             #
#   Sample & subset data with only WAPS as predictors, train model & predict  #
#                                                                             #
###############################################################################

library("caret")
library("dplyr")
library("class")
library("plotly")

# take a representable sample from the training dataset in order to train a model
# taking a sample will save time while running the first model(s)
# use only WAP columns to predict FLOOR

# load the preprocessed dataframes and preprocess dependent variable
trainingData <- readRDS(file = "data/trainingDataProc(V7).rds")
validationData <- readRDS(file = "data/validationDataProc(V7).rds")
trainingData$FLOOR <- as.factor(trainingData$FLOOR)

# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingData$FLOOR, p = .05, list = FALSE)
setTraining <- trainingData[indexTrain,]
setTest <- trainingData[-indexTrain,]

# I want to predict FLOOR only on the dBm measured by the WAPs, 
# therefore I remove other columns
setTraining <- select(setTraining, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, 
                          -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE, -LATITUDE)

# set cross validation parameters
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                                preProc = c("center", "scale", "range"), verboseIter = TRUE)



# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model
modelSvm <- train(FLOOR~., data = setTraining, method = "svmLinear", trControl = CrossValidation)


# check the metrics and variable importance
modelSvm
varImp(modelSvm)


###############################################################################
#                                                                             #
#                         predicting training data                            #
#                                                                             #
###############################################################################
# make predictions with the model and predict the FLOOR of from the TRAININGDATA ----
predFLOOR_SVM <- predict(modelSvm, setTest)

#create a new column with predicted data
setTest$predFLOOR_SVM <- predFLOOR_SVM

# check the metrics postResample() for regression and confusionMatrix() for classification ---
setTest$FLOOR <- as.factor(setTest$FLOOR)
setTest$predFLOOR_SVM <- as.factor(setTest$predFLOOR_SVM)
confusionMatrix(setTest$predFLOOR_SVM, setTest$FLOOR)


# visualize the errors in the TRAININGDATA----
setTest$predFLOOR_SVM <- as.integer(setTest$predFLOOR_SVM)
setTest$FLOOR <- as.integer(setTest$FLOOR)
setTest  <- mutate(setTest, errorsFLOOR = predFLOOR_SVM - FLOOR) 


# visualize where the errors occur exactly
plot_ly(setTest, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorsFLOOR == 0, size = 1) %>%
  layout(title = "Wrongly predicted FLOORS trainingData",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'))) 

# subset the errors  
wrongFLOOR <-setTest %>%
  filter(errorsFLOOR != 0)
rightFLOOR <-setTest %>%
  filter(errorsFLOOR == 0)

# what do the errors have in common?
wrongFLOOR[,465:475]
saveRDS(wrongFLOOR, file = "data/errorsFLOOR-training.rds")


###############################################################################
#                                                                             #
#                         predicting validation data                          #
#                                                                             #
###############################################################################


# make predictions with the model and predict the FLOOR of from the validationData ----
predFLOOR_SVM <- predict(modelSvm, validationData)


#create a new column with predicted data
validationData$predFLOOR_SVM <- predFLOOR_SVM

# make a confusion matrix between the column with predicted and actual values from that dataset, 
validationData$predFLOOR_SVM <- as.factor(validationData$predFLOOR_SVM)
validationData$FLOOR <- as.factor(validationData$FLOOR)
confusionMatrix(validationData$predFLOOR_SVM, validationData$FLOOR)

# add column with errors to the dataframe
validationData$predFLOOR_SVM <- as.integer(validationData$predFLOOR_SVM)
validationData$FLOOR <- as.integer(validationData$FLOOR)
validationData  <- mutate(validationData, errorsFLOOR = predFLOOR_SVM - FLOOR) 

# subset the errors  
wrongFLOORVal <-validationData %>%
  filter(errorsFLOOR != 0)
wrongFLOORVal <-validationData %>%
  filter(errorsFLOOR == 0)

# what do the errors have in common?
wrongFLOORVal[,521:531]
write.csv(wrongFLOORVal, file = "data/errorsfloorbuilding1.csv")


###############################################################################
#                                                                             #
#                         visualizing errors                                  #
#                                                                             #
###############################################################################

wrongFLOORVal$predFLOOR_SVM <- as.factor(wrongFLOORVal$predFLOOR_SVM)
plot_ly(wrongFLOORVal, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~predFLOOR_SVM) %>%
  layout(title = "Wrongly predicted FLOORS",
         scene = list(xaxis = list(title = ''),
                      yaxis = list(title = ''),
                      zaxis = list(title = ''))) 

saveRDS(wrongFLOOR, file = "data/errorsFLOOR-validation.rds")

###############################################################################
#                                                                             #
#                         store the results                                   #
#                                                                             #
###############################################################################
# store the predicted values, actual values and errors in a tibble ----
resultsFLOOR <- tibble(.rows = 1111)

# add FLOOR and its prediction to the tibble ---
resultsFLOOR$predFLOOR_SVM <- predFLOOR_SVM
resultsFLOOR$FLOOR <- validationData$FLOOR

# make numerical in order to mutate the errors and add them to the tibble
resultsFLOOR$predFLOOR_SVM <- as.integer(resultsFLOOR$predFLOOR_SVM)
resultsFLOOR$FLOOR <- as.integer(resultsFLOOR$FLOOR)
resultsFLOOR  <- mutate(resultsFLOOR, errorsFLOOR = predFLOOR_SVM - FLOOR) 


# store as RDS
saveRDS(resultsFLOOR, file = "data/resultsFLOOR(V7).rds")