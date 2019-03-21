###############################################################################
#                                                                             #
#   WIFI | TRAINING KNN MODEL ONE FOR FLOOR | VERSION 4.0 | by ELSE           #
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

# load the preprocessed dataframes
trainingData <- readRDS(file = "data/trainingDataProc(V5).rds")
validationData <- readRDS(file = "data/validationDataProc(V5).rds")

trainingData$FLOOR <- as.factor(trainingData$FLOOR)

# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingData$FLOOR, p = .05, list = FALSE)
setTraining <- trainingData[indexTrain,]
setTest <- trainingData[-indexTrain,]

# I want to predict FLOOR only on the dBm measured by the WAPs, 
# therefore I remove other columns
setTraining <- select(setTraining, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, 
                          -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)

# set cross validation parameters
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                                preProc = c("center", "scale", "range"), verboseIter = TRUE)



# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model
modelKnn <- train(FLOOR~., data = setTraining, method = "knn", trControl = CrossValidation)


# check the metrics
modelKnn



#see variable importance
varImp(modelKnn)





# make predictions with the model and predict the BUILDING ID of from the TRAININGDATA ----
predFLOOR_KNN <- predict(modelKnn, setTest)

plot(predFLOOR_KNN)


#create a new column with predicted data
setTest$predFLOOR_KNN <- predFLOOR_KNN

# and actual values from that dataset, make sure column of validationData is also factor
setTest$FLOOR <- as.factor(setTest$FLOOR)
setTest$predFLOOR_KNN <- as.factor(setTest$predFLOOR_KNN)


# check the metrics postResample() for regression and confusionMatrix() for classification ---
confusionMatrix(setTest$predFLOOR_KNN, setTest$FLOOR)




# visualize the errors ----
# add column with errors to the dataframe
setTest$predFLOOR_KNN <- as.integer(setTest$predFLOOR_KNN)
setTest$FLOOR <- as.integer(setTest$FLOOR)
setTest  <- mutate(setTest, errorsFLOOR = predFLOOR_KNN - FLOOR) 

# turn the errors back into factors to produce an easy to read plot
setTest$errorsFLOOR <- as.factor(setTest$errorsFLOOR)

plot(setTest$errorsFLOOR,
     main = "BUILDING predictions",
     xlab = "correct = 0 | incorrect != 0",
     ylab = "count")

# find out where the errors occur exactly
plot_ly(setTest, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorsFLOOR == 0, size = 1) %>%
  layout(title = "Wrongly predicted FLOORS",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'))) 

names(setTest)

# subset the errors  
wrongFLOOR <-setTest %>%
  filter(errorsFLOOR != 0)
rightFLOOR <-setTest %>%
  filter(errorsFLOOR == 0)

# what do the errors have in common?
wrongFLOOR[,465:475]

saveRDS(wrongFLOOR, file = "data/errorsFLOOR-training.rds")


#####################################################







# make predictions with the model and predict the FLOOR of from the validationData ----
predFLOOR_KNN <- predict(modelKnn, validationData)
plot(predFLOOR_KNN)



#create a new column with predicted data
validationData$predFLOOR_KNN <- predFLOOR_KNN

# make a confusion matrix between the column with predicted and actual values from that dataset, 
# make sure column of validationData is also factor
validationData$predFLOOR_KNN <- as.factor(validationData$predFLOOR_KNN)
validationData$FLOOR <- as.factor(validationData$FLOOR)

confusionMatrix(validationData$predFLOOR_KNN, validationData$FLOOR)

# add column with errors to the dataframe
validationData$predFLOOR_KNN <- as.integer(validationData$predFLOOR_KNN)
validationData$FLOOR <- as.integer(validationData$FLOOR)
validationData  <- mutate(validationData, errorsFLOOR = predFLOOR_KNN - FLOOR) 

# turn the errors back into factors to produce an easy to read plot
validationData$errorsFLOOR <- as.factor(validationData$errorsFLOOR)
plot(validationData$errorsFLOOR,
     main = "FLOOR predictions",
     xlab = "correct = 0 | incorrect != 0",
     ylab = "count")

# store the predicted values, actual values and errors in a tibble ----
resultsFLOOR <- tibble(.rows = 1111)

# add FLOOR and its prediction to the tibble ---
resultsFLOOR$predFLOOR_KNN <- predFLOOR_KNN
resultsFLOOR$FLOOR <- validationData$FLOOR

# make numerical in order to mutate the errors and add them to the tibble
resultsFLOOR$predFLOOR_KNN <- as.integer(resultsFLOOR$predFLOOR_KNN)
resultsFLOOR$FLOOR <- as.integer(resultsFLOOR$FLOOR)
resultsFLOOR  <- mutate(resultsFLOOR, errorsFLOOR = predFLOOR_KNN - FLOOR) 


# store as RDS
saveRDS(resultsFLOOR, file = "resultsFLOOR(V5).rds")


plot_ly(validationData, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorsFLOOR == 0, size = 1) %>%
  layout(title = "Wrongly predicted FLOORS",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'))) 

validationData$predFLOOR_KNN <- as.factor(validationData$predFLOOR_KNN)
ggplot(validationData, aes(x=LONGITUDE, y=LATITUDE, color = predFLOOR_KNN))+
  geom_point() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="Wrongly predicted FLOORS",
       subtitle = "")

