###############################################################################
#                                                                             #
#   WIFI | TRAINING KNN MODEL FOR LATITUDE | FINAL VERSION  | by ELSE          #
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

# load the preprocessed dataframes
trainingData <- readRDS(file = "data/trainingDataProc(V7).rds")
validationData <- readRDS(file = "data/validationDataProc(V7).rds")

# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingData$LATITUDE, p = .05, list = FALSE)
setTraining <- trainingData[indexTrain,]
setTest <- trainingData[-indexTrain,]

# select dependent variable and sample your dataset ----
# remove other columns in order to predict LATITUDE by the values of WAPs
setTraining <- select(setTraining, -FLOOR, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, -USERID, 
                      -PHONEID, -TIMESTAMP, -LONGITUDE)

# set cross validation parameters ----
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", 
                                number = 10, 
                                repeats = 1, 
                                preProc = c("center", "scale", "range"), 
                                verboseIter = TRUE)

# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model ----
modelKNN <- train(LATITUDE~.,
                  data = setTraining, 
                  method = "knn", 
                  trControl = CrossValidation)

# check the metrics ----
modelKNN


###############################################################################
#                                                                             #
#                         predicting training data                            #
#                                                                             #
###############################################################################

# make predictions with the model and predict the LATITUDE of from the TRAININGDATA ----
predLATITUDE_KNN <- predict(modelKNN, setTest)

#create a new column with predicted data
setTest$predLATITUDE_KNN <- predLATITUDE_KNN

# check the metrics postResample() for regression and confusionMatrix() for classification ---
setTest$LATITUDE <- as.numeric(setTest$LATITUDE)
setTest$predLATITUDE_KNN <- as.numeric(setTest$predLATITUDE_KNN)
postResample(setTest$predLATITUDE_KNN, setTest$LATITUDE)

# add column with errors to the dataframe
setTest  <- mutate(setTest, errorsLATITUDE = predLATITUDE_KNN - LATITUDE) 

# subset the errors  
wrongLATITUDE <-setTest %>%
  filter(errorsLATITUDE >= 8)
wrongLATITUDE2 <-setTest %>%
  filter(errorsLATITUDE <= -8)

rightLATITUDE <-setTest %>%
  filter(errorsLATITUDE <= 8 & errorsLATITUDE >= -8)

# what do the errors have in common?
wrongLATITUDE[, c((ncol(wrongLATITUDE)-10):(ncol(wrongLATITUDE)))]



###############################################################################
#                                                                             #
#                         predicting validation data                          #
#                                                                             #
###############################################################################

# make predictions with the model and predict the BuildingID of from the validationData ----
predLATITUDE_KNN <- predict(modelKNN, validationData)

#create a new column with predicted data
validationData$predLATITUDE_KNN <- predLATITUDE_KNN

# check the metrics postResample() for regression and confusionMatrix() for classification ---
postResample(validationData$predLATITUDE_KNN, validationData$LATITUDE)

# add column with errors to the dataframe
validationData$LATITUDE <- as.numeric(validationData$LATITUDE)
validationData$predLATITUDE_KNN <- as.numeric(validationData$predLATITUDE_KNN)
validationData  <- mutate(validationData, 
                          errorsLATITUDE = predLATITUDE_KNN - LATITUDE) 

###############################################################################
#                                                                             #
#                         visualizing errors                                  #
#                                                                             #
###############################################################################

# turn the errors back into factors to produce an easy to read plot
plot(validationData$errorsLATITUDE,
     main = "LATITUDE predictions",
     xlab = "meters",
     ylab = "count")


# subset the errors  
wrongLATITUDE <-validationData %>%
  filter(errorsLATITUDE >= 8)
rightLATITUDE <-validationData %>%
  filter(errorsLATITUDE <= 8)

# what do the errors have in common?
wrongLATITUDE[,c((ncol(wrongBUILDING)-10):(ncol(wrongBUILDING)))]


ggplot(validationData, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter()+
  geom_jitter(aes(colour = (errorsLATITUDE >= 8 | errorsLATITUDE <= -8)))+
  theme_classic() +
  facet_wrap(~FLOOR) +
  labs(title="Errors LATITUDE > 8 meters",
       subtitle = "Divided by FLOOR")

#Move the info to the front
wrongLATITUDE_Gathered <- wrongLATITUDE[ , c((ncol(wrongLATITUDE)-10):(ncol(wrongLATITUDE)), 
                                             1:(ncol(wrongLATITUDE)-11))]
rightLATITUDE_Gathered <- rightLATITUDE[ , c((ncol(rightLATITUDE)-10):(ncol(rightLATITUDE)), 
                                             1:(ncol(rightLATITUDE)-11))]

# gather the data 
wrongLATITUDE_Gathered <- gather(wrongLATITUDE_Gathered, WAP, DBM, 
                                 12:ncol(wrongLATITUDE_Gathered))
rightLATITUDE_Gathered <- gather(rightLATITUDE_Gathered, WAP, DBM, 
                                 12:ncol(rightLATITUDE_Gathered))

# write CSV to understand which WAPS are making the errors
write.csv(wrongLATITUDE_Gathered, file = "data/wrongLATITUDE_Gathered.csv")
write.csv(rightLATITUDE_Gathered, file = "data/rightLATITUDE_Gathered.csv")


###############################################################################
#                                                                             #
#                         store the results                                   #
#                                                                             #
###############################################################################

# combine the predicted results and the corresponding errors in a tibble or datafrme ---
resultsLATITUDE <- tibble(.rows = 1111)

# add LATITUDE and its prediction to the tibble ----
resultsLATITUDE$predLATITUDE_KNN <- predLATITUDE_KNN
resultsLATITUDE$LATITUDE <- validationData$LATITUDE

# mutate the errors and add them to the tibble
resultsLATITUDE  <- mutate(resultsLATITUDE, 
                           errorsLATITUDE = predLATITUDE_KNN - LATITUDE) 
resultsLATITUDE$errorsLATITUDE <- resultsLATITUDE$predLATITUDE_KNN - resultsLATITUDE$LATITUDE

# store as RDS
saveRDS(resultsLATITUDE, file = "data/resultsLATITUDE(V7).rds")
