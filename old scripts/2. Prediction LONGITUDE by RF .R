###############################################################################
#                                                                             #
#   WIFI | TRAINING RF  MODEL FOR LONGITUDE | FINAL VERSION | by ELSE         #
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
library("tidyverse")
library("class")
library("readr")
library("corrplot")
library("plotly")



# load the preprocessed dataframes
trainingData <- readRDS(file = "data/trainingDataProc(V7).rds")
validationData <- readRDS(file = "data/validationDataProc(V7).rds")

trainingData$FLOOR <- as.factor(trainingData$FLOOR)

# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingData$LONGITUDE, p = .05, list = FALSE)
setTraining <- trainingData[indexTrain,]
setTest <- trainingData[-indexTrain,]

# I want to predict FLOOR only on the dBm measured by the WAPs, 
# therefore I remove other columns
setTraining <- select(setTraining, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, 
                      -USERID, -PHONEID, -TIMESTAMP, -FLOOR, -LATITUDE)






# set cross validation parameters ----
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                                preProc = c("center", "scale", "range"), verboseIter = TRUE)


# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model ----
modelRF <- train(LONGITUDE~., data = setTraining, method = "rf", trControl = CrossValidation)


# check the metrics ----
modelRF




# make predictions with the model and predict the LONGITUDE of from the TRAININGDATA ----
predLONGITUDE_RF <- predict(modelRF, setTest)


#create a new column with predicted data
setTest$predLONGITUDE_RF <- predLONGITUDE_RF


setTest$LONGITUDE <- as.numeric(setTest$LONGITUDE)
setTest$predLONGITUDE_RF <- as.numeric(setTest$predLONGITUDE_RF)

# check the metrics postResample() for regression and confusionMatrix() for classification ---
postResample(setTest$predLONGITUDE_RF, setTest$LONGITUDE)










# make predictions with the model and predict the LONGITUDE of from the validationData ----
predLONGITUDE_RF <- predict(modelRF, validationData)


#create a new column with predicted data
validationData$predLONGITUDE_RF <- predLONGITUDE_RF


validationData$LONGITUDE <- as.numeric(validationData$LONGITUDE)
validationData$predLONGITUDE_RF <- as.numeric(validationData$predLONGITUDE_RF)

# check the metrics postResample() for regression and confusionMatrix() for classification ---
postResample(validationData$predLONGITUDE_RF, validationData$LONGITUDE)




# add column with errors to the dataframe
validationData  <- mutate(validationData, errorsLONGITUDE = predLONGITUDE_RF - LONGITUDE) 

# turn the errors back into factors to produce an easy to read plot
plot(validationData$errorsLONGITUDE,
     main = "LONGITUDE predictions",
     xlab = "meters",
     ylab = "count")


# subset the errors  
wrongLONGITUDE <-validationData %>%
  filter(errorsLONGITUDE >= 8)
rightLONGITUDE <-validationData %>%
  filter(errorsLONGITUDE <= 8)

# what do the errors have in common?
wrongLONGITUDE[,521:531]



ggplot(validationData, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter()+
  geom_jitter(aes(colour = (errorsLONGITUDE > 100 | errorsLONGITUDE < -100)))+
  theme_classic() +
  facet_wrap(~FLOOR) +
  labs(title="Errors LONGITUDE > 8 meters",
       subtitle = "Divided by FLOOR")

ggplot(validationData, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter()+
  geom_jitter(aes(colour = (errorsLONGITUDE < 8 | errorsLONGITUDE > -8)))+
  theme_classic() +
  facet_wrap(~FLOOR) +
  labs(title="Errors LONGITUDE < 8 meters",
       subtitle = "Divided by FLOOR")




#Move the info to the front
wrongLONGITUDE_Gathered <- wrongLONGITUDE[ , c((ncol(wrongLONGITUDE)-10):(ncol(wrongLONGITUDE)), 1:(ncol(wrongLONGITUDE)-11))]
rightLONGITUDE_Gathered <- rightLONGITUDE[ , c((ncol(rightLONGITUDE)-10):(ncol(rightLONGITUDE)), 1:(ncol(rightLONGITUDE)-11))]

# gather the data 
wrongLONGITUDE_Gathered <- gather(wrongLONGITUDE_Gathered, WAP, DBM, 12:ncol(wrongLONGITUDE_Gathered))
rightLONGITUDE_Gathered <- gather(rightLONGITUDE_Gathered, WAP, DBM, 12:ncol(rightLONGITUDE_Gathered))

# write CSV to understand which WAPS are making the errors
write.csv(wrongLONGITUDE_Gathered, file = "data/wrongLONGITUDE_Gathered.csv")
write.csv(rightLONGITUDE_Gathered, file = "data/rightLONGITUDE_Gathered.csv")






# save the errors for later
saveRDS(wrongFLOOR, file = "data/errorsFLOOR-training.rds")


# combine the predicted results and the corresponding errors in a tibble or datafrme ---
resultsLONGITUDE <- tibble(.rows = 1111)


# add LONGITUDE and its prediction to the tibble ----
resultsLONGITUDE$predLONGITUDE_RF <- predLONGITUDE_RF
resultsLONGITUDE$LONGITUDE <- validationData$LONGITUDE


# mutate the errors and add them to the tibble
resultsLONGITUDE  <- mutate(resultsLONGITUDE, errorsLONGITUDE = predLONGITUDE_RF - LONGITUDE) 
resultsLONGITUDE$errorsLONGITUDE <- resultsLONGITUDE$predLONGITUDE_RF - resultsLONGITUDE$LONGITUDE



# store as RDS
saveRDS(resultsLONGITUDE, file = "resultsLONGITUDE(V7).rds")
