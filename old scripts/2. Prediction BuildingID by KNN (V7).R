###############################################################################
#                                                                             #
#   WIFI | TRAINING KNN FOR BUILDINGID | VERSION 7.0 | by ELSE                #
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


# load the preprocessed dataframes
trainingData <- readRDS(file = "data/trainingDataProc(V7).rds")
validationData <- readRDS(file = "data/validationDataProc(V7).rds")

trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)




# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingData$BUILDINGID, p = .05, list = FALSE)
setTraining <- trainingData[indexTrain,]
setTest <- trainingData[-indexTrain,]

# select dependent variable and sample your dataset ----
# I want to predict building ID only on the dBm measured by the WAPs, 
# therefore remove other columns
# remove WAP WAP058 and WAP057 from the DATASETS
setTraining <- select(setTraining, -FLOOR, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LONGITUDE, -LATITUDE)
#setTrainingINFO <- select(trainingData, FLOOR, SPACEID, RELATIVEPOSITION, USERID, 
#                           PHONEID, TIMESTAMP, LONGITUDE, LATITUDE)


# set cross validation parameters ----


# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                                preProc = c("center", "scale", "range"), 
                                verboseIter = TRUE)
                            



# check the models available in caret package by using names(getModelInfo())
# set the training parameters of the model ----
modelKNN <- train(BUILDINGID~., data = setTraining, 
                  method = "knn", 
                  trControl = CrossValidation)


# check the metrics ----
modelKNN



#see variable importance
varImp(modelKNN)



# make predictions with the model and predict the BUILDING ID of from the trainingData ----
predBUILDING_KNN <- predict(modelKNN, setTest)


#create a new column with predicted data
setTest$predBUILDING_KNN <- predBUILDING_KNN

names(setTest)

# and actual values from that dataset, make sure column of validationData is also factor
setTest$BUILDINGID <- as.factor(setTest$BUILDINGID)
setTest$predBUILDING_KNN <- as.factor(setTest$predBUILDING_KNN)

# check the metrics postResample() for regression and confusionMatrix() for classification ---
confusionMatrix(setTest$predBUILDING_KNN, setTest$BUILDINGID)




# make predictions with the model and predict the BUILDING ID of from the validationData ----
predBUILDING_KNN <- predict(modelKNN, validationData)

plot(predBUILDING_KNN)



#create a new column with predicted data
validationData$predBUILDING_KNN <- predBUILDING_KNN

# and actual values from that dataset, make sure column of validationData is also factor
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
validationData$predBUILDING_KNN <- as.factor(validationData$predBUILDING_KNN)


# check the metrics postResample() for regression and confusionMatrix() for classification ---
confusionMatrix(validationData$predBUILDING_KNN, validationData$BUILDINGID)






# visualize the errors ----
# add column with errors to the dataframe
validationData$predBUILDING_KNN <- as.integer(validationData$predBUILDING_KNN)
validationData$BUILDINGID <- as.integer(validationData$BUILDINGID)
validationData  <- mutate(validationData, errorsBUILDING = predBUILDING_KNN - BUILDINGID) 

# turn the errors back into factors to produce an easy to read plot
validationData$errorsBUILDING <- as.factor(validationData$errorsBUILDING)

plot(validationData$errorsBUILDING,
     main = "BUILDING predictions",
     xlab = "correct = 0 | incorrect != 0",
     ylab = "count")

# find out where the errors occur exactly
plot_ly(validationData, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorsBUILDING == 0, size =1) %>%
  # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
  # add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(title = "Wrongly predicted BUILDINGID's",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'))) 



# subset the errors  
wrongBUILDING <-validationData %>%
  filter(errorsBUILDING != 0)


ggplot(wrongBUILDING, aes(x=LONGITUDE, y=LATITUDE))+
  geom_point() + 
  theme_light() +
  labs(title="Points of measurement of the errors in BUILDINGID",
       subtitle = "Actual BUILDINGID = 0, predicted BUILDINGID =1")

# what do the errors have in common?
wrongBUILDING[,521:531]



#Move the info to the front
wrongBUILDING_gathered <- wrongBUILDING[ , c((ncol(wrongBUILDING)-10):(ncol(wrongBUILDING)), 1:(ncol(wrongBUILDING)-11))]


# gather the data 
wrongBUILDING_gathered <- gather(wrongBUILDING_gathered, 
                                key = WAP, 
                                value = DBM, 
                                12:ncol(wrongBUILDING_gathered))

# Save csv of the final error to check which WAPS are causing the problem
write.csv(wrongBUILDING_gathered, file = "data/errorBUILDINGwaps.csv")




# plot the measurements that cause the errors
wrongBUILDING_gathered <- wrongBUILDING_gathered %>%
  filter(DBM >= -100)
ggplot(wrongBUILDING_gathered) +
  geom_density(aes(x = DBM)) + 
  facet_wrap(~BUILDINGID, scale="free") +
  labs(title="Measurements",
     subtitle = "")

wrongBUILDING$predBUILDING_KNN <- as.factor(wrongBUILDING$predBUILDING_KNN)
ggplot(wrongBUILDING, aes(x=LONGITUDE, y=LATITUDE))+
  geom_point() + 
  theme_light() +
  labs(title="Locations that wrongly predicted BUILDINGID 1",
       subtitle = "and the WAPS connected to these measurements")




# store the predicted values, actual values and errors in a tibble ----
resultsBUILDING <- tibble(.rows = 1111)

resultsBUILDING$predBUILDING_KNN <- predBUILDING_KNN
resultsBUILDING$BUILDINGID <- validationData$BUILDINGID

# make numerical in order to mutate the errors and add them to the tibble
resultsBUILDING$predBUILDING_KNN <- as.integer(resultsBUILDING$predBUILDING_KNN)
resultsBUILDING$BUILDINGID <- as.integer(resultsBUILDING$BUILDINGID)

resultsBUILDING  <- mutate(resultsBUILDING, errorsBUILDING = predBUILDING_KNN - BUILDINGID) 
resultsBUILDING$errorsBUILDING <- resultsBUILDING$predBUILDING_KNN - resultsBUILDING$BUILDINGID



# store as RDS
#saveRDS(resultsBUILDING, file = "resultsBUILDING(V7).rds")

