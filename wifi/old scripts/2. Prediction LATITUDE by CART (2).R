###############################################################################
#                                                                             #
#   WIFI | TRAINING CART FOR LATITUDE | VERSION 1.0 | by ELSE                 #
#                                                                             #
#   Sample & subset data with only WAPS as predictors, train model & predict  #
#                                                                             #
###############################################################################

# take a representable sample from the training dataset in order to train a model
# taking a sample will save time while running the first model(s)
# use only WAP columns to predict: LATITUDE

# libraries & data----
library("rpart")
library("dplyr")
library("class")
library("readr")
library("corrplot")


# load training data file
trainingData <- read.csv("data/trainingData.csv")
validationData <- read.csv("data/validationData.csv")





# select dependent variable and sample your dataset ----
# make sure LATITUDE is numerical to run a regression
trainingData$LATITUDE <- as.numeric(trainingData$LATITUDE)



# remove other columns in order to predict LATITUDE by the values of WAPs
trainingDataWAP <- select(trainingData, -FLOOR, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LONGITUDE)



# partitioning data
set.seed(123)
indexTrain <- createDataPartition(y = trainingDataWAP$LATITUDE, p = .25, list = FALSE)
setTraining <- trainingDataWAP[indexTrain,]





# set cross validation parameters ----
# default search = random, change it to grid search if searching with Manual Grid
CrossValidation <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                                preProc = c("center", "scale"), verboseIter = TRUE)



# set the training parameters of the model ----
modelTREE <- rpart(LATITUDE~., data = setTraining, method= "anova")

# check the metrics ----
modelTREE



# examine the results ----
printcp(modelTREE) #	display cp table
plotcp(modelTREE)	# plot cross-validation results
rsq.rpart(modelTREE) #	plot approximate R-squared and relative error for different splits (2 plots). labels are only appropriate for the "anova" method.
print(modelTREE) #	print results
summary(modelTREE) #	detailed results including surrogate splits
plot(modelTREE)	# plot decision tree
text(modelTREE) #	label the decision tree plot




#see variable importance
varImp(modelTREE)


# make predictions with the model and predict the LATITUDE of from the validationData ----
predLATITUDE_TREE <- predict(modelTREE, validationData)



# check the metrics postResample() for regression and confusionMatrix() for classification ----
postResample(validationData$predLATITUDE_TREE, validationData$LATITUDE)


#create a new column with predicted data
validationData$predLATITUDE_TREE <- predLATITUDE_TREE


# and actual values from that dataset, make sure column of validationData is also factor
validationData$LATITUDE <- as.numeric(validationData$LATITUDE)
validationData$predLATITUDE_TREE <- as.numeric(validationData$predLATITUDE_TREE)
