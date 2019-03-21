###############################################################################
#                                                                             #
#   WIFI | CREATE TIBBLE/DATAFRAME WITH RESULTS | VERSION 1.0 | by ELSE       #
#                                                                             #
#   combine all the predicted values in one table or dataframe for comparison #
#                                                                             #
###############################################################################


# Load previous scripts
source(file = "1. Loading and getting to know the data.R")
source(file = "2. Prediction FLOOR by KNN (1).R")
source(file = "2. Prediction BuildingID by SVM (1).R")
source(file = "2. Prediction LATITUDE by LM (1).R")
source(file = "2. Prediction LONGITUDE by LINEAR MODEL (1).R")

# combine the predicted results and the corresponding errors in a tibble or datafrme ---
resultsVSactual <- tibble(.rows = 1111)

# add FLOOR and its prediction to the tibble ---
resultsVSactual$PredictedFLOOR_KNN <- PredictedFLOOR_KNN
resultsVSactual$FLOOR <- validationData$FLOOR

# make numerical in order to mutate the errors and add them to the tibble
resultsVSactual$PredictedFLOOR_KNN <- as.integer(resultsVSactual$PredictedFLOOR_KNN)
resultsVSactual$FLOOR <- as.integer(resultsVSactual$FLOOR)
resultsVSactual  <- mutate(resultsVSactual, errorsFLOOR = PredictedFLOOR_KNN - FLOOR) 




# add BUILDING and its prediction to the tibble ----
resultsVSactual$PredictedBUIDLING_SVM <- predictedBUILDING_SVM
resultsVSactual$BUILDINGID <- validationData$BUILDINGID

# make numerical in order to mutate the errors and add them to the tibble
resultsVSactual$PredictedBUIDLING_SVM <- as.integer(resultsVSactual$PredictedBUIDLING_SVM)
resultsVSactual$BUILDINGID <- as.integer(resultsVSactual$BUILDINGID)

resultsVSactual  <- mutate(resultsVSactual, errorsBUILDING = PredictedBUIDLING_SVM - BUILDINGID) 




# add LATITUDE and its prediction to the tibble ----
resultsVSactual$predLATITUDE_LM <- predLATITUDE_LM
resultsVSactual$LATITUDE <- validationData$LATITUDE

# mutate the errors and add them to the tibble
resultsVSactual  <- mutate(resultsVSactual, errorsLATITUDE = predLATITUDE_LM - LATITUDE) 




# add LONGITUDE and its prediction to the tibble ----
resultsVSactual$predLONGITUDE_LM <- predLONGITUDE_LM
resultsVSactual$LONGITUDE <- validationData$LONGITUDE

# mutate the errors and add them to the tibble
resultsVSactual  <- mutate(resultsVSactual, errorsLONGITUDE = predLONGITUDE_LM - LONGITUDE) 
resultsVSactual$errorsLONGITUDE <- resultsVSactual$predLONGITUDE_LM - resultsVSactual$LONGITUDE


# save tibble with all results in a file
saveRDS(resultsVSactual, file = "resultsVSactual(1).rds")




# plot the predictions and errors of LATITUDE
ggplot(resultsVSactual, aes(x = LONGITUDE, y = LATITUDE)) +  # Set up canvas with outcome variable on y-axis
  geom_segment(aes(xend = LONGITUDE, yend = predLATITUDE_LM), alpha = .2) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_point(aes(color = abs(errorsLATITUDE))) + # Color mapped to abs(residuals)
  scale_color_continuous(low = "black", high = "red") +  # Colors to use here
  guides(color = FALSE) +  # Color legend removed
  theme_bw() +
  geom_point() + # Plot the actual points
  geom_point(aes(y = predLATITUDE_LM), shape = 2)  # Add the predicted values
  



#ggplot(d, aes(x = hp, y = mpg)) +
#  geom_point() +
#  geom_point(aes(y = predicted), shape = 1, colour)  # Add the predicted values


# visualize predictions, actuals and errors in a plot
# ggplot(data = resultsVSactual, aes(x = LONGITUDE, y= LATITUDE)) 
   
   
 
 
# ggplot(data = results) +
#   geom_point(aes(x = LATITUDE, y = LONGITUDE), color = "green") +
#   geom_point(aes(x = LATITUDE, y = predLONGITUDE_LM), color = "blue") 
 
 
 # visualize them in a plot, try a 3D plot!
 ggplot(data = resultsVSactual) +
   geom_point(aes(x = LONGITUDE, y = LATITUDE, z = FLOOR, color = BUILDINGID))
 
 
 #
 #ggplot(data = resultsFLOOR) +
 #  geom_point(aes(y = PredictedFLOOR_KNN), color = "blue") +
 #  geom_point(aes(y = FLOOR), color = "green") +
 #  geom_point(aes(y = errors), color = "red")
 
 #ggplot(trainingData, aes(x=LONGITUDE, y=FLOOR, z=LATITUDE, color=BUILDINGID)) + 
 #  theme_void() +
 #  axes_3D() +
 #  stat_3D()
 
 #library("plotly")
 #plot_ly(trainingData, x=LONGITUDE, y=FLOOR, z=LATITUDE, type="scatter3d", mode="markers", color=BUILDINGID)  
 
 
 
 
 #make scatterpot to look at the errors of prediction on test data
 #testing$errors <- testing$brand - testing$Prediction
 #ggplot(testing) + geom_point(aes(x = age, 
 #                                 y = salary, 
 #                                 color = (errors != 0)))
 
 
 