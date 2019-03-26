###############################################################################
#                                                                             #
#   WIFI | VISUALISING THE RESULTS WITH PLOTS | VERSION 5.0 | by ELSE         #
#                                                                             #
#   combine all the predicted values in one table or dataframe for comparison #
#                                                                             #
###############################################################################


#This line of code installs the pacman page if you do not have it installed - if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")

pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot', 'devtools', 'plotly')


# load the predictions, actuals and error file
resultsVSactual <- readRDS(file = "resultsALL(5).rds")



# change BUILDINGID, FLOOR and their predicted values back into factors
resultsVSactual$BUILDINGID <- as.factor(resultsVSactual$BUILDINGID)
resultsVSactual$FLOOR <- as.factor(resultsVSactual$FLOOR)
resultsVSactual$predBUILDING_SVM <- as.factor(resultsVSactual$predBUILDING_SVM)
resultsVSactual$predFLOOR_KNN <- as.factor(resultsVSactual$predFLOOR_KNN)
resultsVSactual$errorsBUILDING <- as.factor(resultsVSactual$errorsBUILDING)
resultsVSactual$errorsFLOOR <- as.factor(resultsVSactual$errorsFLOOR)




# plots grouped by building ----
#Make plot of actual values and group by BUILDING
PlotBUILDING <- ggplot(resultsVSactual, aes(x = LONGITUDE, y = LATITUDE, colour = BUILDINGID)) +
  geom_jitter()+
  theme_classic() +
  labs(title="Actual Locations",
       subtitle = "By buidling")
PlotBUILDING


# make plot of predicted values and group by BUILDING
PlotPredBUILDING <- ggplot(resultsVSactual, aes(x = predLONGITUDE_KNN, 
                                               y = predLATITUDE_KNN,
                                               colour = predBUILDING_SVM))+
  geom_jitter()+
  theme_classic() +
  labs(title="Predicted Locations",
       subtitle = "By buidling")
PlotPredBUILDING

# make plot of the errors and group by BUILDING (red is good prediction)
PlotErrorBUILDING <- ggplot(resultsVSactual, aes(x = LONGITUDE, 
                                                y = LATITUDE,
                                                colour = (errorsBUILDING != 0)))+
  geom_jitter(size=3)+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "By building")
PlotErrorBUILDING

# plots grouped by FLOOR ----
# make plot of the errors and group by FLOOR
PlotErrorFLOOR <- ggplot(resultsVSactual, aes(x = LONGITUDE, 
                                                 y = LATITUDE,
                                                 colour = (errorsFLOOR != 0)))+
  geom_jitter()+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "By building")
PlotErrorFLOOR


# plot the errors ----
# make plot of the errors of more dan 8 meters with latitude:
PlotErrorLATITUDE <- ggplot(resultsVSactual, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter()+
  geom_jitter(aes(x = predLONGITUDE_KNN, 
                  y = predLATITUDE_KNN,
                  colour = (errorsLATITUDE > 8 | errorsLATITUDE < -8)))+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "More than 8 meters in LATITUDE")
PlotErrorLATITUDE



# plot the errors of LONGITUDE that is more than 8 meters
PlotErrorLONGITUDE <- ggplot(resultsVSactual, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter() +
  geom_jitter(aes(x = predLONGITUDE_KNN, 
                  y = predLATITUDE_KNN,
                  colour = (errorsLONGITUDE > 8 | errorsLONGITUDE < -8 )))+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "More than 8 meters in LONGITUDE")
PlotErrorLONGITUDE

# plot the errors of LATITUDE that is more than 8 meters
PlotErrorLATITUDE <- ggplot(resultsVSactual, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter() +
  geom_jitter(aes(x = predLONGITUDE_KNN, 
                  y = predLATITUDE_KNN,
                  colour = (errorsLATITUDE > 8 | errorsLATITUDE < -8 )))+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "More than 8 meters in LONGITUDE")

# combine the layers for the errors in LONGITUDE and LATITUDE ----
PlotErrorLATITUDE <- ggplot(resultsVSactual, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter() +
  geom_jitter(aes(x = predLONGITUDE_KNN, 
                  y = predLATITUDE_KNN,
                  colour = (errorsLATITUDE > 8 | errorsLATITUDE < -8 )))+
  geom_jitter(aes(x = predLONGITUDE_KNN, 
                  y = predLATITUDE_KNN,
                  colour = (errorsLONGITUDE > 8 | errorsLONGITUDE < -8 )))+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "More than 8 meters in LONGITUDE")


# 3D plots ----
# Make a 3D plot with plotly for the actual values
Plot3D <- plot_ly(resultsVSactual, 
             x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, color = ~BUILDINGID, colors = c('#ffb600','#0800ff')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))
Plot3D

#Make a 3D plot with plotly for the predicted values
PlotPred3D <- plot_ly(resultsVSactual, 
                  x = ~predLATITUDE_KNN, 
                  y = ~predLONGITUDE_KNN, 
                  z = ~predFLOOR_KNN, 
                  color = ~predBUILDING_SVM, 
                  colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))
PlotPred3D


# turn errors in numerical for the plot?
resultsVSactual$errorsLATITUDE <- as.numeric(resultsVSactual$errorsLATITUDE)
resultsVSactual$errorsLONGITUDE <- as.numeric(resultsVSactual$errorsLONGITUDE)

#Make a 3D plot with plotly for the errors and find out how to show the errors
PlotError3D <- plot_ly(resultsVSactual, 
                      x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ffb600','#0800ff')) %>%
  #add_markers(color = ~errorsBUILDING != 0) %>%
  add_markers(color = ~errorsFLOOR != 0 ) %>%
 # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
# add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))
PlotError3D




