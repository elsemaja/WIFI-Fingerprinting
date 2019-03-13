###############################################################################
#                                                                             #
#   WIFI | VISUALISING THE RESULTS WITH PLOTS | VERSION 1.0 | by ELSE         #
#                                                                             #
#   combine all the predicted values in one table or dataframe for comparison #
#                                                                             #
###############################################################################


# install.packages('devtools')
devtools::install_github('bbc/bbplot', force = TRUE)

#This line of code installs the pacman page if you do not have it installed - if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")

pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot', 'devtools', 'plotly')


# load the predictions, actuals and error file
resultsVSactual <- readRDS(file = "resultsVSactual(1).rds")


# change BUILDINGID, FLOOR and their predicted values back into factors
resultsVSactual$BUILDINGID <- as.factor(resultsVSactual$BUILDINGID)
resultsVSactual$FLOOR <- as.factor(resultsVSactual$FLOOR)
resultsVSactual$PredictedBUIDLING_SVM <- as.factor(resultsVSactual$PredictedBUIDLING_SVM)
resultsVSactual$PredictedFLOOR_KNN <- as.factor(resultsVSactual$PredictedFLOOR_KNN)
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
PlotPredBUILDING <- ggplot(resultsVSactual, aes(x = predLONGITUDE_LM, 
                                               y = predLATITUDE_LM,
                                               colour = PredictedBUIDLING_SVM))+
  geom_jitter()+
  theme_classic() +
  labs(title="Predicted Locations",
       subtitle = "By buidling")
PlotPredBUILDING

# make plot of the errors and group by BUILDING
PlotErrorBUILDING <- ggplot(resultsVSactual, aes(x = errorsLONGITUDE, 
                                                y = errorsLATITUDE,
                                                colour = (errorsBUILDING != 0)))+
  geom_jitter()+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "By buidling")
PlotErrorBUILDING

# plots grouped by FLOOR ----
#Make plot of actual values and group by FLOOR
PlotFLOOR <- ggplot(resultsVSactual, aes(x = LONGITUDE, y = LATITUDE, colour = FLOOR)) +
  geom_jitter()+
  theme_classic() +
  labs(title="Actual Locations",
       subtitle = "By FLOOR")
PlotFLOOR


# make plot of predicted values and group by PredictedFLOOR
PlotPredFLOOR <- ggplot(resultsVSactual, aes(x = predLONGITUDE_LM, 
                                                y = predLATITUDE_LM, 
                                                colour = PredictedFLOOR_KNN))+
  geom_jitter()+
  theme_classic() +
  labs(title="Predicted Locations",
        subtitle = "By FLOOR")
PlotPredFLOOR


# make plot of the errors and group by FLOOR
PlotErrorFLOOR <- ggplot(resultsVSactual, aes(x = errorsLONGITUDE, 
                                                 y = errorsLATITUDE,
                                                 colour = (errorsFLOOR != 0)))+
  geom_jitter()+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "By buidling")
PlotErrorFLOOR


# plot the errors ----
# make plot of the errors of more dan 8 meters with latitude:
PlotErrorLATITUDE <- ggplot(resultsVSactual, aes(x = errorsLONGITUDE, 
                                              y = errorsLATITUDE,
                                              colour = (errorsLATITUDE > 8)))+
  geom_jitter()+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "More than 8 meters in LATITUDE")
PlotErrorLATITUDE

# make plot of the errors of more dan 8 meters with longitude:
PlotErrorLONGITUDE <- ggplot(resultsVSactual, aes(x = predLONGITUDE_LM, 
                                                 y = predLATITUDE_LM,
                                                 colour = (errorsLONGITUDE > 8 | errorsLONGITUDE < -8 )))+
  geom_jitter() +
  geom_jitter(aes(x=LONGITUDE, y=LATITUDE))+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "More than 8 meters in LONGITUDE")
PlotErrorLONGITUDE

# plot the errors of LONGITUDE that is more than 8 meters
PlotErrorLONGITUDE <- ggplot(resultsVSactual, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter() +
  geom_jitter(aes(x = predLONGITUDE_LM, 
                  y = predLATITUDE_LM,
                  colour = (errorsLONGITUDE > 8 | errorsLONGITUDE < -8 )))+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "More than 8 meters in LONGITUDE")

# plot the errors of LATITUDE that is more than 8 meters
PlotErrorLATITUDE <- ggplot(resultsVSactual, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter() +
  geom_jitter(aes(x = predLONGITUDE_LM, 
                  y = predLATITUDE_LM,
                  colour = (errorsLATITUDE > 8 | errorsLATITUDE < -8 )))+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "More than 8 meters in LONGITUDE")

# combine the layers for the errors in LONGITUDE and LATITUDE ----
PlotErrorLATITUDE <- ggplot(resultsVSactual, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter() +
  geom_jitter(aes(x = predLONGITUDE_LM, 
                  y = predLATITUDE_LM,
                  colour = (errorsLATITUDE > 8 | errorsLATITUDE < -8 )))+
  geom_jitter(aes(x = predLONGITUDE_LM, 
                  y = predLATITUDE_LM,
                  colour = (errorsLONGITUDE > 8 | errorsLONGITUDE < -8 )))+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "More than 8 meters in LONGITUDE")


# 3D plots ----
# Make a 3D plot with plotly for the actual values
Plot3D <- plot_ly(resultsVSactual, 
             x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, color = ~BUILDINGID, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))


#Make a 3D plot with plotly for the predicted values
PlotPred3D <- plot_ly(resultsVSactual, 
                  x = ~predLATITUDE_LM, y = ~predLONGITUDE_LM, z = ~PredictedFLOOR_KNN, color = ~PredictedBUIDLING_SVM, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))
PlotPred3D


#Make a 3D plot with plotly for the errors 
PlotError3D <- plot_ly(resultsVSactual, 
                      x = ~errorsLATITUDE, y = ~errorsLONGITUDE, z = ~errorsFLOOR, color = ~errorsBUILDING, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))
PlotError3D




