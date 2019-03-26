###############################################################################
#                                                                             #
#   WIFI | CREATE TIBBLE/DATAFRAME WITH RESULTS | FINAL VERSION | by ELSE     #
#                                                                             #
#   combine all the predicted values in one table or dataframe for comparison #
#                                                                             #
###############################################################################

# This line of code installs the pacman page if you do not have it installed - 
# if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")

pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot', 'devtools', 'plotly')

# Load the resultsVSactuals
resultsFLOOR <- readRDS(file = "data/resultsFLOOR(V7).rds")
resultsBUILDING <- readRDS(file = "data/resultsBUILDING(V7).rds")
resultsLATITUDE <- readRDS(file = "data/resultsLATITUDE(V7).rds")
resultsLONGITUDE <- readRDS(file = "data/resultsLONGITUDE(V7).rds")

# combine the predicted results and the corresponding errors in a tibble or datafrme ---
resultsALL <- data.frame(resultsFLOOR,resultsBUILDING,resultsLATITUDE,resultsLONGITUDE)


# change BUILDINGID, FLOOR and their predicted values back into factors
resultsVSactual$BUILDINGID <- as.factor(resultsVSactual$BUILDINGID)
resultsVSactual$FLOOR <- as.factor(resultsVSactual$FLOOR)
resultsVSactual$predBUILDING_SVM <- as.factor(resultsVSactual$predBUILDING_SVM)
resultsVSactual$predFLOOR_SVM <- as.factor(resultsVSactual$predFLOOR_SVM)
resultsVSactual$errorsBUILDING <- as.factor(resultsVSactual$errorsBUILDING)
resultsVSactual$errorsFLOOR <- as.factor(resultsVSactual$errorsFLOOR)

# plots grouped by building ----
#Make plot of actual values and group by BUILDING
ggplot(resultsVSactual, aes(x = LONGITUDE, y = LATITUDE, colour = BUILDINGID)) +
  geom_jitter()+
  theme_light() +
  labs(title="Actual locations",
       subtitle = "validation") +
  labs(x = "LONGITUDE", y ="LATITUDE")


# make plot of predicted values and group by BUILDING
ggplot(resultsVSactual, aes(x = predLONGITUDE_KNN, y = predLATITUDE_KNN, 
                            colour = predBUILDING_SVM))+
  geom_jitter()+
  theme_light() +
  labs(title="Predicted locations",
       subtitle = "validation")+
  labs(x = "LONGITUDE", y ="LATITUDE")


ggplot(resultsVSactual, aes(x = LONGITUDE, y = LATITUDE, colour = predBUILDING_SVM))+
  geom_jitter()+
  theme_light() +
  labs(title="Predicted locations",
       subtitle = "validation")+
  labs(x = "LONGITUDE", y ="LATITUDE")


# make plot of the errors and group by BUILDING (red is good prediction)
ggplot(resultsVSactual, aes(x = LONGITUDE, y = LATITUDE, colour = (errorsBUILDING != 0)))+
  geom_jitter(size=3)+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "By building")

# plots grouped by FLOOR ----
# make plot of the errors and group by FLOOR

ggplot(resultsVSactual, aes(x = LONGITUDE, y = LATITUDE, colour = (errorsFLOOR != 0)))+
  geom_jitter()+
  theme_light() +
  facet_wrap(~FLOOR)+
  labs(title="Error Locations",
       subtitle = "wrongly predicted FLOOR")


ggplot(wrongFLOORValB1, aes(x = LONGITUDE, y = LATITUDE, colour = (errorsFLOOR != 0)))+
  geom_jitter()+
  theme_light() +
  facet_wrap(~FLOOR)+
  labs(title="Error Locations",
       subtitle = "wrongly predicted FLOOR")


# plot the errors ----
# make plot of the errors of more dan 8 meters with latitude:
ggplot(resultsVSactual, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter()+
  geom_jitter(aes(x = predLONGITUDE_KNN, 
                  y = predLATITUDE_KNN,
                  colour = (errorsLATITUDE >= 8 | errorsLATITUDE <= -8)))+
  theme_light() +
  labs(title="Error Locations",
       subtitle = "More than 8 meters in LATITUDE")



# plot the errors of LONGITUDE that is more than 8 meters
ggplot(resultsVSactual, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter() +
  geom_jitter(aes(x = predLONGITUDE_KNN, 
                  y = predLATITUDE_KNN,
                  colour = (errorsLONGITUDE >= 8 | errorsLONGITUDE <= -8 )))+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "More than 8 meters in LONGITUDE")


# plot the errors of LATITUDE that is more than 8 meters
ggplot(resultsVSactual, aes(x=LONGITUDE, y=LATITUDE), colour = "black")+
  geom_jitter() +
  geom_jitter(aes(x = predLONGITUDE_KNN, 
                  y = predLATITUDE_KNN,
                  colour = (errorsLATITUDE <= -8 | errorsLATITUDE >= 8 )))+
  theme_classic() +
  labs(title="Error Locations",
       subtitle = "More than 8 meters in LONGITUDE")




# 3D plots ----
# Make a 3D plot with plotly for the actual values
plot_ly(resultsVSactual, x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, 
        color = ~BUILDINGID, colors = c('#ffb600','#0800ff')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))


#Make a 3D plot with plotly for the predicted values
plot_ly(resultsVSactual, x = ~predLATITUDE_KNN, y = ~predLONGITUDE_KNN, z = ~predFLOOR_SVM, 
        color = ~predBUILDING_SVM, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))

# turn errors in numerical for the plot?
resultsVSactual$errorsLATITUDE <- as.numeric(resultsVSactual$errorsLATITUDE)
resultsVSactual$errorsLONGITUDE <- as.numeric(resultsVSactual$errorsLONGITUDE)

#Make a 3D plot with plotly for the errors and find out how to show the errors
plot_ly(resultsVSactual, x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, 
        colors = c('#ffb600','#0800ff', '#009e0d')) %>%
  add_markers(color = ~BUILDINGID, size = 1) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))

plot_ly(resultsVSactual, 
        x = ~predLATITUDE_KNN, y = ~predLONGITUDE_KNN, z = ~predFLOOR_SVM, 
        colors = c('#ffb200','#55d6cd', '#ff93b9')) %>%
  add_markers(color = ~predBUILDING_SVM, size = 1) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))




