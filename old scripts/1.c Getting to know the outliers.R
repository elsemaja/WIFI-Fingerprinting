###############################################################################
#                                                                             #
#   WIFI | UNDERSTANDING MEASUREMENTS BETWEEN -30 - 0| VERSION 1.0 | by ELSE #
#                                                                             #
#   connected to results V4, preprocessed: values between -30-0, +100         #
#                                                                             #
###############################################################################

library(ggplot)
library(plotly)

# Load the data
Outliers30 <- readRDS(file = "data/Outliers.rds")


Outliers30$SPACEID <- as.integer(Outliers30$SPACEID)
USERID6 <- Outliers30 %>%
  filter(USERID == 6)
USERID6_2 <- trainingData %>%
  filter(USERID == 6)
Outliers30



# understand the characteristics of the zero variance rows
summary(Outliers30[,521:529])
print(Outliers30[,521:529])

hist(Outliers30$LONGITUDE)
hist(Outliers30$LATITUDE)

Outliers30$FLOOR <- as.integer(Outliers30$FLOOR)
hist(Outliers30$FLOOR)

Outliers30$BUILDINGID <- as.integer(Outliers30$BUILDINGID)
hist(Outliers30$BUILDINGID)
hist(Outliers30$RELATIVEPOSITION)
hist(Outliers30$USERID)
hist(Outliers30$SPACEID)



# Make plots to perform visual analysis
Outliers30$USERID <- as.factor(Outliers30$USERID)
Outliers30$SPACEID <- as.factor(Outliers30$SPACEID)

USERID6 <- trainingData %>%
  filter(USERID == 6)

ggplot(USERID6, aes(x=LONGITUDE, y=LATITUDE, color = USERID))+
  geom_point() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="Measurements above between -30 an 0 dBM",
       subtitle = "Registered by SPACEID and divided by USERID")

Outliers30$RELATIVEPOSITION <- as.factor(Outliers30$RELATIVEPOSITION)
ggplot(Outliers30, aes(x=LONGITUDE, y=LATITUDE, color = RELATIVEPOSITION))+
  geom_point() + 
  theme_light() +
  labs(title="Measurements above -30 dBM",
       subtitle = "Divided by RELATIVE POSITION")

Outliers30$BUILDINGID <- as.factor(Outliers30$BUILDINGID)
ggplot(Outliers30, aes(x=LONGITUDE, y=LATITUDE, color = BUILDINGID))+
  geom_point() + 
  theme_light() +
  labs(title="Measurements above -30 dBM",
       subtitle = "Divided by BUILDING")

Outliers30$FLOOR <- as.factor(Outliers30$FLOOR)
ggplot(Outliers30, aes(x=LONGITUDE, y=LATITUDE, color = FLOOR))+
  geom_point() + 
  theme_light() +
  labs(title="Measurements above -30 dBM",
       subtitle = "Divided by FLOOR")

Outliers30$SPACEID <- as.factor(Outliers30$SPACEID)
ggplot(Outliers30, aes(x=LONGITUDE, y=LATITUDE, color = SPACEID))+
  geom_point() + 
  theme_light() +
  labs(title="Measurements above -30 dBM",
       subtitle = "Divided by SPACEID")


# 3D plot
plot_ly(Outliers30, 
                       x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ffb600','#00ff5d','#0800ff')) %>%
  #add_markers(color = ~errorsBUILDING != 0) %>%
  add_markers(color = ~RELATIVEPOSITION) %>%
  # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
  # add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))
PlotError3D

#create a subset with only floor4 building 2

BUILDING2FLOOR4 <- Outliers30 %>%
  filter(USERID == 6,
         FLOOR == 4, 
         BUILDINGID == 2)

BUILDING2FLOOR4$SPACEID <- as.factor(BUILDING2FLOOR4$SPACEID)
BUILDING2FLOOR4$USERID <- as.factor(BUILDING2FLOOR4$USERID)


print(BUILDING2FLOOR4[,520:529])

plot_ly(BUILDING2FLOOR4, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ffb600','#00ff5d','#0800ff')) %>%
  #add_markers(color = ~errorsBUILDING != 0) %>%
  add_markers(color = ~USERID) %>%
  # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
  # add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'),
                      titel = "test"))

BUILDING2FLOOR4 <- trainingData %>%
  filter(FLOOR == 4 & BUILDINGID == 2)
BUILDING2FLOOR4$SPACEID <- as.factor(BUILDING2FLOOR4$SPACEID)
BUILDING2FLOOR4$USERID <- as.factor(BUILDING2FLOOR4$USERID)

plot_ly(BUILDING2FLOOR4, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ffb600','#00ff5d','#0800ff')) %>%
  #add_markers(color = ~errorsBUILDING != 0) %>%
  add_markers(color = ~USERID) %>%
  # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
  # add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'),
                      titel = "test"))

trainingData2$SPACEID <- as.factor(trainingData2$SPACEID)
trainingData2$USERID <- as.factor(trainingData2$USERID)
plot_ly(trainingData2, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ffb600','#00ff5d','#0800ff')) %>%
  #add_markers(color = ~errorsBUILDING != 0) %>%
  add_markers(color = ~SPACEID) %>%
  # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
  # add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'),
                      titel = "test"))


# the outliers were mainly produced by USERID 6 in BUILDINGID 2 on FLOOR 5
# the measurements made were connected to multiple SPACEID's.
#USERID6 <- trainingData %>% 
#  filter(USERID == 6 & FLOOR ==4)
#USERID5BUILDING2FLOOR3 <- trainingData %>%
#  filter(USERID == 5 & BUILDINGID == 2 & FLOOR == 3)
#USERID1BUILDING0FLOOR2 <- trainingData %>%
#  filter(USERID == 1 & BUILDINGID == 0 & FLOOR == 2)
#USERID11BUILDING0FLOOR2 <- trainingData %>%
#  filter(USERID == 11 & BUILDINGID == 0 & FLOOR == 2)
#PHONEID13 <- trainingData %>%
#  filter(PHONEID == 13)
#PHONEID14 <- trainingData %>%
#  filter(PHONEID == 14)
# PHONEID17 <- trainingData %>%
#  filter(PHONEID == 17, BUILDINGID == 1 & FLOOR == 1)
