###############################################################################
#                                                                             #
#   WIFI | VISUALIZING TRAININGDATA | VERSION 5.0 | by ELSE                   #
#                                                                             #
#                                                                             #
#                                                                             #
###############################################################################
# show differences in measurements between datasets


#libraries
library("dplyr")
library("ggplot")
library("tidyverse")
library("plotly")


# load the data
trainingData <- readRDS(file = "data/trainingDataProc(V7).rds")
validationData <- readRDS(file = "data/validationDataProc(V7).rds")





#Move the info to the front
trainingData_Gathered <- trainingData[ , c((ncol(trainingData)-8):(ncol(trainingData)), 1:(ncol(trainingData)-11))]


# gather the data 
trainingData_Gathered <- gather(trainingData_Gathered, WAP, DBM, 10:ncol(trainingData_Gathered))


PHONEID13 <- trainingData_Gathered %>%
  filter(PHONEID == 13)



# visualizing the data in general 
trainingData$PHONEID <- as.factor(trainingData$PHONEID)
ggplot(trainingData, aes(x=LONGITUDE, y = LATITUDE, color = PHONEID)) +
  geom_jitter() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="WIFI fingerprints collected in training data",
       subtitle = "seperated by FLOOR & PHONEID")

validationData$PHONEID <- as.factor(validationData$PHONEID)
ggplot(validationData, aes(x=LONGITUDE, y = LATITUDE, color = PHONEID)) +
  geom_jitter() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="WIFI fingerprints collected in validation data",
       subtitle = "seperated by FLOOR & PHONEID")





trainingData$USERID <- as.factor(trainingData$USERID)
trainingData$PHONEID <- as.factor(trainingData$PHONEID)
ggplot(trainingData, aes(x=LONGITUDE, y = LATITUDE, color = USERID)) +
  geom_jitter() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="WIFI fingerprints collected",
       subtitle = "seperated by USERID")




trainingData_Gathered <- trainingData_Gathered %>%
  filter(BUILDINGID == 2,
         FLOOR == 3 & FLOOR == 4,
         DBM > -30 & DBM <= 0)

ggplot(trainingData_Gathered) +
  geom_density(aes(x = DBM)) +
  labs(title="RSSI between -30 and 0 dBm",
       subtitle= "BUILDINGID 2, FLOOR 0, measured by USERID 10, PHONEID 8 ")




#plot the distribution of WPA values between -30 and 0 ----
# select the values of DBM above -30
outliers30_2 <- trainingData_Gathered %>% filter(trainingData_Gathered$DBM > -30)
outliers30_2 <- trainingData_Gathered %>% filter(DBM <= 0 & DBM >= -105)

ggplot(trainingData_Gathered) +
  geom_density(aes(x = DBM)) +
  labs(title="Measurements",
       subtitle= "What is going on at +100?")
  


ggplot(outliers30_2) +
  geom_density(aes(x = DBM)) +
  facet_wrap(~FLOOR, scale="free")

# plot the distribution of WPA values between -90 and -30 ----
# select the values of DBM 
WAPSrange <- trainingData_Gathered %>% 
  filter(DBM <= -30 &  DBM >= -90)

ggplot(WAPSrange) +
  geom_density(aes(x = DBM)) + 
  facet_wrap(~BUILDINGID, scale="free")


ggplot(WAPSrange) +
  geom_density(aes(x = DBM)) +
  facet_wrap(~FLOOR, scale="free")

# plot the useless WAPS ----
WAPSuseless <-  trainingData_Gathered %>% 
  filter(DBM <= -90 & DBM >= -105)

ggplot(WAPSuseless) +
  geom_density(aes(x = DBM)) + 
  facet_wrap(~BUILDINGID, scale="free")

ggplot(WAPSuseless) +
  geom_density(aes(x = DBM)) +
  facet_wrap(~FLOOR, scale="free")

# plot the general range ----
WAPS <-  trainingData_Gathered %>% 
  filter(DBM <= -30 & DBM >= -100)

ggplot(WAPS) +
  geom_density(aes(x = DBM)) + 
  facet_wrap(~BUILDINGID, scale="free")

ggplot(WAPS) +
  geom_density(aes(x = DBM)) +
  facet_wrap(~FLOOR, scale="free")

WAPSFLOOR4 <- trainingData_Gathered %>% 
  filter(DBM <= -30 & DBM >= -100,
         FLOOR == 4) 

ggplot(WAPSFLOOR4) +
  geom_density(aes(x = DBM)) +
  facet_wrap(~USERID, scale="free") +
  labs(title="Measurements made at FLOOR 4, BUILDING 2",
       subtitle = "by USERID 3 and 13")

# subset the values that generated errors on building 2, floor 0 and 1 ----
FLOOR0B1 <-  trainingData_Gathered %>% 
  filter(BUILDINGID == 1,
         USERID == 13,
         FLOOR == 0,
         DBM >= -105)

       #  LATITUDE >= 486489 & LATITUDE <= 486484,
        # LONGITUDE >= -7516 & LATITUDE <= -7470)


# visualize the users in this B 1 FLOOR 0 ----
ggplot(FLOOR0B1) +
  geom_density(aes(x = DBM)) +
  facet_wrap(~USERID, scale="free") +
  labs(title="Measurements made by USERID 13 at FLOOR 0, BUILDING 1",
       subtitle = "by USERID")

FLOOR0B1$USERID <- as.factor(FLOOR0B1$USERID)
ggplot(FLOOR0B1, aes(x=LONGITUDE, y=LATITUDE, color = USERID)) +
  geom_point()+
  facet_wrap(~USERID) +
  labs(title="Measurements made at FLOOR 0, BUILDING 1",
     subtitle = "by USERID")

# visualize the measurements made by these users
FLOOR0B1$USERID <- as.factor(FLOOR0B1$USERID)
ggplot(FLOOR0B1)+
  geom_density(aes(x = DBM)) +
  facet_wrap(~USERID) +
  theme_light() +
  labs(title="Measurements made at FLOOR 0, BUILDING 1",
       subtitle = "by USERID")


# subset the values that generated errors on building 2, floor 0 and 1 ----
FLOOR1B1 <-  trainingData_Gathered %>% 
  filter(BUILDINGID == 1,
         USERID == 13,
         FLOOR == 1,
         DBM >= -105)

# visualize the users in this B 1 FLOOR 1 ----
ggplot(FLOOR1B1) +
  geom_density(aes(x = DBM)) +
  facet_wrap(~USERID, scale="free") +
  labs(title="Measurements made at FLOOR 1, BUILDING 1",
       subtitle = "by USERID")


# visualize the users in this B 1 FLOOR 1
ggplot(FLOOR1B1) +
  geom_density(aes(x = DBM)) +
  facet_wrap(~PHONEID, scale="free") +
  labs(title="Measurements made at FLOOR 1, BUILDING 1",
       subtitle = "by PHONEID")

# visualize the users in this B 1 FLOOR 1
ggplot(FLOOR1B1) +
  geom_density(aes(x = DBM)) +
  facet_wrap(~PHONEID, scale="free") +
  labs(title="Measurements made at FLOOR 1, BUILDING 1",
       subtitle = "by PHONEID")

# 3D plots
trainingData$USERID <- as.factor(trainingData$USERID)
plot_ly(trainingData, 
                       x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff7f00','#00ffe9', '#ff00a5')) %>%
  #add_markers(color = ~errorsBUILDING != 0) %>%
  add_markers(color = ~USERID, size = 1) %>%
  # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
  # add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))

trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)
trainingData$PHONEID <- as.factor(trainingData$PHONEID)
plot_ly(trainingData, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ffb200','#55d6cd', '#ff93b9')) %>%
  #add_markers(color = ~errorsBUILDING != 0) %>%
  add_markers(color = ~PHONEID, size = 1) %>%
  # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
  # add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))

validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
validationData$PHONEID <- as.factor(validationData$PHONEID)
plot_ly(validationData, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#ff9400', '#fffa00','#009e0d', '#00fff2', '#003bff')) %>%
  #add_markers(color = ~errorsBUILDING != 0) %>%
  add_markers(color = ~PHONEID, size = 1) %>%
  # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
  # add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(scene = list(xaxis = list(title = ''),
                      yaxis = list(title = ''),
                      zaxis = list(title = '')))

  

