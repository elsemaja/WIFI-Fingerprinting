###############################################################################
#                                                                             #
#   WIFI | UNDERSTANDING MEASUREMENTS BETWEEN -30 - 0| VERSION 1.0 | by ELSE #
#                                                                             #
#   connected to results V4, preprocessed: values between -30-0, +100         #
#                                                                             #
###############################################################################

# Load the data
Outliers30 <- readRDS(file = "data/Outliers.rds")

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
ggplot(Outliers30, aes(x=LONGITUDE, y=LATITUDE, color = USERID))+
  geom_point() + 
  theme_light() +
  labs(title="Measurements above -30 dBM",
       subtitle = "Measured by Users")

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
                       x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ffb600','#0800ff')) %>%
  #add_markers(color = ~errorsBUILDING != 0) %>%
  add_markers(color = ~USERID) %>%
  # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
  # add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))
PlotError3D
