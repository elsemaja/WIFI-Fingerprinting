###############################################################################
#                                                                             #
#   WIFI | UNDERSTANDING ROWS WITH ZERO VARIANCE | VERSION 1.0 | by ELSE      #
#                                                                             #
#   connected to results V4, preprocessed: values between -30-0, +100         #
#                                                                             #
###############################################################################


# Load the data
ZeroVarRows <- readRDS(file = "data/ZeroVarRows.rds")

# understand the characteristics of the zero variance rows
summary(ZeroVarRows[,521:529])
print(ZeroVarRows[,521:529])

hist(ZeroVarRows$LONGITUDE)
hist(ZeroVarRows$LATITUDE)

ZeroVarRows$FLOOR <- as.integer(ZeroVarRows$FLOOR)
hist(ZeroVarRows$FLOOR)

ZeroVarRows$BUILDINGID <- as.integer(ZeroVarRows$BUILDINGID)
hist(ZeroVarRows$BUILDINGID)
hist(ZeroVarRows$RELATIVEPOSITION)
hist(ZeroVarRows$USERID)
hist(ZeroVarRows$SPACEID)


# Make plots to perform visual analysis
ZeroVarRows$USERID <- as.factor(ZeroVarRows$USERID)
ggplot(ZeroVarRows, aes(x=LONGITUDE, y=LATITUDE, color = USERID))+
  geom_point() + 
  theme_light() +
  labs(title="Zero Variance Measurements",
       subtitle = "Measured by Users")

ZeroVarRows$RELATIVEPOSITION <- as.factor(ZeroVarRows$RELATIVEPOSITION)
ggplot(ZeroVarRows, aes(x=LONGITUDE, y=LATITUDE, color = RELATIVEPOSITION))+
  geom_point() + 
  theme_light() +
  labs(title="Zero Variance Measurements",
       subtitle = "Divided by RELATIVE POSITION")

ZeroVarRows$BUILDINGID <- as.factor(ZeroVarRows$BUILDINGID)
ggplot(ZeroVarRows, aes(x=LONGITUDE, y=LATITUDE, color = BUILDINGID))+
  geom_point() + 
  theme_light() +
  labs(title="Zero Variance Measurements",
       subtitle = "Divided by BUILDING")

ZeroVarRows$FLOOR <- as.factor(ZeroVarRows$FLOOR)
ggplot(ZeroVarRows, aes(x=LONGITUDE, y=LATITUDE, color = FLOOR))+
  geom_point() + 
  theme_light() +
  labs(title="Zero Variance Measurements",
       subtitle = "Divided by FLOOR")

ZeroVarRows$SPACEID <- as.factor(ZeroVarRows$SPACEID)
ggplot(ZeroVarRows, aes(x=LONGITUDE, y=LATITUDE, color = SPACEID))+
  geom_point() + 
  theme_light() +
  labs(title="Zero Variance Measurements",
       subtitle = "Divided by SPACEID")

