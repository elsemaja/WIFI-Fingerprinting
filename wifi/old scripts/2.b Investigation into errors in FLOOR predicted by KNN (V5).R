
###############################################################################
#                                                                             #
#   WIFI | UNDERSTANDING ERRORS FLOORS | VERSION 1.0 | by ELSE                #
#                                                                             #
#   connected to results V4, preprocessed: values between -30-0, +100         #
#                                                                             #
###############################################################################


# Load the data
wrongFLOOR <- readRDS(file = "data/errorsFLOOR-training.rds")

# understand the characteristics of the zero variance rows
summary(wrongFLOOR[,465:475])
wrongFLOOR[,465:475]

plot(wrongFLOOR$LONGITUDE, wrongFLOOR$LATITUDE)


wrongFLOOR$FLOOR <- as.integer(wrongFLOOR$FLOOR)
hist(wrongFLOOR$FLOOR)

wrongFLOOR$BUILDINGID <- as.integer(wrongFLOOR$BUILDINGID)
wrongFLOOR$USERID <- as.integer(wrongFLOOR$USERID)
hist(wrongFLOOR$BUILDINGID)
hist(wrongFLOOR$RELATIVEPOSITION)
plot(wrongFLOOR$USERID)
hist(wrongFLOOR$SPACEID)
summary(wrongFLOOR$USERID)



# find out where the errors occur exactly
plot_ly(wrongFLOOR, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorsFLOOR == 0) %>%
  # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
  # add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(title = "Wrongly predicted FLOORS",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'))) 

# find out where the errors occur exactly
plot_ly(wrongFLOOR, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR) %>%
  add_markers(color = ~PHONEID) %>%
  # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
  # add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(title = "Wrongly predicted FLOORS's",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'))) 

# Make plots to perform visual analysis
wrongFLOOR$PHONEID <- as.factor(wrongFLOOR$PHONEID)
ggplot(wrongFLOOR, aes(x=LONGITUDE, y=LATITUDE, color = PHONEID))+
  geom_point() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="Wrongly predicted FLOOR",
       subtitle = "divided by PHONEID")

Building1Floors <- wrongFLOOR %>%
  filter(BUILDINGID == 1)


setTest$predFLOOR_KNN <- as.factor(setTest$predFLOOR_KNN)
ggplot(setTest, aes(x=LONGITUDE, y=LATITUDE, color = predFLOOR_KNN))+
  geom_point() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="Wrongly predicted FLOORS",
       subtitle = "")



wrongFLOOR$RELATIVEPOSITION <- as.factor(wrongFLOOR$RELATIVEPOSITION)
ggplot(wrongFLOOR, aes(x=LONGITUDE, y=LATITUDE, color = RELATIVEPOSITION))+
  geom_point() + 
  theme_light() +
  labs(title="Wrongly predicted FLOOR",
       subtitle = "Divided by RELATIVE POSITION")

wrongFLOOR$BUILDINGID <- as.factor(wrongFLOOR$BUILDINGID)
ggplot(wrongFLOOR, aes(x=LONGITUDE, y=LATITUDE, color = BUILDINGID))+
  geom_point() + 
  theme_light() +
  labs(title="Wrongly predicted FLOOR",
       subtitle = "Divided by BUILDING")

wrongFLOOR$FLOOR <- as.factor(wrongFLOOR$FLOOR)
ggplot(wrongFLOOR, aes(x=LONGITUDE, y=LATITUDE, color = FLOOR))+
  geom_point() + 
  theme_light() +
  labs(title="Wrongly predicted FLOOR",
       subtitle = "Divided by FLOOR")

wrongFLOOR$SPACEID <- as.factor(wrongFLOOR$SPACEID)
ggplot(wrongFLOOR, aes(x=LONGITUDE, y=LATITUDE, color = SPACEID))+
  geom_point() + 
  theme_light() +
  labs(title="Wrongly predicted FLOOR",
       subtitle = "Divided by SPACEID")





