###############################################################################
#                                                                             #
#   WIFI | VISUALIZING TRAININGDATA | VERSION 5.0 | by ELSE                   #
#                                                                             #
#                                                                             #
#                                                                             #
###############################################################################

# visualizing the data in general
trainingData$PHONEID <- as.factor(trainingData$PHONEID)
ggplot(trainingData, aes(x=LONGITUDE, y = LATITUDE, color = PHONEID)) +
  geom_jitter() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="Measurements made",
       subtitle = "seperated by people")


trainingDataProc$USERID <- as.factor(trainingDataProc$USERID)
trainingDataProc$PHONEID <- as.factor(trainingDataProc$PHONEID)
ggplot(trainingDataProc, aes(x=LONGITUDE, y = LATITUDE, color = USERID)) +
  geom_jitter() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="Measurements made",
       subtitle = "seperated by people")



