###############################################################################
#                                                                             #
#   WIFI | VISUALIZING TRAININGDATA | VERSION 4.0 | by ELSE                   #
#                                                                             #
#                                                                             #
#                                                                             #
###############################################################################

# visualizing the data in general
trainingDataProc$USERID <- as.factor(trainingDataProc$USERID)
ggplot(trainingDataProc, aes(x=LONGITUDE, y = LATITUDE, color = USERID)) +
  geom_jitter() + 
  facet_wrap(~USERID) +
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
