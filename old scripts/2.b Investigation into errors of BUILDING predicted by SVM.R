
#Move the info to the front
trainingData_Gathered <- trainingData[ , c((ncol(trainingData)-8):(ncol(trainingData)), 1:(ncol(trainingData)-11))]
validationData_Gathered <- validationData[ , c((ncol(validationData)-8):(ncol(validationData)), 1:(ncol(validationData)-11))]


# gather the data 
trainingData_Gathered <- gather(trainingData_Gathered, WAP, DBM, 10:ncol(trainingData_Gathered))
validationData_Gathered <- gather(validationData_Gathered, WAP, DBM, 10:ncol(validationData_Gathered))










B0F1 <- validationData %>%
  filter(PHONEID == 13)
B0F1[520:529]

hist(B0F1$PHONEID)

trainingData$PHONEID <- as.factor(trainingData$PHONEID)
ggplot(trainingData, aes(x=LONGITUDE, y=LATITUDE, color = PHONEID)) +
  geom_jitter() + 
  facet_wrap(~FLOOR)


BUILDING0 <-trainingData %>%
  filter(BUILDINGID == 0) 

BUILDING0V <-validationData %>%
  filter(BUILDINGID == 0) 

BUILDING0$PHONEID <- as.factor(BUILDING0$PHONEID)
ggplot(BUILDING0, aes(x=LONGITUDE, y=LATITUDE, color = PHONEID)) +
  geom_jitter() + 
  facet_wrap(~FLOOR)


BUILDING0V$PHONEID <- as.factor(BUILDING0V$PHONEID)
ggplot(BUILDING0V, aes(x=LONGITUDE, y=LATITUDE, color = PHONEID)) +
  geom_jitter() + 
  facet_wrap(~FLOOR)




sum(trainingData$PHONEID)
hist(validationData$PHONEID)


trainingData()




ALLWAP027T <- trainingData_Gathered %>%
  filter(WAP == "WAP027",
         DBM >= -80 & DBM <= -70)

ALLWAP027V <- validationData_Gathered %>%
  filter(WAP == "WAP027",
         DBM >= -80 & DBM <= -70)



# visualizing the data in general 
ALLWAP027T$PHONEID <- as.factor(ALLWAP027T$PHONEID)
ALLWAP027T$USERID <- as.factor(ALLWAP027T$USERID)
ggplot(ALLWAP027T, aes(x=LONGITUDE, y = LATITUDE, color = PHONEID)) +
  geom_jitter() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="Measurements of WAP027",
       subtitle = "seperated by people")

ALLWAP027V$PHONEID <- as.factor(ALLWAP027V$PHONEID)
ALLWAP027V$USERID <- as.factor(ALLWAP027V$USERID)
ggplot(ALLWAP027V, aes(x=LONGITUDE, y = LATITUDE, color = PHONEID)) +
  geom_jitter() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="Measurements of WAP027",
       subtitle = "seperated by people")


WAP027 <- validationData_Gathered %>%
  filter(WAP == "WAP027",
         DBM <= -30 & DBM >= -46)


WAP028 <- validationData_Gathered %>%
  filter(WAP == "WAP028",
         DBM <= -30 & DBM >= -46)

WAP057 <- validationData_Gathered %>%
  filter(WAP == "WAP057",
         DBM <= -30 & DBM >= -50)

WAP058 <- validationData_Gathered %>%
  filter(WAP == "WAP058",
         DBM <= -30 & DBM >= -50)


wap27 <-trainingData_Gathered %>%
  filter(WAP == "WAP027",
         DBM >= -90,
         BUILDINGID == 1)
ggplot(wap27, aes(x=LONGITUDE, y= LATITUDE, color = PHONEID))+
  geom_point()

ggplot(wap27) +
  geom_density(aes(x = DBM)) 


wap57 <-trainingData_Gathered %>%
  filter(WAP == "WAP057",
         DBM >= -90)

ggplot(wap58, aes(x=LONGITUDE, y= LATITUDE, color = BUILDINGID))+
  geom_point()

# looking into the errors of building


WAPS <- wrongBUILDING[wrongBUILDING <= -30]

#Move the info to the front
wrongBUILDING_gathered <- wrongBUILDING[ , c((ncol(wrongBUILDING)-8):(ncol(wrongBUILDING)), 1:(ncol(wrongBUILDING)-11))]

# gather the data 
wrongBUILDING_gathered <- gather(wrongBUILDING_gathered, WAP, DBM, 10:ncol(wrongBUILDING_gathered))

wrongBUILDING_gathered <- wrongBUILDING_gathered %>%
  filter(DBM <= -30 & DBM >=100)

ggplot(wrongBUILDING_gathered) +
  geom_density(aes(x = DBM)) +
  facet_wrap(~FLOOR, scale="free")


wrongBUILDING[,1:50]
wrongBUILDING[,51:100]
wrongBUILDING[,101:151]
