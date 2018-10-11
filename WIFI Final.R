#libraries

library("dplyr")
library("ggplot2")
library("lattice")
library("caret")
library("scatterplot3d")
library("tidyr")
library("ISLR")
library("caTools")
library("lubridate")
library("plotly")
library ("e1071")

#import dataset

training <-read.csv("/Users/Aynaz/Documents/Berlin/ubiqum/R/002- Module 3/Module 3/UJIndoorLoc/trainingData.csv", na.strings = "?")
validation <- read.csv("/Users/Aynaz/Documents/Berlin/ubiqum/R/002- Module 3/Module 3/UJIndoorLoc/validationData.csv", na.strings = "?")
valid <- read.csv("/Users/Aynaz/Documents/Berlin/ubiqum/R/002- Module 3/Module 3/UJIndoorLoc/validationData.csv", na.strings = "?")

####change the type of attributes
#training dataset

training$FLOOR <- as.factor(training$FLOOR)
training$BUILDINGID <- as.factor(training$BUILDINGID)
training$SPACEID <- factor(training$SPACEID)
training$RELATIVEPOSITION<- factor(training$RELATIVEPOSITION)
training$USERID <- as.factor(training$USERID)
training$PHONEID <- as.factor(training$PHONEID)

#change time format

training$TIMESTAMP <- as.POSIXct(training$TIMESTAMP, origin="1970-01-01")

# Creat UniqueID 

training$UniqueID <- training %>%
  group_indices(BUILDINGID, FLOOR, SPACEID)
training$UniqueID <- as.factor(training$UniqueID)


#validation dataset

validation$BUILDINGID <- as.factor(validation$BUILDINGID)

#creating dataframe

training_df <- training [,1:520]
training_vector <- c(as.matrix(training_df))

# Count WAPs

training$WAPNOT100 <- apply(training_df,1,function(x)sum(x!=100))
WAP_col <- apply(training_df,2,function(x)sum(x!=100))
head(WAP_col[which(WAP_col>4831)])

# check/remove NAs

any(is.na(FLOORPREDICTION))
colSums (is.na(training_norm_FLOOR))
sum(is.na(training))

#explore -30 till 0 

training$range30to0 <-apply(training_df,1,function(x)sum(x>=-30 & x<=0))
training$LOg30to0 <- apply(training_df,1,function(row) "TRUE" %in% row)

#eliminating rows that include -30 to 0

training <- training %>% filter(range30to0 ==0 )

#### Removing coloumns that have a variance of 0 (all 100= no signal)#######

training <- training[ - as.numeric(which(apply(training, 2, var) == 0))]

####creating new data sets

#changing 100 to -105

training2 <- training
training2$LONGITUDE <- NULL
training2$LATITUDE <- NULL
training2$FLOOR <- NULL

training3 <-training2[,1:465]

training3[,1:464] <- data.frame(sapply(training3, function(x)ifelse(x ==100,-105,x )))
table(training3$BUILDINGID)
training3 $ BUILDINGID <- as.factor(training3 $ BUILDINGID)
training4 <- training3[sample(nrow(training3), 3000),]
training4$BUILDINGID <- as.factor(training4$BUILDINGID)
str(training4[,450:465])
table(training4$WAP001)

#### Preparing validation set

# matching validation attributes with training attributes
validation <- validation[, which(names(validation) %in% names(training4))]
str(validation[,450:465])

#check and remove var=0 in validation set
validation <- validation[ - as.numeric(which(apply(validation, 2, var) == 0))]
validation2<- validation
table(validation2$BUILDINGID)


#changing 100 to -105 in validation set

validation2 [,1:312]<- data.frame(sapply(validation2, function(x)ifelse(x == 100,-105, x )))
table(validation2$BUILDINGID)

# matching training attributes with validation attributes

training5 <- training4[, which(names(training4) %in% names(validation2))]
training5$BUILDINGID <- as.factor(training5$BUILDINGID )
validation2$BUILDINGID <- as.factor(validation2$BUILDINGID )
table(validation2$BUILDINGID)
table(training6$WAP001)

####### Sampling/test/train

inTraining <- createDataPartition(training5$BUILDINGID, p = .75, list = FALSE)
TR <- training5[inTraining,]
TEST <- training5[-inTraining,]
 
#fold cross validation

fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 5)
set.seed(123)

#Ranodm Forrest

RF <- train(BUILDINGID~., data = training5, method = "rf", trControl=fitControl)
Predictions_rf<- predict(RF,TEST)
predictors(RF)
postResample(Predictions_rf, TEST$BUILDINGID)


### predict BuildingID in validation data set with Random Forrest

table(validation2$BUILDINGID)
BIDRF <- predict(RF,validation2)
postResample(BIDRF,validation2$BUILDINGID)
str(BIDRF)
table(BIDRF)


############## predicting Floor##################

### preparing training data set

training6 <- training[,1:468]
training6$LATITUDE <-NULL
training6$ LONGITUDE <-NULL
training6$BUILDINGID <- as.factor(training6$BUILDINGID)
training6$FLOOR<-as.factor(training6$FLOOR)
table(training6$WAP001)
table(training7$WAP001)
str(training6[,1])
str(training7[,1:5])
training6[,1:464] <- data.frame(sapply(training6, function(x)ifelse(x ==100,-105,x )))
training6 $ BUILDINGID <- as.factor(training6 $ BUILDINGID)
training6 $ FLOOR <- as.factor(training6$FLOOR)
training7 <- training6[sample(nrow(training3), 3000),]
str(training6[,450:466])

### preparing validation data set

valid <- valid[ - as.numeric(which(apply(valid, 2, var) == 0))]
validation3 <-valid[,1:371]
validation3$LATITUDE <-NULL
validation3$ LONGITUDE <-NULL
validation3$BUILDINGID <- as.factor(validation3$BUILDINGID)
validation3$FLOOR<-as.factor(validation3$FLOOR)
validation3[,1:367] <- data.frame(sapply(validation3, function(x)ifelse(x ==100,-105,x )))

#####Matching both data frame/changing 100 to -105

training7 <- training7[, which(names(training7) %in% names(validation3))]
validation3 <- validation3[, which(names(validation3) %in% names(training7))]

validation3$BUILDINGID <-as.factor(validation3$BUILDINGID)
validation3$FLOOR <-as.factor(validation3$FLOOR)
training7$BUILDINGID <-as.factor(training7$BUILDINGID)
training7$FLOOR <-as.factor(training7$FLOOR)


############## test and train on training data set for predicting floor

inTrainingF <- createDataPartition(training7$FLOOR, p = .75, list = FALSE)
FLOORTRAINING <- training7[inTrainingF,]
FLOORTESTING <- training7[-inTrainingF,]

#fold cross validation

fitControlF <- trainControl(method = "repeatedcv", number = 2, repeats = 5)
set.seed(123)

########KNN

KNNFitF <- train(FLOOR~., data = FLOORTRAINING, method = "knn", trControl=fitControlF)
predictors(KNNFitF)
testPredKNNF <- predict(KNNFitF, FLOORTESTING)
postResample(testPredKNNF, FLOORTESTING$FLOOR)

#####RF

RFFLOOR <- train(FLOOR~., data = FLOORTRAINING, method = "rf", trControl=fitControlF)
predictors(RFFLOOR)
testPredRF <- predict(RFFLOOR, FLOORTESTING)
postResample(testPredRF, FLOORTESTING$FLOOR)  
varImp(RFFLOOR)

####SVM

svm_LinearF <- train(FLOOR~., data = FLOORTRAINING, method = "svmLinear",trControl=fitControlF)
Predictions_SVMF <-predict(svm_LinearF, FLOORTESTING)
postResample(Predictions_SVMF,FLOORTESTING$FLOOR)


### predict FLOOR in validation data set with KNN

validation4 <- cbind(validation3,BIDRF)
validation4$BUILDINGID <-NULL
names(validation4)[314]<-"BUILDINGID"

table(validation4$FLOOR)
RFFLOORvalidation <- predict(RFFLOOR, validation4)
postResample(RFFLOORvalidation,validation4$FLOOR)
str(RFFLOORvalidation )
table(RFFLOORvalidation )

#######################################################################
#### PREDICTING BUILDINGID AND FLOOR WITH NORMALISED DATASETS###

###normalise WAPs

training_norm <-training3
BID <- training3[,465]
training_norm <- as.data.frame(apply(training_norm[, 1:464], 2, function(x) (x - min(x))/(max(x)-min(x))))
training_norm <- cbind(training_norm,BID)
names(training_norm)[465]<-"BUILDINGID"
training_norm$BUILDINGID <- as.factor(training_norm$BUILDINGID)
table(training_norm$BUILDINGID)

validation_norm <-validation3
VBID <- validation3[,314]
validation_norm <- as.data.frame(apply(validation3[, 1:312], 2, function(x) (x - min(x))/(max(x)-min(x))))
validation_norm <- cbind(validation_norm,VBID)
names(validation_norm)[313]<-"BUILDINGID"
validation_norm$BUILDINGID <- as.factor(validation_norm$BUILDINGID)
table(validation_norm$BUILDINGID)

#matching training attributes with validation attributes

training_norm <- training_norm[, which(names(training_norm) %in% names(validation_norm))]
validation_norm <- validation_norm[, which(names(validation_norm) %in% names(training_norm))]

### Building model

## test and train on training data set for predicting BUILDINGID

TRnorm <-training_norm[sample(nrow(training_norm), 3000),]
inTraining_norm <- createDataPartition(TRnorm$BUILDINGID, p = .75, list = FALSE)
BIDTR <- TRnorm[inTraining_norm,]
BIDTEST <- TRnorm[-inTraining_norm,]

#fold cross validation

fitControl_norm <- trainControl(method = "repeatedcv", number = 2, repeats = 5)
set.seed(123)

#Ranodm Forrest

RFnorm <- train(BUILDINGID~., data = BIDTR, method = "rf", trControl=fitControl)
Predictions_rf_norm<- predict(RFnorm,BIDTEST)
predictors(RFnorm)
postResample(Predictions_rf_norm, BIDTEST$BUILDINGID)

### predict BuildingID in validation data set with Random Forrest

table(validation_norm$BUILDINGID)
BIDRF_norm <- predict(RFnorm,validation_norm)
postResample(BIDRF_norm,validation_norm$BUILDINGID)
Predicted <- write.csv(data.frame(predict(RFnorm,validation_norm)), "BUILDING.csv")
Predicted2 <- read.csv("/Users/Aynaz/Documents/BUILDING.csv")
Prediction <- cbind(validation_norm,Predicted2)
Prediction$BUILDINGID <-NULL
Prediction$X <- NULL
names(Prediction)[313] <-"BUILDINGID"

####predicting Floor

##preparing data sets and matching them 

training_norm_FLOOR <- training6[sample(nrow(training3), 3000),]
FBID <- training_norm_FLOOR[,465:466]
training_norm_FLOOR <- as.data.frame(apply(training_norm_FLOOR[, 1:464], 2, function(x) (x - min(x))/(max(x)-min(x))))
training_norm_FLOOR <- cbind(training_norm_FLOOR,FBID)
training_norm_FLOOR$BUILDINGID <- as.factor(training_norm_FLOOR$BUILDINGID)
training_norm_FLOOR$FLOOR <- as.factor(training_norm_FLOOR$FLOOR)

FLOORPREDICTION <-Prediction
FLOORPREDICTION$BUILDINGID <- as.factor(FLOORPREDICTION$BUILDINGID )
FLOORPREDICTION <- cbind(FLOORPREDICTION,validation3[313])
FLOORPREDICTION$BUILDINGID <- as.factor(FLOORPREDICTION$BUILDINGID)
FLOORPREDICTION$FLOOR <- as.factor(FLOORPREDICTION$FLOOR)
training_norm_FLOOR <- training_norm_FLOOR[, which(names(training_norm_FLOOR) %in% names(FLOORPREDICTION))]
FLOORPREDICTION <- FLOORPREDICTION[, which(names(FLOORPREDICTION) %in% names(training_norm_FLOOR))]
table(training_norm_FLOOR$FLOOR)
training_norm_FLOOR$WAP001 <- NULL
colSums (is.na(FLOORPREDICTION))
sum(is.na(FLOORPREDICTION))

colSums (is.na(training_norm_FLOOR))
sum(is.na(training_norm_FLOOR))
str(training_norm_FLOOR[,200:310])

training_norm_FLOOR$WAP266 <-NULL
training_norm_FLOOR$WAP153 <-NULL
training_norm_FLOOR$WAP207 <-NULL
training_norm_FLOOR$WAP475 <-NULL
training_norm_FLOOR$WAP358 <-NULL
training_norm_FLOOR$WAP354 <-NULL
training_norm_FLOOR$WAP297 <-NULL
training_norm_FLOOR$WAP283 <-NULL

training_norm_FLOOR <- training_norm_FLOOR[, which(names(training_norm_FLOOR) %in% names(FLOORPREDICTION))]
FLOORPREDICTION <- FLOORPREDICTION[, which(names(FLOORPREDICTION) %in% names(training_norm_FLOOR))]

## test and train on training data set for predicting BUILDINGID

inTraining_norm_F <- createDataPartition(training_norm_FLOOR$FLOOR, p = .75, list = FALSE)
BIDTR_F <- training_norm_FLOOR[inTraining_norm_F,]
BIDTEST_F <- training_norm_FLOOR[-inTraining_norm_F,]

#fold cross validation

FN <- trainControl(method = "repeatedcv", number = 2, repeats = 5)
set.seed(123)

#Ranodm Forrest

RFnorm_F <- train(FLOOR~., data = BIDTR_F, method = "rf", trControl= FN, na.action = na.pass)
Predictions_rf_norm_F<- predict(RFnorm_F,BIDTEST_F)
predictors(RFnorm_F)
postResample(Predictions_rf_norm_F, BIDTEST_F$FLOOR)

### predict FLOOR in validation data set with Random Forrest

PREFL <- predict(RFnorm_F,FLOORPREDICTION)
postResample(PREFL,FLOORPREDICTION$FLOOR)


#######################PREDICTING LOCATION FINALLY######################

####prepare validation data set

write.csv(data.frame(predict(RFnorm_F,FLOORPREDICTION)), "FLOOR.csv")
LP <- read.csv("/Users/Aynaz/Documents/FLOOR.csv")
PA <- cbind(validation_norm,LP)
PA$X <-NULL
names(PA)[314] <-"FLOOR"
PA <- cbind(PA,valid[,368:369])


#### prepare training data set

FTA <- training[,1:468]
FTA <- FTA[, which(names(FTA) %in% names(PA))]
FTA[,1:312] <- data.frame(sapply(FTA, function(x)ifelse(x ==100,-105,x )))
colSums (is.na(FTA))
sum(is.na(FTA))
FTA2 <- FTA
FTA2_1 <- FTA2[,1:313]
FTA2 <- cbind(FTA2_1,FTA2[,315:316])
PA <- PA[, which(names(PA) %in% names(FTA2))]
FTA2 <- FTA2[sample(nrow(FTA2), 3000),]

#### test and train

intrainingLOC1 <- createDataPartition(FTA2$LONGITUDE, p = .75, list = FALSE)
TRLOC1 <- FTA2[intrainingLOC1,]
TESTLOC1 <- FTA2[-intrainingLOC1,]

TCLOC1 <- trainControl(method = "repeatedcv", number = 2, repeats = 5)
set.seed(123)

#Ranodm Forrest

RFLOC1 <- train(LONGITUDE~., data = TRLOC1, method = "rf", trControl= TCLOC1)
PredictionsLOC1 <- predict(RFLOC1,TESTLOC1)
predictors(RFLOC1)
postResample(PredictionsLOC1, TESTLOC1$LONGITUDE)

####use RF for predicting longtitude in validation (PA) dataset

RFLONG <-predict(RFLOC1,PA)
PA$FLOOR <- as.factor(PA$FLOOR)
postResample(RFLONG, PA$LONGITUDE)
write.csv(data.frame(predict(RFLOC1,PA)), "LONGTITUDE.csv")
PRELONG <- read.csv("/Users/Aynaz/Documents/LONGTITUDE.csv")

#### Latitude

FTA3 <- FTA
FTA3_1 <- FTA3[,1:312]
FTA3 <- cbind(FTA3_1,FTA3[,314:316])
FTA3 <- FTA3[sample(nrow(FTA3), 3000),]
PA2 <-cbind(PA,valid[,369])
names(PA2)[316] <-"LATITUDE"
PA2 <- PA2[, which(names(PA2) %in% names(FTA3))]
FTA3 <- FTA3[, which(names(FTA3) %in% names(PA2))]

####build model

intrainingLOC2 <- createDataPartition(FTA3$LATITUDE, p = .75, list = FALSE)
TRLOC2 <- FTA3[intrainingLOC2,]
TESTLOC2 <- FTA3[-intrainingLOC2,]

TCLOC2 <- trainControl(method = "repeatedcv", number = 2, repeats = 5)
set.seed(123)

#Ranodm Forrest

RFLOC2 <- train(LATITUDE~., data = TRLOC2, method = "rf", trControl= TCLOC1)
PredictionsLOC2 <- predict(RFLOC2,TESTLOC2)
predictors(RFLOC2)
postResample(PredictionsLOC2, TESTLOC2$LATITUDE)

####use RF for predicting latitude in validation (PA2) dataset

RFLAT <-predict(RFLOC2,PA2)
PA2$FLOOR <- as.factor(PA2$FLOOR)
postResample(RFLAT, PA2$LATITUDE)
write.csv(data.frame(predict(RFLOC2,PA2)), "LATITUDE.csv")
PRELATITUDE <- read.csv("/Users/Aynaz/Documents/LATITUDE.csv")

#### creating a validation data set with predicted BUILDINGID,FLOOR,LONGTITUDE AND LATITUDE####

FINALPREDICTIONS <- cbind(PA,PRELONG,PRELATITUDE)
FINALPREDICTIONS[,318]<-NULL
FINALPREDICTIONS[,316]<-NULL
FINALPREDICTIONS[,315]<-NULL
names(FINALPREDICTIONS)[315] <- "P.LONGTITUDE"
names(FINALPREDICTIONS)[316] <- "P.LATITUDE"
FINALPREDICTIONS2 <- cbind(FINALPREDICTIONS,valid[,368:369])

FINALPREDICTIONS3 <- FINALPREDICTIONS2 %>% mutate(x=(FINALPREDICTIONS2[,318]-FINALPREDICTIONS2[,316])**2)
FINALPREDICTIONS3 <- FINALPREDICTIONS3 %>% mutate(y=(FINALPREDICTIONS2[,317]-FINALPREDICTIONS2[,315])**2)
FINALPREDICTIONS3 <- FINALPREDICTIONS3 %>% mutate(distance=((x**2)+(y**2))**0.5)
summary(FINALPREDICTIONS3$distance)
histogram(FINALPREDICTIONS3$distance)
