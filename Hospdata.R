setwd("~/Documents/1.Exam")

library(dplyr)

#--------Read the data--------

mydata <- read.csv("Hospital_General_Information.csv", sep=",", 
                   header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

#--------Deleting the rows not required for our analysis--------

mydata$Provider.ID <- NULL
mydata$Hospital.Name <- NULL
mydata$Address <- NULL
mydata$County.Name <- NULL
mydata$Phone.Number <- NULL
mydata$Location <- NULL
mydata$ZIP.Code <- NULL
mydata$City <- NULL
mydata$State <- NULL

#--------Applying the asfactor to the variables--------

mydata$Hospital.Type <- as.factor(mydata$Hospital.Type)
mydata$Hospital.Ownership <- as.factor(mydata$Hospital.Ownership)
mydata$Emergency.Services <- as.factor(mydata$Emergency.Services)
mydata$Mortality.national.comparison <- as.factor(mydata$Mortality.national.comparison)
mydata$Safety.of.care.national.comparison <- as.factor(mydata$Safety.of.care.national.comparison)
mydata$Patient.experience.national.comparison <- as.factor(mydata$Patient.experience.national.comparison)
mydata$Effectiveness.of.care.national.comparison <- as.factor(mydata$Effectiveness.of.care.national.comparison)
mydata$Timeliness.of.care.national.comparison <- as.factor(mydata$Timeliness.of.care.national.comparison)
mydata$Efficient.use.of.medical.imaging.national.comparison <- as.factor(mydata$Efficient.use.of.medical.imaging.national.comparison)
mydata$low_rating <- as.factor(mydata$low_rating)

#--------Split the data into test and train--------

set.seed(651)

# rfn will be the number of obs. in data

rfn = nrow(mydata)

# Creating an index for 63.2% of obs. by random

rftrainIndex = sample(1:rfn, 
                      size = round(0.632*rfn), 
                      replace=FALSE) 

# We use the index to create training data

rftrain_data = mydata[rftrainIndex,] 
rftest_data = mydata[-rftrainIndex,]

nrow(rftrain_data)

nrow(rftest_data)

#--------Random Forest Classification model--------

install.packages("randomForest")
library(randomForest)

rf1 <-randomForest(low_rating~ Hospital.Type + 
                     Hospital.Ownership + Emergency.Services + 
                     Mortality.national.comparison + 
                     Safety.of.care.national.comparison + 
                     Patient.experience.national.comparison + 
                     Effectiveness.of.care.national.comparison + 
                     Timeliness.of.care.national.comparison + 
                     Efficient.use.of.medical.imaging.national.comparison, 
                    data=rftrain_data, ntree=10, na.action=na.exclude, 
                   importance=T, proximity=T) 
print(rf1)

rf2 <-randomForest(low_rating~ Hospital.Type + 
                     Hospital.Ownership + Emergency.Services + 
                     Mortality.national.comparison + 
                     Safety.of.care.national.comparison + 
                     Patient.experience.national.comparison + 
                     Effectiveness.of.care.national.comparison + 
                     Timeliness.of.care.national.comparison + 
                     Efficient.use.of.medical.imaging.national.comparison, 
                   data=rftrain_data, ntree=20, na.action=na.exclude, 
                   importance=T, proximity=T) 

print(rf2)

rf3 <-randomForest(low_rating~ Hospital.Type + 
                     Hospital.Ownership + Emergency.Services + 
                     Mortality.national.comparison + 
                     Safety.of.care.national.comparison + 
                     Patient.experience.national.comparison + 
                     Effectiveness.of.care.national.comparison + 
                     Timeliness.of.care.national.comparison + 
                     Efficient.use.of.medical.imaging.national.comparison, 
                   data=rftrain_data, ntree=30, na.action=na.exclude, 
                   importance=T, proximity=T) 

print(rf3)

rf4 <-randomForest(low_rating~ Hospital.Type + 
                     Hospital.Ownership + Emergency.Services + 
                     Mortality.national.comparison + 
                     Safety.of.care.national.comparison + 
                     Patient.experience.national.comparison + 
                     Effectiveness.of.care.national.comparison + 
                     Timeliness.of.care.national.comparison + 
                     Efficient.use.of.medical.imaging.national.comparison, 
                   data=rftrain_data, ntree=40, na.action=na.exclude, 
                   importance=T, proximity=T)

print(rf4)

rf5 <-randomForest(low_rating~ Hospital.Type + 
                     Hospital.Ownership + Emergency.Services + 
                     Mortality.national.comparison + 
                     Safety.of.care.national.comparison + 
                     Patient.experience.national.comparison + 
                     Effectiveness.of.care.national.comparison + 
                     Timeliness.of.care.national.comparison + 
                     Efficient.use.of.medical.imaging.national.comparison, 
                   data=rftrain_data, ntree=50, na.action=na.exclude, 
                   importance=T, proximity=T)

print(rf5)

rf6 <-randomForest(low_rating~ Hospital.Type + 
                     Hospital.Ownership + Emergency.Services + 
                     Mortality.national.comparison + 
                     Safety.of.care.national.comparison + 
                     Patient.experience.national.comparison + 
                     Effectiveness.of.care.national.comparison + 
                     Timeliness.of.care.national.comparison + 
                     Efficient.use.of.medical.imaging.national.comparison, 
                   data=rftrain_data, ntree=60, na.action=na.exclude, 
                   importance=T, proximity=T)

print(rf6)

rf7 <-randomForest(low_rating~ Hospital.Type + 
                     Hospital.Ownership + Emergency.Services + 
                     Mortality.national.comparison + 
                     Safety.of.care.national.comparison + 
                     Patient.experience.national.comparison + 
                     Effectiveness.of.care.national.comparison + 
                     Timeliness.of.care.national.comparison + 
                     Efficient.use.of.medical.imaging.national.comparison, 
                   data=rftrain_data, ntree=70, na.action=na.exclude, 
                   importance=T, proximity=T)

print(rf7)

rf8 <-randomForest(low_rating~ Hospital.Type + 
                     Hospital.Ownership + Emergency.Services + 
                     Mortality.national.comparison + 
                     Safety.of.care.national.comparison + 
                     Patient.experience.national.comparison + 
                     Effectiveness.of.care.national.comparison + 
                     Timeliness.of.care.national.comparison + 
                     Efficient.use.of.medical.imaging.national.comparison, 
                   data=rftrain_data, ntree=80, na.action=na.exclude, 
                   importance=T, proximity=T)

print(rf8)


rf9 <-randomForest(low_rating~ Hospital.Type + 
                     Hospital.Ownership + Emergency.Services + 
                     Mortality.national.comparison + 
                     Safety.of.care.national.comparison + 
                     Patient.experience.national.comparison + 
                     Effectiveness.of.care.national.comparison + 
                     Timeliness.of.care.national.comparison + 
                     Efficient.use.of.medical.imaging.national.comparison, 
                   data=rftrain_data, ntree=90, na.action=na.exclude, 
                   importance=T, proximity=T)

print(rf9)

rf10 <-randomForest(low_rating~ Hospital.Type + 
                     Hospital.Ownership + Emergency.Services + 
                     Mortality.national.comparison + 
                     Safety.of.care.national.comparison + 
                     Patient.experience.national.comparison + 
                     Effectiveness.of.care.national.comparison + 
                     Timeliness.of.care.national.comparison + 
                     Efficient.use.of.medical.imaging.national.comparison, 
                   data=rftrain_data, ntree=100, na.action=na.exclude, 
                   importance=T, proximity=T)

print(rf10)

colnames(rftrain_data)

#excluding lowrating 

summary(rftrain_data[-c(1,10)]) 

#Tuning RF

mtry <- tuneRF(rftrain_data[-c(1,10)], rftrain_data$low_rating,ntreeTry=100, 
               stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)

print(best.m)

#--------Final model--------
set.seed(5413)
rf <-randomForest(low_rating~ 
                    Hospital.Type + Hospital.Ownership + 
                    Emergency.Services + Mortality.national.comparison + 
                    Safety.of.care.national.comparison + 
                    Patient.experience.national.comparison + 
                    Effectiveness.of.care.national.comparison + 
                    Timeliness.of.care.national.comparison + 
                    Efficient.use.of.medical.imaging.national.comparison, 
                  data=rftrain_data, mtry=best.m, importance=TRUE, ntree=100)

print(rf)

#-------Variable importance------

importance(rf)
varImpPlot(rf)

#-------Model Evaluation--------

library(caret)

predicted_values <- predict(rf, rftest_data,type= "prob")

head(predicted_values)

# Add the predictions to test_data
final_data <- cbind(rftest_data, predicted_values) 

# Add the predictions to test_data
colnames <- c(colnames(rftest_data),"prob.zero","prob.one")

# write the csv file of the output
write.table(final_data, file="RF.csv", sep=",", row.names=F, col.names = colnames) 

#---------Confusion Matrix------- 

threshold <- 0.5

pred <- factor( ifelse(predicted_values[,2] > threshold, 1, 0) )

levels(rftest_data$low_rating)[2]

confusionMatrix(pred, rftest_data$low_rating, 
                
                positive = levels(rftest_data$low_rating)[2])

#-------ROC Curve-------

library(ROCR)

library(ggplot2)

predicted_values <- predict(rf, rftest_data,type= "prob")[,2] 

pred <- prediction(predicted_values, rftest_data$low_rating)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")

auc <- performance(pred, measure = "auc")

auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

#---------------------------- NEURAL NETWORK -------------------------

#install.packages("nnet")

library(nnet)

n = nrow(mydata)

#colnames(mydata)

set.seed(61327)

#Indexing for 70% of observations by random

ntrainIndex = sample(1:n, 
                      size = round(0.7*n), 
                      replace=FALSE) 

#Using the index creating the training dataset

ntrain_data = mydata[ntrainIndex,] 

ntest_data = mydata[-ntrainIndex,] 

summary(ntrain_data)

#Fitting the ann to the training data 

ann <- nnet(low_rating ~ ., data=ntrain_data, size=7, maxit=1000)

summary(ann)

#Predictions using Caret 

library(caret)

predicted_values <- predict(ann, ntest_data,type= "raw") 

head(predicted_values)

threshold <- 0.5 

pred <- factor( ifelse(predicted_values[,1] > threshold, 1, 0) ) 

head(pred)

levels(ntest_data$low_rating)[2]

#---------Confusion Matrix------- 

confusionMatrix(pred, ntest_data$low_rating, 
                positive = levels(ntest_data$low_rating)[2]) 

#-------ROC Curve-------

library(ROCR)
library(ggplot2)

predicted_values <- predict(ann, ntest_data,type= "raw")

pred <- prediction(predicted_values, ntest_data$low_rating)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")

auc <- performance(pred, measure = "auc")

auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))


summary(mydata)


#Which type of hospitals (“Acute Care Hospitals” 
#or “Critical Access Hospitals”) is more likely to have a low rating? 

ftable(xtabs(~Hospital.Type + low_rating, data=mydata))

#Do hospitals with Emergency Services have a better rating than the others?

ftable(xtabs(~Emergency.Services + low_rating, data=mydata))

#Imagine you received two proposals for building a new hospital. 
#All things equal, if one of them proposes a “Government - State” and 
#another proposes “Government – Local”, which one would you approve? Why?

table(mydata$Hospital.Ownership, mydata$low_rating)
ftable(xtabs(~Hospital.Ownership + low_rating, data=mydata))

#What are the best and the worst states in terms of proportion of hospitals 
#with low quality rating?

ftable(xtabs(~State + low_rating, data=mydata))


