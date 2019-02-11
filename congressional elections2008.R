setwd("~/Documents/ABA")

library(dplyr)

## Read the file

mydata <- read.csv("election_campaign_data.csv", sep=",", 
                 header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

# Drop the variables of the dataframe

mydata <- select (mydata,-c("cand_id", "last_name", "first_name", "twitterbirth", "facebookdate", 
                "facebookjan", "youtubebirth"))

#If lost then 0 if won 1
mydata$gen_election <- ifelse(mydata$gen_election == "L", 0, 1)

## Covert the variables into factor variables

mydata$twitter<- as.factor(mydata$twitter)
mydata$facebook<- as.factor(mydata$facebook)
mydata$youtube<- as.factor(mydata$youtube)
mydata$cand_ici<- as.factor(mydata$cand_ici)
mydata$gen_election<- as.factor(mydata$gen_election)

## Removing all the observations with missing values

mydata <- mydata[complete.cases(mydata),]

## Create the training and test data:

# n will be ther number of obs. in data

n = nrow(mydata)

# We create an index for 70% of obs. by random

trainIndex = sample(1:n, 
                    size = round(0.7*n), 
                    replace=FALSE) 

# We use the index to create training data

train_data = mydata[trainIndex,] 

# We take the remaining 30% as the testing data

test_data = mydata[-trainIndex,] 

summary(train_data)
summary(test_data)

# Load packages required for random forest:

library(randomForest)

#RF Classifier with ntree 10

rf_10 <-randomForest(gen_election ~., data=train_data, ntree=10, na.action=na.exclude, 
                  importance=TRUE, proximity=TRUE)

print(rf_10)

#RF Classifier with ntree 20

rf_20 <-randomForest(gen_election ~., data=train_data, ntree=20, na.action=na.exclude, 
                  importance=TRUE, proximity=TRUE)

print(rf_20)

#RF Classifier with ntree 30

rf_30 <-randomForest(gen_election ~., data=train_data, ntree=30, na.action=na.exclude, 
                  importance=TRUE, proximity=TRUE)

print(rf_30)

#RF Classifier with ntree 40

rf_40 <-randomForest(gen_election ~., data=train_data, ntree=40, na.action=na.exclude, 
                  importance=TRUE, proximity=TRUE)

print(rf_40)

#RF Classifier with ntree 50

rf_50 <-randomForest(gen_election ~., data=train_data, ntree=50, na.action=na.exclude, 
                  importance=TRUE, proximity=TRUE)

print(rf_50)

#RF Classifier with ntree 60

rf_60 <-randomForest(gen_election ~., data=train_data, ntree=60, na.action=na.exclude, 
                  importance=TRUE, proximity=TRUE)

print(rf_60)

#RF Classifier with ntree 70

rf_70 <-randomForest(gen_election ~., data=train_data, ntree=70, na.action=na.exclude, 
                  importance=TRUE, proximity=TRUE)

print(rf_70)

#RF Classifier with ntree 80

rf_80 <-randomForest(gen_election ~., data=train_data, ntree=80, na.action=na.exclude, 
                  importance=TRUE, proximity=TRUE)

print(rf_80)

#RF Classifier with ntree 90

rf_90 <-randomForest(gen_election ~., data=train_data, ntree=90, na.action=na.exclude, 
                     importance=TRUE, proximity=TRUE)

print(rf_90)

#RF Classifier with ntree 100

rf_100 <-randomForest(gen_election ~., data=train_data, ntree=100, na.action=na.exclude, 
                     importance=TRUE, proximity=TRUE)

print(rf_100)

#RF Classifier with ntree 100

rf_110 <-randomForest(gen_election ~., data=train_data, ntree=110, na.action=na.exclude, 
                      importance=TRUE, proximity=TRUE)

print(rf_110)


#RF Classifier with ntree 120

rf_120 <-randomForest(gen_election ~., data=train_data, ntree=120, na.action=na.exclude, 
                      importance=TRUE, proximity=TRUE)

print(rf_120)


#Tuning RF

mtry <- tuneRF(train_data[-26], train_data$gen_election, ntreeTry=80,  
               stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE,
               na.action=na.exclude)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

#RF with the best mtry

set.seed(32)

rf <-randomForest(gen_election~., data=train_data, 
                  mtry=best.m, importance=TRUE, ntree=80) 

print(rf)


# RF on test data

predicted_values <- predict(rf, test_data, type= "prob")

# Confusion matrix

library(caret)

threshold <- 0.5 

pred <- factor( ifelse(predicted_values[,2] > threshold, 1, 0) )

head(pred)

levels(test_data$gen_election)[2]

confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2]) 

#ROC


library(ggplot2)
library(ROCR)
library(class)

predicted_values <- predict(rf, test_data,type= "prob")

pred <- prediction(predicted_values[,2], test_data$gen_election)

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

importance(rf)

varImpPlot(rf)

#ANN

# Load packages required for random forest:
library(nnet)

ann <- nnet(gen_election ~ ., data=train_data, size=5, maxit=1000)

summary(ann)

# Use the gbm classifier to make the prediction s
predicted_values <- predict(ann, test_data, type= "raw") 

predicted_values

threshold <- 0.5 

pred <- factor( ifelse(predicted_values[,1] > threshold, 1, 0) ) 

head(pred) 

levels(test_data$gen_election)[2]

# Confusion matrix

confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2]) 
#ROC

predicted_values <- predict(ann, test_data,type= "raw")
pred <- prediction(predicted_values, test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc)) 

#Experiment with hidden nodes

ann_24 <- nnet(gen_election ~ ., data=train_data, size=24, maxit=1000)

summary(ann)

# Use the gbm classifier to make the prediction s
predicted_values <- predict(ann, test_data, type= "raw") 

predicted_values

threshold <- 0.5 

pred <- factor( ifelse(predicted_values[,1] > threshold, 1, 0) ) # We ask R to use the threshold and convert the probabilities to class labels (zero and one)

head(pred) 

levels(test_data$gen_election)[2]

# Confusion matrix

confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2]) 
#ROC

predicted_values <- predict(ann, test_data,type= "raw")
pred <- prediction(predicted_values, test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN_24")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc)) 



ftable(xtabs(~twitter+gen_election, data=mydata))
ftable(xtabs(~facebook+gen_election, data=mydata))
ftable(xtabs(~youtube+gen_election, data=mydata))

#filter to keep only the candidates who won the election.  
basic_summ_0 = filter(mydata, gen_election %in% c(0))

# set up data frame for by-group processing.  
basic_summ_0 = group_by(basic_summ_0, coh_bop, ttl_disb, ttl_receipts)



#filter to keep only the candidates who won the election.  
basic_summ_1 = filter(mydata, gen_election %in% c(1))

# 2: set up data frame for by-group processing.  
basic_summ_1 = group_by(basic_summ_1, coh_bop, ttl_disb, ttl_receipts)
