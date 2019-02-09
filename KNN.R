library(ROCR)
library(ggplot2)
library(class)
library(caret)


mydata <- read.table("salary_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
mydata <- mydata[complete.cases(mydata),]
head(mydata)
summary(mydata)

mydata$fnlwgt <- NULL
mydata$education.num <- NULL
mydata$relationship <- NULL
mydata$salary.class <- ifelse(mydata$salary.class == "<=50K", 0, 1)
mydata$salary.class <- as.factor(mydata$salary.class)
## Note: Since categorical variables enter into statistical models differently than continuous variables, storing data as factors insures that the modeling functions will treat such data correctly:
mydata$education <- as.factor(mydata$education)
mydata$ord_education <- factor(mydata$education,
                               levels = c("Preschool","1st-4th",
                                          "5th-6th","7th-8th",
                                          "9th","10th",
                                          "11th","12th","HS-grad",
                                          "Prof-school",
                                          "Assoc-voc","Assoc-acdm",
                                          "Some-college","Bachelors",
                                          "Masters","Doctorate"),
                               ordered=TRUE) # Convert to ordinal 

mydata$edu <- as.numeric(mydata$ord_education)
mydata$martital.status <- as.factor(mydata$martital.status)
mydata$occupation <- as.factor(mydata$occupation)
mydata$race <- as.factor(mydata$race)
mydata$sex <-as.factor(mydata$sex)
mydata$native.country <- as.factor(mydata$native.country)
summary(mydata)
class(mydata$salary.class)
mydata <- mydata[1:1000,c("age", "capital.gain", "capital.loss","hours.per.week","edu","salary.class")]

## Create the training and test data:
n = nrow(mydata) # n will be ther number of obs. in data
trainIndex = sample(1:n, 
                    size = round(0.7*n), 
                    replace=FALSE) # We create an index for 70% of obs. by random
train_data = mydata[trainIndex,] # We use the index to create training data
test_data = mydata[-trainIndex,] # We take the remaining 30% as the testing data
summary(train_data)
summary(test_data)

# Build KNN classifier:
nb <- train(salary.class ~ ., data = train_data, method = "naive_bayes")

nb
plot(nb)

## Model Evaluation:
predicted_values <- predict(knnFit, test_data, type= "prob") # Use the classifier to make the predictions

head(predicted_values)
threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, 1, 0))
head(pred)
levels(pred)
levels(test_data$salary.class)
confusionMatrix(pred, test_data$salary.class, 
                positive = levels(test_data$salary.class)[2])

predicted_values <- predict(knnFit, test_data, type= "prob") # Use the classifier to make the predictions
pred <- prediction(predicted_values[,2], test_data$salary.class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="Decision Tree")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))