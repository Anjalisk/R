mydata <- read.table("~/Documents/2.ABA/Data/salary_data.csv", sep=",", header=T, strip.white = T, 
                     na.strings = c("NA","NaN", "","?"))

summary(mydata)
#we use education as numerical 

unique(mydata$occupation)
unique(mydata$native.country)

mydata$fnlwgt <- NULL
mydata$education.num <- NULL
mydata$relationship <- NULL
mydata$salary.class <- ifelse(mydata$salary.class == "<=50K", 0, 1)

#this will make it categorical
#mydata$salary.class<- as.factor(mydata$salary.class)


# Note: Since categorical variables enter into statistical models differently 
#than continuous variables, s toring data as factors insures that the modeling functions will treat 
#such data correctly: 

mydata$education <- as.factor(mydata$education)
mydata$martital.status <- as.factor(mydata$martital.status) 
mydata$occupation <- as.factor(mydata$occupation) 
mydata$race <- as.factor(mydata$race)
mydata$sex <- as.factor(mydata$sex) 
mydata$native.country <- as.factor(mydata$native.country)
mydata$salary.class <- as.factor(mydata$salary.class)

summary(mydata)

# Install packages required for random forest:
install.packages("randomForest")

# Load packages required for random forest:
library(randomForest)

# Since the data is large, we sample the first 5k observations:
train_data <- head(mydata, n = 5000) 
summary(train_data)

#RF Classifier
rf <-randomForest(salary.class~., data=train_data, ntree=10, na.action=na.exclude, 
                  importance=TRUE, proximity=TRUE)

# . = all the features except the mentioned, thus salary.class Target
# if using specific variable - salary.class~ education+marital status, data=train_data.......

print(rf)

# First, remove incomplete observations:
train_data <- mydata[complete.cases(mydata),]
mtry <- tuneRF(train_data[-12], train_data$salary.class, ntreeTry=20,
               stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)


best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1] 
print(mtry)

set.seed(32)
rf <-randomForest(salary.class~., data=train_data, mtry=best.m, importance=TRUE, ntree=20) 
print(rf)

#Evaluate variable importance
importance(rf)

varImpPlot(rf)

# Calculate predictive probabilities of training dataset.
pred1 = predict(rf,type = "prob")

# Sample the last 1000 observations of mydata and save it as test_ data
test_data = tail(mydata, n = 1000) 

# Use the rf classifier to make the predictions 
final_data <- cbind(test_data, predicted_values) 

# Add the predictions to test_data
predicted_values = predict(rf, type = "prob", test_data) 

# Add the new column names to the origina l column names
colnames <- c(colnames(test_data),"prob.zero","prob.one") 

# write the csv file of the output
write.table(final_data, file="RF_predictions.csv", sep=",", row.names=F, col.names = colnames) 

