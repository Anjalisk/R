library(ROCR)
library(ggplot2)
library(class)
library(caret)
library(shiny)

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
mydata <- mydata[1:2000,c("age", "capital.gain", "capital.loss","hours.per.week","edu","salary.class")]

## Create the training and test data:
n = nrow(mydata) # n will be ther number of obs. in data
trainIndex = sample(1:n, 
                    size = round(0.7*n), 
                    replace=FALSE) # We create an index for 70% of obs. by random
train_data = mydata[trainIndex,] # We use the index to create training data
test_data = mydata[-trainIndex,] # We take the remaining 30% as the testing data
summary(train_data)
summary(test_data)

# Build the classifier:
Model = "nb"

rf <- train(salary.class ~ ., data = train_data, method = Model, tuneLength = 4, 
            preProcess = c("center", "scale"))

rf

## Model Evaluation:
predicted_values <- predict(rf, test_data, type= "prob") # Use the classifier to make the predictions

head(predicted_values)
threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, 1, 0))
head(pred)
levels(pred)
levels(test_data$salary.class)
confusionMatrix(pred, test_data$salary.class, 
                positive = levels(test_data$salary.class)[2])

predicted_values <- predict(rf, test_data, type= "prob") # Use the classifier to make the predictions
pred <- prediction(predicted_values[,2], test_data$salary.class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model=Model)

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))


ui = pageWithSidebar(
  headerPanel("Salary Example"),
  sidebarPanel(
    selectInput("model", "Please choose a model:",
                choices = c("rf", "nb")),
    sliderInput("edu", "Education Level", min=1, max=16, value=10),
    sliderInput("age", "Age", min=0, max=80, value=30),
    sliderInput("capital.gain", "Capital Gain", min=0, max=200000, value=35),
    sliderInput("capital.loss", "Capital Loss", min=0, max=200000, value=0),
    sliderInput("hours.per.week", "Hours Worked Per Week", min=0, max=70, value=40)
  ),
  mainPanel(
    dataTableOutput('testTable'),
    textOutput('outputBox')
  )
)

server=function(input, output){
  
  values <- reactiveValues()
  
  newEntry <- observe({ # use observe pattern
    
    x=as.data.frame(matrix(0, nrow=1, ncol=7))
    colnames(x)=c("edu", "age", "capital.gain", "capital.loss","hours.per.week","Probability", "Prediction")
    
    x[1,1]=as.numeric(input$edu)
    x[1,2]=input$age
    x[1,3]=as.numeric(input$capital.gain)
    x[1,4]=as.numeric(input$capital.loss)
    x[1,5]=input$hours.per.week
    
    classifier <- train(salary.class ~ ., data = train_data, method = input$model, tuneLength = 4, 
                preProcess = c("center", "scale"))
    
    
    ## Model Evaluation:
    predicted_values <- predict(classifier, test_data, type= "prob") # Use the classifier to make the predictions
    
    
    prob <- data.matrix(predict(object=classifier, x[-length(x)], type="prob")[,2])
    x[1,6] <- round(prob,2)
    pred <- ifelse(data.matrix(predict(object=classifier, x[-length(x)], type="prob")[,2])> threshold, 1, 0)
    x[1,7] <- round(pred,2)
    
    isolate(values$df <- x)
  })
  
  output$testTable <- renderDataTable({values$df})
}

shinyApp(ui=ui, server=server)
