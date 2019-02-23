#NAIVE BAYES

data <- read.csv(file.choose(),stringsAsFactors <- TRUE)
#data <- read.csv(file.choose(),header=T)
summary(data)
str(data)
data$Survivedf <- factor(data$Survived)
#data$MaternalAgef <- factor(data$MaternalAge)
#data$GestationalAgef <- factor(data$GestationalAge)
str(data)
table(data$Survivedf)

#partioning data
set.seed(1234)
pd <- sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))
train <- data[pd==1,]
test <- data[pd==2,]
summary(train)
summary(test)

#naiveBayes
library(e1071)
library(rminer)

#model Build
model<-naiveBayes(Survivedf~MaternalAge+GestationalAge+GeneticAbnorm+Congeniticalabnorm+
                    Rhdisease+Placental.UmbillicalcordAccident,data=train)
#model summary
model

#checks the conditional probability of a variable 
prop.table(table(train$Rhdisease,train$Survivedf),2)

#check frequeny of a variable
table(train$Survivedf,train$Rhdisease)

#prediction on TestDataset
predictions<-predict(model,test)

#check prediction for n rows
head(predictions,n=10)
predictions

#confusion matrix
library(caret)
print(confusionMatrix(predictions,test$Survivedf,
                      positive="1",dnn = c("Prediction","True")))
table(test$Survivedf)
print(test$Survivedf)
library(Metrics)
#Metric to calculate accuracy,precision
mmetric(test$Survivedf,predictions,c("ACC","PRECISION","TPR","F1"))

