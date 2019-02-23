#reading the data file in csv format 

data <- read.csv(file.choose(),header=T)

#describing structure of data and doing some data preprocessing
#changing dependent variable from binary to categorical
#randomizing the rows in data set

str(data)

#data <- data[,-7]
data
data$Survivedf <- factor(data$Survived)
data
str(data)

#partioning data
set.seed(1234)
g <- runif(nrow(data))
data <- data[order(g),]

pd <- sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))
train <- data[pd==1,]
validate <- data[pd==2,]
#validate<- validate[,1-6]
#print(validate)
#decision tree


library(party)
data
tree <- ctree(Survivedf~MaternalAge+GestationalAge+GeneticAbnorm+Congeniticalabnorm+
                Rhdisease+Placental.UmbillicalcordAccident,data=train)
tree
summary(tree)

plot(tree)
#predict

p <- predict(tree,validate,type="prob")

#missclassification error in training
tab <- table(predict(tree),train$Survivedf)
print(tab)
1-sum(diag(tab))/sum(tab)

#missclassification in test
testpre <- predict(tree,newdata=validate)
tab <-table(testpre,validate$Survived)
print(tab)
1-sum(diag(tab))/sum(tab)

#classification using recursive partioning

library(rpart)
tree1 <- rpart(Survived~MaternalAge+GestationalAge+GeneticAbnorm+Congeniticalabnorm+
                 Rhdisease+Placental.UmbillicalcordAccident,train)
library(rpart.plot)
rpart.plot(tree1,extra=1)


#predict

predict(tree1,validate)

printcp(tree1)
#cross validation graph
plotcp(tree1)
summary(tree1)
tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"]
pfit<- prune(tree1, cp=   tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"])
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for survival")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)



#random forest

data <- read.csv(file.choose(),header=T)
data$Survived<-as.factor(data$Survived)
table(data$Survived)
#partion

set.seed(123)
ind <-sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
train <- data[ind==1,]
validate<-data[ind==2,]
#forest creation

library(randomForest)
set.seed(222)
rf <- randomForest(Survived~.,data=train)
print(rf)
attributes(rf)
#rf$confusion
#prediction
library(caret)
p1<-predict(rf,train)
confusionMatrix(p1,train$Survived)
#p2<-predict(rf,validate)
#confusionMatrix(p2,validate$Survived)
#error rate

plot(rf)
#tuning
t<-tuneRF(train[,-7],train[,7],stepFactor=0.5,
       plot=TRUE,
       ntreeTry=200,trace=TRUE,
       improve=0.05)

rf <- randomForest(Survived~.,data=train,
                   ntree=300,mtry=2,
                   importance=TRUE,
                   proximity=TRUE)
#variable importeance
varImpPlot(rf)
#partial dependence plot
partialPlot(rf,train,GestationalAge,"0")

