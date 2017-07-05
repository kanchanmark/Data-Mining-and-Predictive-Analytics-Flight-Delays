file1=read.csv("flights1.csv")
head(file1)
data.work1=file1

#data.work1$ARRIVAL_DELAY_BUCKETED <- data.work1$ARRIVAL_DELAY/15
#data.work1$ARRIVAL_DELAY_BUCKETED <- as.integer(data.work1$ARRIVAL_DELAY_BUCKETED) 
data.work1$ARRIVAL_DELAY_BUCKETED <- ifelse(data.work1$ARRIVAL_DELAY_BUCKETED <= 0, 0, data.work1$ARRIVAL_DELAY_BUCKETED)
data.work1$FINAL2=ifelse(data.work1$ARRIVAL_DELAY_BUCKETED <= 8, 0, 1)
data.work1$FINAL4=ifelse(data.work1$ARRIVAL_DELAY_BUCKETED <= 16, 0, 1)

str(data.work1)

data.work1$ARRIVAL_DELAY_BUCKETED_TEMP=NULL
data.work1$ARRIVAL_DELAY_BUCKETED=NULL
data.work1$DIVERTED=NULL
data.work1$CANCELLED=NULL
data.work1$CANCELLATION_REASON=NULL

data.work1$DAY_OF_WEEK <- as.factor(data.work1$DAY_OF_WEEK)
data.work1$AIRLINE <- as.factor(data.work1$AIRLINE)

data.work1$YEAR=NULL
data.work1$DAY=NULL
data.work1$MONTH=NULL
data.work1$FLIGHT_NUMBER=NULL
data.work1$TAIL_NUMBER=NULL
data.work1$SCHEDULED_DEPARTURE=data.work1$SCHEDULED_DEPARTURE/100
data.work1$SCHEDULED_DEPARTURE=round(data.work1$SCHEDULED_DEPARTURE)
data.work1$DEPARTURE_TIME=NULL
data.work1$WHEELS_OFF=NULL
data.work1$WHEELS_ON=NULL
data.work1$SCHEDULED_ARRIVAL=data.work1$SCHEDULED_ARRIVAL/100
data.work1$SCHEDULED_ARRIVAL=round(data.work1$SCHEDULED_ARRIVAL)
data.work1$ARRIVAL_TIME=NULL
data.work1$AIR_SYSTEM_DELAY=NULL
data.work1$SECURITY_DELAY=NULL
data.work1$AIRLINE_DELAY=NULL
data.work1$LATE_AIRCRAFT_DELAY=NULL
data.work1$WEATHER_DELAY=NULL
data.work1$DEPARTURE_DELAY=NULL
data.work1$ARRIVAL_DELAY=NULL


data.work1$SCHEDULED_DEPARTURE <- as.factor(data.work1$SCHEDULED_DEPARTURE)
data.work1$SCHEDULED_ARRIVAL <- as.factor(data.work1$SCHEDULED_ARRIVAL)
#levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))
data.work1$ARRIVAL_DELAY_BUCKETED <- as.factor(data.work1$ARRIVAL_DELAY_BUCKETED)

data.work1=na.omit(data.work1)
data.work1$SCHEDULED_DEPARTURE <- as.integer(data.work1$SCHEDULED_DEPARTURE)
data.work1$SCHEDULED_ARRIVAL <- as.integer(data.work1$SCHEDULED_ARRIVAL)
#data.work1$ARRIVAL_DELAY_BUCKETED <- as.integer(data.work1$ARRIVAL_DELAY_BUCKETED)
data.work1
str(data.work1)

set.seed(1000)
train=sample(nrow(data.work1),0.7*nrow(data.work1))

data.train <- data.work1[train,]
library("rpart")
library("ROSE")
data.over.train=ovun.sample(ARRIVAL_DELAY_BUCKETED~.,data.train, p=0.5)
data.valid <-data.work1[-train,]

str(data.train$SCHEDULED_ARRIVAL)
str(data.valid$SCHEDULED_ARRIVAL)
data.train$ARRIVAL_DELAY_BUCKETED=as.integer(data.train$ARRIVAL_DELAY_BUCKETED)
data.valid$ARRIVAL_DELAY_BUCKETED=as.integer(data.valid$ARRIVAL_DELAY_BUCKETED)


#Regression tree
#install.packages("tree")
#install.packages("MASS")
library(MASS)
library(tree)
data.train$ARRIVAL_DELAY_BUCKETED=as.integer(data.train$ARRIVAL_DELAY_BUCKETED)
tree.regression = tree(ARRIVAL_DELAY_BUCKETED~.,data= data.train)
summary(tree.regression)
tree.regression
plot(tree.regression)
text(tree.regression,pretty=0)

set.seed(12345)
cv.regression = cv.tree(tree.regression)
names(cv.regression)
cv.regression
plot(cv.regression$size,cv.regression$dev,type = "b")

prune.regression = prune.tree(tree.regression,best=8)
plot(prune.regression)
text(prune.regression,pretty=0)

pred.prune.regression = predict(prune.regression,data.valid)
pred.prune.regression
plot(pred.prune.regression,data.valid$ARRIVAL_DELAY)
abline(0,1)
mean((pred.prune.regression-data.valid$ARRIVAL_DELAY)^2)


#Bagging
#install.packages("randomForest")
library(randomForest)
# We first do bagging (which is just RF with m = p)
set.seed(12345)
bag=randomForest(ARRIVAL_DELAY~.,data=data.train,mtry=4,importance=TRUE)
bag
yhat.bag = predict(bag,newdata=data.valid)
yhat.bag
plot(yhat.bag, data.valid$ARRIVAL_DELAY)
abline(0,1)
mean((yhat.bag-data.valid$ARRIVAL_DELAY)^2)
importance(bag)
varImpPlot(bag)