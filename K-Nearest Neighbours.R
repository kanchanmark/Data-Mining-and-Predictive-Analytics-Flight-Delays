file1=read.csv("flights1.csv")
head(file1)
data.work1=file1

data.work1=na.omit(data.work1)

data.work1$ARRIVAL_DELAY_BUCKETED <- ifelse(data.work1$ARRIVAL_DELAY_BUCKETED <= 0, 0, data.work1$ARRIVAL_DELAY_BUCKETED)
data.work1$FINAL2=ifelse(data.work1$ARRIVAL_DELAY_BUCKETED <= 8, 0, 1)
data.work1$FINAL4=ifelse(data.work1$ARRIVAL_DELAY_BUCKETED <= 16, 0, 1)

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
data.work1$TAXI_OUT=NULL
data.work1$TAXI_IN=NULL
data.work1$ELAPSED_TIME=NULL

data.work1$SCHEDULED_DEPARTURE <- as.factor(data.work1$SCHEDULED_DEPARTURE)
data.work1$SCHEDULED_ARRIVAL <- as.factor(data.work1$SCHEDULED_ARRIVAL)

data.work2=data.work1
data.work1$FINAL4=NULL
data.work2$FINAL2=NULL
data.work1$FINAL2=as.factor(data.work1$FINAL2)
data.work2$FINAL4=as.factor(data.work2$FINAL4)




set.seed(1000)
train=sample(nrow(data.work1),0.7*nrow(data.work1))

data.train <- data.work1[train,]
data.valid <-data.work1[-train,]
data.validx <- data.work2[-train,]
data.validy <- data.work1[-train,]

set.seed(1000)
data.train <- data.work1[train,]
library("rpart")
library("ROSE")
data.over.train=ovun.sample(FINAL2~.,data.train, p=0.5,  method ="over")$data
data.valid <-data.work1[-train,]

data.rose <- ROSE(FINAL2 ~ ., data = data.train, seed = 1)$data
table(data.rose$FINAL2)

### Data Synthesis ##
library(tree)
tree.final1 = rpart(FINAL2~.,data.rose)
summary(tree.final1)

tree.final1
pred.full.final1 = predict(tree.final1,data.valid,type="class") 
pred.full.final1
classification_matrix_full_rose <- table(data.valid$FINAL2,pred.full.final1)
classification_matrix_full_rose
accuracy.full_rose <- (classification_matrix_full_rose[1,1] + classification_matrix_full_rose[2,2]) / sum(classification_matrix_full_rose)
accuracy.full_rose

### Over Sampling ##
library(tree)
tree.final2 = rpart(FINAL2~.,data.over.train)
summary(tree.final2)

tree.final2
pred.full.final2 = predict(tree.final2,data.valid,type="class") 
pred.full.final2
classification_matrix_full <- table(data.valid$FINAL2,pred.full.final2)
classification_matrix_full
accuracy.full <- (classification_matrix_full[1,1] + classification_matrix_full[2,2]) / sum(classification_matrix_full)
accuracy.full


#4 hour delay

set.seed(1000)
train=sample(nrow(data.work1),0.7*nrow(data.work1))

data.train4 <- data.work2[train,]
data.valid4 <-data.work2[-train,]

set.seed(1000)
data.train4 <- data.work2[train,]
library("rpart")
library("ROSE")
data.over.train4=ovun.sample(FINAL4~.,data.train4, p=0.3,  method ="over")$data
data.valid <-data.work2[-train,]

data.rose <- ROSE(FINAL4 ~ ., data = data.over.train4, seed = 1)$data
table(data.rose$FINAL4)

### Data Synthesis ##
library(tree)
tree.final1 = rpart(FINAL4~.,data.rose)
summary(tree.final1)

tree.final1
pred.full.final14 = predict(tree.final1,data.valid,type="class") 
pred.full.final14
classification_matrix_full_rose <- table(data.valid$FINAL4,pred.full.final14)
table(data.valid$FINAL4,pred.full.final14)
classification_matrix_full_rose
accuracy.full_rose <- (classification_matrix_full_rose[1,1] + classification_matrix_full_rose[2,2]) / sum(classification_matrix_full_rose)
accuracy.full_rose

### Over Sampling ##
library(tree)
tree.final24 = rpart(FINAL4~.,data.over.train4)
summary(tree.final24)

tree.final24
pred.full.final24 = predict(tree.final24,data.valid,type="class") 
pred.full.final24
classification_matrix_full <- table(data.valid$FINAL4,pred.full.final2)
classification_matrix_full
accuracy.full <- (classification_matrix_full[1,1] + classification_matrix_full[2,2]) / sum(classification_matrix_full)
accuracy.full










