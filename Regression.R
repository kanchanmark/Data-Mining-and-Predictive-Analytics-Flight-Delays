file1=read.csv("C:/Users/DELL/Downloads/UMCP/Data Mining and Predictive Analytics/flights1.csv")
head(file1)
data.work1=file1

#data.work1$ARRIVAL_DELAY_BUCKETED <- data.work1$ARRIVAL_DELAY/15
#data.work1$ARRIVAL_DELAY_BUCKETED <- as.integer(data.work1$ARRIVAL_DELAY_BUCKETED) 
data.work1$ARRIVAL_DELAY_BUCKETED <- ifelse(data.work1$ARRIVAL_DELAY_BUCKETED <= 0, 0, data.work1$ARRIVAL_DELAY_BUCKETED)

str(data.work1)


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


#data.work1$SCHEDULED_DEPARTURE <- as.factor(data.work1$SCHEDULED_DEPARTURE)
#data.work1$SCHEDULED_ARRIVAL <- as.factor(data.work1$SCHEDULED_ARRIVAL)
#levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))
#data.work1$ARRIVAL_DELAY_BUCKETED <- as.factor(data.work1$ARRIVAL_DELAY_BUCKETED)

data.work1=na.omit(data.work1)
data.work1$SCHEDULED_DEPARTURE <- as.integer(data.work1$SCHEDULED_DEPARTURE)
data.work1$SCHEDULED_ARRIVAL <- as.integer(data.work1$SCHEDULED_ARRIVAL)
data.work1$ARRIVAL_DELAY_BUCKETED <- as.integer(data.work1$ARRIVAL_DELAY_BUCKETED)
#data.work1
str(data.work1)

set.seed(12356)
  train=sample(nrow(data.work1),0.7*nrow(data.work1))
  
  data.train <- data.work1[train,]
  data.valid <-data.work1[-train,]
  
  #str(data.train$SCHEDULED_ARRIVAL)
  #str(data.valid$SCHEDULED_ARRIVAL)
  
  
  fit1=lm(ARRIVAL_DELAY_BUCKETED~.,data = data.train)
  
  summary(fit1)
  
  actual.linear=data.valid$ARRIVAL_DELAY_BUCKETED
  pred.linear=predict(fit1, newdata=data.valid)
  
  Metrics <- c("AE","RMSE","MAE","SSE","MSE")
  x1 <- mean(actual.linear - pred.linear)
  x2 <- sqrt(mean((actual.linear - pred.linear)^2))
  x3 <- mean(abs(actual.linear - pred.linear))
  x4 <- sum(actual.linear - pred.linear)^2
  x5 <- mean((actual.linear - pred.linear)^2)
  Values <- c(x1,x2,x3,x4,x5)
  x <- data.frame(Metrics,Values)
  x
  
  
  #FORWARD
  #install.packages("leaps")
  library(leaps)
  regfit.fwd=regsubsets(ARRIVAL_DELAY_BUCKETED~., data.train, method="forward")
  summary(regfit.fwd)
  
  #regfit.fwd=regsubsets(ARRIVAL_DELAY_BUCKETED~., data.train, nvmax = 14,method="forward")
  regfit.summaryfwd = summary(regfit.fwd)
  names(regfit.summaryfwd)
  regfit.summaryfwd$rsq
  
  regfit.summaryfwd$bic
  which.min(regfit.summaryfwd$bic)
  plot(regfit.summaryfwd$bic,xlab="Number of variables",ylab="BIC",type='l')
  points(9,regfit.summaryfwd$bic[9],col="red",cex=2,pch=20)
  
  plot(regfit.summaryfwd$rss,xlab="Number of variables",ylab="RSS",type='l')
  
  regfit.summaryfwd$adjr2
  which.max(regfit.summaryfwd$adjr2)
  plot(regfit.summaryfwd$adjr2,xlab="Number of variables",ylab="Adjusted R2",type='l')
  points(9,regfit.summaryfwd$adjr2[9],col="red",cex=2,pch=20)
  
  regfit.summaryfwd$cp
  which.min(regfit.summaryfwd$cp)
  plot(regfit.summaryfwd$cp,xlab="Number of variables",ylab="Cp",type='l')
  points(9,regfit.summaryfwd$cp[9],col="red",cex=2,pch=20)
  
  plot(regfit.fwd,scale="r2")
  plot(regfit.fwd,scale="adjr2")
  plot(regfit.fwd,scale="bic")
  
  coef(regfit.fwd,9)
  
  
  #BACKWARD
  library(leaps)
  regfit.bwd=regsubsets(ARRIVAL_DELAY_BUCKETED~., data.train, method="backward")
  summary(regfit.bwd)
  
  #regfit.bwd=regsubsets(ARRIVAL_DELAY_BUCKETED~., data.train, nvmax = 14,method="backward")
  regfit.summaryback = summary(regfit.bwd)
  names(regfit.summaryback)
  regfit.summaryback$rsq
  
  regfit.summaryback$bic
  which.min(regfit.summaryback$bic)
  plot(regfit.summaryback$bic,xlab="Number of variables",ylab="BIC",type='l')
  points(9,regfit.summaryback$bic[9],col="red",cex=2,pch=20)
  
  plot(regfit.summaryback$rss,xlab="Number of variables",ylab="RSS",type='l')
  
  regfit.summaryback$adjr2
  which.max(regfit.summaryback$adjr2)
  plot(regfit.summaryback$adjr2,xlab="Number of variables",ylab="Adjusted R2",type='l')
  points(9,regfit.summaryback$adjr2[9],col="red",cex=2,pch=20)
  
  regfit.summaryback$cp
  which.min(regfit.summaryback$cp)
  plot(regfit.summaryback$cp,xlab="Number of variables",ylab="Cp",type='l')
  points(9,regfit.summaryback$cp[9],col="red",cex=2,pch=20)
  
  plot(regfit.bwd,scale="r2")
  plot(regfit.bwd,scale="adjr2")
  plot(regfit.bwd,scale="bic")
  
  coef(regfit.bwd,9)
  
  #BEST SUBSET
  set.seed(12356)
  regfit.best=regsubsets(ARRIVAL_DELAY_BUCKETED~.,data=data.train,really.big = T)
  summary(regfit.best)
  test.mat=model.matrix(ARRIVAL_DELAY_BUCKETED~.,data=data.valid)
  val.errors=rep(NA,14)
  for(i in 1:14){
    coefi=coef(regfit.best,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    val.errors[i]=mean((data.valid$ARRIVAL_DELAY-pred)^2)
  }
  val.errors
  which.min(val.errors)
  
  coef(regfit.best,9)
  
  bestsubsetmodel <- lm(ARRIVAL_DELAY_BUCKETED~DAY_OF_WEEK+AIRLINE+ORIGIN_AIRPORT+SCHEDULED_DEPARTURE+SCHEDULED_TIME+ELAPSED_TIME+AIR_TIME, data=data.train)
  predict_test <- predict(bestsubsetmodel, newdata = data.valid)
  actual <- data.valid$ARRIVAL_DELAY
  Metrics <- c("AE","RMSE","MAE","SSE","MSE")
  x1 <- mean(actual - predict_test)
  x2 <- sqrt(mean((actual - predict_test)^2))
  x3 <- mean(abs(actual - predict_test))
  x4 <- sum(actual - predict_test)^2
  x5 <- mean((actual - predict_test)^2)
  Values <- c(x1,x2,x3,x4,x5)
  x <- data.frame(Metrics,Values)
  x
  
  #Lasso regression
  set.seed(12356)
  x = model.matrix(ARRIVAL_DELAY_BUCKETED~.,data.train)[,-1]
  y = data.train$ARRIVAL_DELAY_BUCKETED
  #install.packages("glmnet")
  library(glmnet)
  grid = 10^seq(10,-2,length=100)
  lasso.mod = glmnet(x,y,alpha=1,lambda=grid)
  plot(lasso.mod)
  
  cv.out = cv.glmnet(x,y,alpha=1)
  plot(cv.out)
  
  bestlam=cv.out$lambda.min
  bestlam
  
  lasso.pred = predict(lasso.mod,s=bestlam,newx=x[1:nrow(data.valid),])
  actual.lasso = data.valid$ARRIVAL_DELAY_BUCKETED
  mean((actual.lasso - lasso.pred)^2)
  
  out=glmnet(x,y,alpha=1,lambda=grid)
  lasso.coef=predict(out,type="coefficients",s=bestlam)
  lasso.coef
  lasso.coef[lasso.coef!=0]
  
  
  #Ridge regression
  set.seed(12356)
  x = model.matrix(ARRIVAL_DELAY_BUCKETED~.,data.train)[,-1]
  y = data.train$ARRIVAL_DELAY_BUCKETED
  #install.packages("glmnet")
  library(glmnet)
  grid = 10^seq(10,-2,length=100)
  ridge.mod = glmnet(x,y,alpha=0,lambda=grid)
  plot(ridge.mod)
  
  cv.out = cv.glmnet(x,y,alpha=0)
  plot(cv.out)
  
  bestlam=cv.out$lambda.min
  bestlam
  
  ridge.pred = predict(ridge.mod,s=bestlam,newx=x[1:nrow(data.valid),])
  actual.ridge = data.valid$ARRIVAL_DELAY_BUCKETED
  mean((actual.ridge - ridge.pred)^2)
  
  out=glmnet(x,y,alpha=0,lambda=grid)
  ridge.coef=predict(out,type="coefficients",s=bestlam)
  ridge.coef
  ridge.coef[ridge.coef!=0]
  
  #Regression tree
  #install.packages("tree")
  #install.packages("MASS")
  library(MASS)
  library(tree)
  tree.regression = tree (ARRIVAL_DELAY_BUCKETED~.,data.train)
  summary(tree.regression)
  tree.regression
  plot(tree.regression)
  text(tree.regression,pretty=0)
  
  set.seed(12356)
  cv.regression = cv.tree(tree.regression)
  names(cv.regression)
  cv.regression
  plot(cv.regression$size,cv.regression$dev,type = "b")

prune.regression = prune.tree(tree.regression,best=3)
plot(prune.regression)
text(prune.regression,pretty=0)

pred.prune.regression = predict(prune.regression,data.valid)
pred.prune.regression
plot(pred.prune.regression,data.valid$ARRIVAL_DELAY_BUCKETED)
abline(0,1)
mean((pred.prune.regression-data.valid$ARRIVAL_DELAY_BUCKETED)^2)


