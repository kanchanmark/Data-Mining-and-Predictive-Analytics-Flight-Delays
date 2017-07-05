file1=read.csv("flights.csv")
head(file1)
data.work1=file1

data.work1$DIVERTED=NULL
data.work1$CANCELLED=NULL
data.work1$CANCELLATION_REASON=NULL
data.work1$YEAR=NULL
data.work1$TAIL_NUMBER=NULL
data.work1$FLIGHT_NUMBER=NULL
data.work1$DAY=NULL
data.work1[,7:12]=NULL
data.work1$AIR_TIME=NULL
data.work1[,7:9]=NULL
data.work1[,10:14]=NULL
data.work1$ARRIVAL_DELAY_BUCKETED <- ifelse(data.work1$ARRIVAL_DELAY<= 0, 0, data.work1$ARRIVAL_DELAY)

data.work1$SCHEDULED_ARRIVAL=data.work1$SCHEDULED_ARRIVAL/100
data.work1$SCHEDULED_ARRIVAL=round(data.work1$SCHEDULED_ARRIVAL)

data.work1$SCHEDULED_DEPARTURE=data.work1$SCHEDULED_DEPARTURE/100
data.work1$SCHEDULED_DEPARTURE=round(data.work1$SCHEDULED_DEPARTURE)
data.work1$ARRIVAL_DELAY=NULL
data.work1$ARRIVAL_TIME=NULL
data.work1$FINAL2=data.work1$ARRIVAL_DELAY_BUCKETED/120
data.work1$FINAL2 <- ifelse(data.work1$FINAL2<= 1, 0, 1)

data.work1=na.omit(data.work1)

data.work1=data.frame(lapply(data.work1, as.factor))


library("dummies")
#data.work1= dummy.data.frame(data.work1, names=c("DAY_OF_WEEK", "AIRLINE", "ORIGIN_AIRPORT","DESTINATION_AIRPORT", "SCHEDULED_DEPARTURE", "SCHEDULED_ARRIVAL"), sep=",")


library("arules")
rules <- apriori(data=data.work1, parameter = list(supp=0.001, conf=0.01),
                 appearance = list(default="lhs", rhs="FINAL2=1"),
                 control = list(verbose=F))
rules <- sort(rules, decreasing = TRUE, by="confidence")
inspect(rules[1:15])
library("arulesViz")
plot(rules, measure=c("support","lift"), shading="confidence")
