data <- read.csv("E:/S/Coursera/DS-JHU/5Reproducible Research/assignment 2/activity.csv", header = T, sep=",")

summary(data)

str(data)

dim(data)

head(data)

data1 <- tapply(data$steps,data$date,sum)
hist(data1,xlab="Numbers of steps taken per day",main="Histogram of the total number of steps taken per day")

library("dplyr")
data2 <- filter(data,!is.na(data$steps))
data22 <- aggregate(data=data2,steps ~ interval,mean)
plot(data22$interval,data22$steps,type="l",xlab="5-minute interval", ylab="Average number of steps taken")

sum(!is.na(data))

data3 <- data
na_datarows <- is.na(data3$steps)
data33 <- tapply(data3$steps,data3$interval,mean,na.rm=T)

data3$steps[na_datarows] <- data33[as.character(data3$interval[na_datarows])]
head(data3)

data333 <- tapply(data3$steps,data3$date,sum)
hist(data333,xlab="Mean & median total number of steps taken per day",main="Histogram of the total number of steps taken per day")
head(data3)

data4 <- data3
data4$week_type<-data4$date
head(data4)
data4$date <- as.Date(data4$date)
data4$week_type <- as.Date(data4$week_type)
weekend <- function(actdate) {ifelse(weekdays(actdate) == "ĞÇÆÚÁù" | weekdays(actdate) == "ĞÇÆÚÈÕ","weekend","weekday")}
data4 <- mutate(data4, week_type = weekend(data4$date))
head(data4)

data44 <- aggregate(data4$steps,list(data4$interval, data4$week_type),sum)
head(data44)
library(lattice)
xyplot(x~Group.1|Group.2,data=data44,layout=c(1,2),type="l",xlab="5-minute interval",ylab="Steps")