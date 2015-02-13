setwd("~/Data Mining/R/Course-ReproducableResearch")
library(plyr)
library(ggplot2)
#
#Fetch and load Tracker Data
#
#myactiv<-read.csv("activity.csv")
working <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",working)
myactiv <- read.csv(unz(working, "activity.csv"))
unlink(working)
#
#
# Calculate Total Number of Steps per day
daysum<-ddply(myactiv,.(date),summarize,totstep=sum(steps))

#
#Distribution of total Daily Steps
#
hist(daysum$totstep,10,col="blue",xlab="Total Steps per Day",main="Histogram of Tracker Total Steps Per Day")
#
#Median and mean number of steps per day
#
raw_med<-median(daysum$totstep,na.rm=T)
raw_mean<-mean(daysum$totstep,na.rm=T)
raw_med
raw_mean
#
#Mean steps per interval
#
meanint<-ddply(myactiv,.(interval),summarise,meanstep=mean(steps,na.rm=T))
plot(meanint$interval,meanint$meanstep,type="l")
#
#CALCULATE interval with max ave value and mark on plot
#

maxint<-meanint[meanint$meanstep==max(meanint$meanstep),1]
maxval<-meanint[meanint$meanstep==max(meanint$meanstep),2]
abline(v=maxint,col="red")
abline(h=maxval,col="blue")
text(maxint+15,maxval-25, paste("Max Average # Steps occurs in interval ",maxint," \nand is ", 
                                round(maxval,2),"steps"), col = "gray50", adj = c(0, 0))
#
# Max int
#
maxint
#
#Replace NA values with imputed values - in this case replace NAs with
#the mean value  for that interval. In this sample NAs occur for a whole day
#The mean is rounded to the nearest integer
#
myactiv1<-myactiv
myactiv1[which(is.na(myactiv1$steps)),1]<-round(meanint[,2],0)
#
#Calc new histogram of total steps per day
#
daysum1<-ddply(myactiv1,.(date),summarize,totstep=sum(steps))
#
#Distribution of total Daily Steps
#
hist(daysum1$totstep,10,col="blue",xlab="Total Steps per Day",main="Histogram of Tracker Total Steps Per Day with Imputed Data")
#
#Median and mean number of steps per day
#
imp_med<-median(daysum1$totstep,na.rm=T)
imp_mean<-mean(daysum1$totstep,na.rm=T)
imp_med
imp_mean
#
#Change in mean and median
#
raw_med-imp_med
raw_mean-imp_mean

#
#Increase in total number of steps
#
sum(daysum1$totstep)/sum(daysum$totstep,na.rm=T)
#
#Add new column to imputed data frame to indicate weekday, weekend
#
myactiv1$daytype<-as.factor(ifelse(tolower(weekdays(strptime(myactiv$date,format="%Y-%m-%d")))
                                   %in% c("sunday","saturday"),"weekend","weekday"))
#
#find means per interval by day type
#
meanint1<-ddply(myactiv1,.(interval,daytype),summarise,meanstep=mean(steps))


a<-ggplot(meanint1,aes(x=interval,y=meanstep)) + geom_line(aes(color=daytype))+facet_wrap(~daytype,nrow=2,ncol=1)
a
