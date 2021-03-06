# PA 1 Repreducible Research
Analysis of Fitness Tracker Data
================================

###Prepared by Skamanrev 10 Feb 2015

###Summary
In this analysis data a from a fitness tracker is analyzed over a period of 61 days

Only 53 of the days contain valid tracker data. 8 Days do not

Initially the analysis will investigate the  data ignoring the missing data. 
In this phase the following will be investigated

1 The mean and median total number of steps per day  
2 The average daily activity pattern (averaged over all days)

In the Second phase data points will be imputed for the missing data and the steps above will 
be recomputed to see what effect the imputed data has on 

1 the mean and median  
2 the total number of steps

Finally using the imputed data the differences between weekend and weekday activity will be analyzed 

###Fetch and load Tracker Data

```r
working <- tempfile()
##setInternet2(TRUE) #This is required in knitr to make download work on Windows ->source stackoverflow
##download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",working)
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",working,method="curl") ##This for macs
myactiv <- read.csv(unz(working, "activity.csv"))
unlink(working)
```
### What is mean total number of steps taken per day?

```r
library(plyr)
daysum<-ddply(myactiv,.(date),summarize,totstep=sum(steps)) #calc number or steps per day
```


```r
hist(daysum$totstep,10,col="blue",xlab="Total Steps per Day",main="Histogram of Tracker Total Steps Per Day")
```

![](PA1_template_files/figure-html/dailysteps-1.png) 

####Median and mean number of steps per day

```r
raw_med<-median(daysum$totstep,na.rm=T)
raw_mean<-mean(daysum$totstep,na.rm=T)
```

The median number of steps is

```r
raw_med
```

```
## [1] 10765
```
and the mean is

```r
raw_mean
```

```
## [1] 10766.19
```

### What is the average daily activity pattern?  

```r
#calculate the mean number of steps per interval across all days
meanint<-ddply(myactiv,.(interval),summarise,meanstep=mean(steps,na.rm=T)) 
plot(meanint$interval,meanint$meanstep,type="l",xlab="Interval",ylab="Average # of steps")
#CALCULATE interval with max ave value and mark on plot
maxint<-meanint[meanint$meanstep==max(meanint$meanstep),1]
maxval<-meanint[meanint$meanstep==max(meanint$meanstep),2]
abline(v=maxint,col="red")
abline(h=maxval,col="blue")
text(maxint+15,maxval-25, paste("Max Average # Steps occurs in interval ",maxint," \nand is ", 
                                round(maxval,2),"steps"), col = "gray50", adj = c(0, 0))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

####The Interval with the highest average number of steps is

```r
maxint
```

```
## [1] 835
```

#### Imputing missing values

To account for the missing values in the data set, data can be imputed to replace the missing values.  

Since we have just calculated the average number of steps per time interval we can replace any data missing from a time interval
with the average value for that interval. This method is used in the analysis below


```r
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
```

![](PA1_template_files/figure-html/imputed_graph-1.png) 

```r
imp_med<-median(daysum1$totstep,na.rm=T)
imp_mean<-mean(daysum1$totstep,na.rm=T)
```
(Note this section uses inline R code to display results - this does print in md docs see source Rmd file for code)

The change in mean and median caused by using imputed data is 

Data Type     | Mean         |Median      |
------------- | -------------|------------|
Raw           |10766.19|10765 |   
Imputed       |10765.64 |10762|
Difference    |0.549335|3|

Including imputed values has no real inpact on the mean or median

The total number of steps increased 1.15 times


#### Are there differences in activity patterns between weekdays and weekends?


```r
#Add new column to imputed data frame to indicate weekday, weekend
#
myactiv1$daytype<-as.factor(ifelse(tolower(weekdays(strptime(myactiv$date,format="%Y-%m-%d")))
                                   %in% c("sunday","saturday"),"weekend","weekday"))
#find means per interval by day type
meanint1<-ddply(myactiv1,.(interval,daytype),summarise,meanstep=mean(steps))
#create plot
library(ggplot2)
a<-ggplot(meanint1,aes(x=interval,y=meanstep)) + geom_line(aes(color=daytype))+facet_wrap(~daytype,nrow=2,ncol=1)+
  labs(y="Mean # of Steps")+
  labs(title="Weekend and Weekday Activity Patterns")
a
```

![](PA1_template_files/figure-html/weekdays-1.png) 

A better stragtegy for imputing data would be to use average steps per interval for weekend days in in weekend missing data and 
average steps per interval for weekdays in in weekday missing data.
