# Reproducible Research: Peer Assessment 1

## Loading required libraries

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(lubridate)
library(data.table)
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     hour, mday, month, quarter, wday, week, yday, year
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, last
```

```r
library(reshape2)
```

```
## 
## Attaching package: 'reshape2'
```

```
## The following objects are masked from 'package:data.table':
## 
##     dcast, melt
```

```r
library(zoo)
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```


## Loading and preprocessing the data

```r
setwd("C:/Users/R/OneDrive/coursera/reproducibleResearch/week1Assignment")

#if(!exists("activity")){
   # unzip("activity.zip")
  activity<- read.csv("activity.csv")
#}

activity$date<-ymd(as.character(activity$date))
```


## What is mean total number of steps taken per day?  
(ignored for median and mean values of 0)

```r
activityByDay<- group_by(activity[,c(1:2)],date) %>%summarise(median(steps[steps>0]),
                                                              mean(steps[steps>0]),
                                                              sum(steps))

names(activityByDay)<-c("date","steps-Mean","steps-Median","steps")

totalStepsaDay <- ggplot(activityByDay, aes(x=date, weights=steps)) + 
  geom_bar()+labs(y="total of steps a day")
print(totalStepsaDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

```r
activityByDay.melt<- melt(activityByDay[,c(1:3)], id.vars="date",variable.name ="Aggregate", value.name="steps")

MeanAndMediumADay <-ggplot(data=activityByDay.melt,aes(date,steps,col=Aggregate)) + geom_point(alpha=0.3) + scale_color_manual(values = c("steps-Mean" = 'red','steps-Median' = 'blue')) 
   
print(MeanAndMediumADay)
```

```
## Warning: Removed 16 rows containing missing values (geom_point).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-2.png)


## What is the average daily activity pattern?

```r
activityByInterval<- group_by(activity[,-2],interval) %>%summarise(mean(steps[steps>0],na.rm=TRUE))
names(activityByInterval)<- c("interval", "stepMean")
 
IntervalMean <- ggplot(data=activityByInterval,aes(x=interval,y=stepMean)) + geom_point(alpha=.3)
print(IntervalMean)
```

```
## Warning: Removed 19 rows containing missing values (geom_point).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

```r
maxInterval <- filter(activityByInterval,stepMean==max(activityByInterval$stepMean, na.rm = TRUE))
maxInterval
```

```
## Source: local data frame [1 x 2]
## 
##   interval stepMean
##      (int)    (dbl)
## 1      835 352.4839
```


## Imputing missing values

```r
NumberOfNA <- sum(is.na(activity$steps))
NumberOfNA
```

```
## [1] 2304
```

```r
activity$steps<-na.locf(activity$steps,na.rm = F,fromLast = F, rule=2)
activity$steps<-na.locf(activity$steps,na.rm = F,fromLast = T,rule=2)
NumberOfNA_afterReplacement <- sum(is.na(activity$steps))
NumberOfNA_afterReplacement
```

```
## [1] 0
```

```r
activityByDayNoNA<- group_by(activity[,c(1:2)],date) %>%summarise(median(steps[steps>0]),
                                                              mean(steps[steps>0]),
                                                              sum(steps))

names(activityByDayNoNA)<-c("date","steps-Mean2","steps-Median2","steps2")

totalStepsaDay <- ggplot(activityByDayNoNA, aes(x=date, weights=steps2)) + 
  geom_bar()+labs(y="total of steps a day")
print(totalStepsaDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

```r
activityByDayNoNA.melt<- melt(activityByDayNoNA[,c(1:3)], id.vars="date",variable.name ="Aggregate", value.name="steps2")

MeanAndMediumADay <-ggplot(data=activityByDayNoNA.melt,aes(date,steps2,col=Aggregate)) + geom_point(alpha=0.3) + scale_color_manual(values = c("steps-Mean2" = 'red','steps-Median2' = 'blue')) 
   
print(MeanAndMediumADay)
```

```
## Warning: Removed 16 rows containing missing values (geom_point).
```

![](PA1_template_files/figure-html/unnamed-chunk-5-2.png)

```r
averages_With_Na_replaced<-summarise_each(activityByDayNoNA[complete.cases(activityByDayNoNA),][,c(-1)],funs(mean))
averages_With_Na_not_replaced<-summarise_each(activityByDay[complete.cases(activityByDay),][,c(-1)],funs(mean))

averages_With_Na_not_replaced
```

```
## Source: local data frame [1 x 3]
## 
##   steps-Mean steps-Median    steps
##        (dbl)        (dbl)    (dbl)
## 1    56.5566     129.7411 10766.19
```

```r
averages_With_Na_replaced
```

```
## Source: local data frame [1 x 3]
## 
##   steps-Mean2 steps-Median2   steps2
##         (dbl)         (dbl)    (dbl)
## 1     56.5566      129.7411 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?
