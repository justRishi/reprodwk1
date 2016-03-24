# Reproducible Research: Peer Assessment 1

## Loading required libraries

```r
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
#library(reshape2)
#library(zoo)
```


## Loading and preprocessing the data

```r
#setwd("C:/Users/R/OneDrive/coursera/reproducibleResearch/week1Assignment")

if(!exists("activity")){
  unzip("activity.zip")
  activity<- read.csv("activity.csv")
}

activity$date<-ymd(as.character(activity$date))
```


## What is mean total number of steps taken per day? 
(See mean and median below the code block output of the variables is included inline in the sentence, See Rmd file for proof ;) )

```r
activityByDay<- group_by(activity[,c(1:2)],date) %>%summarise(sum(steps))
names(activityByDay)<-c("date","totalSteps")

totalStepsaDay <- ggplot(activityByDay, aes(x=totalSteps)) +geom_histogram(bins = 30)  +labs(x="Steps")+ ggtitle(" histogram of the total number of steps taken each day")
print(totalStepsaDay)
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

```r
meanTotalDays <- mean(activityByDay$totalSteps,na.rm = TRUE)
medianTotalDays <- median(activityByDay$totalSteps,na.rm=TRUE)
```

Mean Total steps taken a day: 10766  
Median Total steps taken a day: 10765  



## What is the average daily activity pattern?
See below the time serie of the interval averages over all days. (see max. interval below the code block)

```r
activityByInterval<- group_by(activity[,-2],interval) %>%summarise(mean(steps,na.rm=TRUE))
names(activityByInterval)<- c("interval", "stepMean")

IntervalMean <- ggplot(data=activityByInterval,aes(x=interval,y=stepMean)) + geom_line(alpha=.3) +ggtitle("average number of steps taken, averaged across all days")
print(IntervalMean)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

```r
maxInterval <- filter(activityByInterval,stepMean==max(activityByInterval$stepMean, na.rm = TRUE))
```
The 5 minute inteval of the day with the most steps is: 835, 206  


## Imputing missing values  
First I tried for replace Na's with the function "na.locf" of the zoo package, it replaces a na with a nearest value,  
which most of the time is a 0, so not good, finally I used the suggested solution from the instruction,  
replace a NA in a day for a mean of that day. I had some inspiration from slashdot ([link  to slashdot resource ](http://stackoverflow.com/questions/9322773/how-to-replace-na-with-mean-by-subset-in-r-impute-with-plyr)).    
  
Eyeballing the histogram the frequence for some is now a bit higher but when calculating the means of the totals of the data frame with NA and from the data frame without NA I could hardly see a  difference. (see number of NA, new median and mean below the code block)

```r
NumberOfNA <- sum(!complete.cases(activity))

impute.mean <- function(x) replace(x, is.na(x), mean(x,na.rm=TRUE))
activity<- group_by(activity,interval) %>%mutate(steps =impute.mean(steps))

NumberOfNA_afterReplacement <- sum(!complete.cases(activity))

activityByDayNoNA<- group_by(activity,date) %>%summarise(sum(steps))
names(activityByDayNoNA)<-c("date","totalSteps")
totalStepsaDay <- ggplot(activityByDayNoNA, aes(x=totalSteps)) +geom_histogram(bins = 30)  +labs(x="Steps")  + ggtitle(" histogram of the total number of steps taken each day")
print(totalStepsaDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

```r
# 
 meanNoNATotalDays <- mean(activityByDayNoNA$totalSteps)
 medianNoNATotalDays <- median(activityByDayNoNA$totalSteps)
```
Mean Total steps taken a day: 10766  
Median Total steps taken a day: 10766  
Number if NA before replacing them: 2304  
Numer of NA after replacing them: 0  
  
  
## Are there differences in activity patterns between weekdays and weekends?
See below the two time series of steps during weekdays and steps during weekends,  
there is more activity(more steps) early in the day during weekdays than during weekends.

```r
#add factor
isweekend<-ifelse(wday(activity$date) %in% c(7,1),1,0)
activity$kindOfDay<-factor(isweekend,levels = c(0,1),labels=c("weekday","weekend"))

#create data frame interval over weekdays
weekdayActivityByInterval <- filter(activity,kindOfDay=="weekday")
weekdayActivityByInterval<- group_by(weekdayActivityByInterval[,-2],interval) %>%summarise(mean(steps[steps>0],na.rm=TRUE),first(kindOfDay))
names(weekdayActivityByInterval)<- c("interval", "stepMean","kindOfDay")

#create data frame interval over weekends
weekendActivityByInterval <- filter(activity,kindOfDay=="weekend")
weekendActivityByInterval<- group_by(weekendActivityByInterval[,-2],interval) %>%summarise(mean(steps[steps>0],na.rm=TRUE),first(kindOfDay))
names(weekendActivityByInterval)<- c("interval", "stepMean","kindOfDay")

#merge weekdays and weekends data frames
weekActivityByInterval <-rbind(weekendActivityByInterval,weekdayActivityByInterval)

#plotting
dayIntervalMean <- ggplot(data=weekActivityByInterval,aes(x=interval,y=stepMean)) + geom_line()  +
                  facet_grid(kindOfDay ~ .)
print(dayIntervalMean)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)
