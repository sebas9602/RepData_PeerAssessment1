---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
The data is download from the web and un zip. Then is load in to R.

```r
if(!dir.exists("./data"))dir.create("data") # check if the directorio data exist and if not, create one
url<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 
if(is.na(grep("infozip",dir("./data"))[1]))download.file(url = url,destfile = "./data/infozip.zip")#validate thereis zip file o dowloadit
unzip(zipfile = "./data/infozip.zip",exdir = "./data")#extract teh data

data<-read.csv("./data/activity.csv")
```
A summarize of data:


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
## What is mean total number of steps taken per day?
histogram of the total number of steps taken each day

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.6.3
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
data<- group_by(data,date)
v<-summarise(data, "sum steps per day"=sum(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
with(v,hist(`sum steps per day`,))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

the mean is shown below:

```r
mean(v$`sum steps per day`)
```

```
## [1] 9354.23
```

the median is shown below:

```r
median(v$`sum steps per day`)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
A time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) is shown below:

```r
data<- group_by(data,interval)
mean5<- summarise(data, "Average steps 5 minutes intervals across days"=mean(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
with(mean5,plot(interval, `Average steps 5 minutes intervals across days`,type = 'l'))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
the 5-minute interval with more steps is shown below:

```r
m<-max(mean5$`Average steps 5 minutes intervals across days`)
mean5$interval[mean5$`Average steps 5 minutes intervals across days`==m]
```

```
## [1] 835
```

1. The number of missing values in the data set is:

```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

2-3. The NA values of steps variable are chaged to the average of that interval. A new
data set is created.

```r
dataf<-data
for (i in 1:length(dataf$steps)){
     if(is.na(dataf$steps[i])){
          inter<-dataf$interval[i]
          dataf$steps[i]= mean5$`Average steps 5 minutes intervals across days`[mean5$interval==inter]
     }
}
```
4. histogram of the total number of steps taken each day 

```r
dataf<- group_by(dataf,date)
v<-summarise(dataf, "sum steps per day"=sum(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
with(v,hist(`sum steps per day`,))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
the mean and median total number of steps taken per day is calculated.
the mean is shown below:

```r
mean(v$`sum steps per day`)
```

```
## [1] 10766.19
```

the median is shown below:

```r
median(v$`sum steps per day`)
```

```
## [1] 10766.19
```

comparing the first and second histogram it can be concluded that the values differ. the filling of NA decrease the number of days with 5000 or les steps. 

## Are there differences in activity patterns between weekdays and weekends?

A new variable called week_day is created to indicate if the day is weekend or not.

```r
datad<-dataf
datad$date<- as.POSIXct(datad$date)
days<-weekdays(datad$date)
datad$week_day<-0
datad$week_day[days=="domingo"|days=="sabado"]="weekend"
datad$week_day[!(days=="domingo"|days=="sabado")]="weekday"
datad$week_day<-as.factor(datad$week_day)
```

A plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis), is created.

```r
datad<- group_by(datad,interval,week_day)
datad$steps<- as.numeric(datad$steps)
dayplot<-summarise(datad, "Average of steps"=mean(steps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.6.3
```

```r
qplot(interval, `Average of steps`,data=dayplot,geom = "line")+geom_line()+facet_wrap(~week_day,ncol = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

