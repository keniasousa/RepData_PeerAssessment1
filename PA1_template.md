# Analysis of Activity Monitoring Data
Kenia Sousa  
17 May 2015  



This is the analysis of data from a personal activity monitoring device. This device collects data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and includes the number of steps taken in 5 minute intervals each day.

The activity monitoring dataset is available in this repository in the file activity.zip.
The file needs to be unzipped and saved in the working directory.

The analysis starts by loading the data:


```r
activity <- read.csv("activity.csv", sep = ",", na.strings="NA") 
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA).

- date: The date on which the measurement was taken in YYYY-MM-DD format.

- interval: Identifier for the 5-minute interval in which measurement was taken.

## What is the mean of the total number of steps taken per day?

The first step is to calculate the total number of steps taken per day, ignoring the missing values. 


```r
activity_no_na <- activity[complete.cases(activity$steps),]
library(dplyr)
day <- group_by(activity_no_na,date)
stepsByDay <- summarize(day,
                        sum = sum(as.numeric(as.character(steps)), na.rm=F) )
```

Followed by calculating the mean and median of the total number of steps taken per day. 


```r
meanMedianStepsByDay <- summarize(stepsByDay,
                                  mean = mean(as.numeric(as.character(sum)), na.rm=TRUE) ,
                                  median = median(as.numeric(as.character(sum)), na.rm=TRUE) )
library(reshape2)
meanMedian <- melt(meanMedianStepsByDay, measure.vars=c("mean","median"))
meanMedian
```

```
##   variable    value
## 1     mean 10766.19
## 2   median 10765.00
```

Then, make a histogram of the total number of steps taken each day. This plot also shows the calculated mean and median of the total number of steps taken per day.


```r
library(ggplot2)
ggplot(stepsByDay, aes(x=sum)) + geom_histogram(aes(fill=..count..), binwidth=1000)+ 
    geom_vline(data=meanMedian,aes(xintercept=value, color=variable, linetype=variable), show_guide = TRUE)+
    ggtitle("Total number of steps taken per day")+
    scale_y_continuous(name="Frequency")+
    scale_x_continuous(name="Total number of steps")  
```

![](figure/histogram_na-1.png) 

## What is the average daily activity pattern?

First, group the data by 5-minute interval and calculate the average number of steps taken across all days.


```r
interval <- group_by(activity,interval)
averageStepsbyInterval <- summarize(interval,
                          average = mean(as.numeric(as.character(steps)), 
                                    na.rm=TRUE) )
```

Calculate which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:
 

```r
  max <- averageStepsbyInterval[ averageStepsbyInterval$average == max( averageStepsbyInterval$average ) , ]
  max
```

```
## Source: local data frame [1 x 2]
## 
##   interval  average
## 1      835 206.1698
```

Then make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). And show with the red dot the 5-minute interval, on average across all the days, that contains the maximum number of steps:
   

```r
 ggplot(averageStepsbyInterval, aes(interval, average)) + geom_line() +
    xlab("5-minute interval") + ylab("Average number of steps")+
    ggtitle("Average number of steps taken per interval across all days")+
    geom_point(data = max, colour = "red", size=4)+
    geom_text(aes(label=ifelse(interval == max$interval,max$interval,'')),hjust=0,just=0)
```

![](figure/time_series-1.png) 

# Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA), that were ignored in the analysis up to now. The presence of missing days may introduce bias into some calculations or summaries of the data.
To calculate the total number of missing values in the dataset, get the set of rows with NAs, here is an extract:


```r
activity_na <- activity[!complete.cases(activity$steps),]
head(activity_na)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Here is the total number of missing values in the dataset:


```r
nr_na <- nrow(activity_na)
nr_na
```

```
## [1] 2304
```

The strategy for filling in all of the missing values in the dataset is to use the mean for that 5-minute interval.
To get the mean of the 5-minute interval, first list the rows of data without missing values, group by interval and then calculate mean of steps by interval:
  

```r
  activity <- activity[complete.cases(activity$steps),]
  interval <- group_by(activity,interval)
  stepsByInterval <- summarize(interval,
                                 mean = mean(as.numeric(as.character(steps)), na.rm=FALSE) )
```

Then, substitute the steps with missing values using the mean of the same 5 minute-interval and create a new dataset that is equal to the original dataset but with the missing data filled in:


```r
activity_na$steps <-  stepsByInterval$mean[match(activity_na$interval,stepsByInterval$interval)]
activity_full <- rbind(activity,activity_na)
```

Here is an example of steps that had missing values substituted by the mean of the same 5 minute-interval:


```r
head(filter(activity_full, date == "2012-10-01"))
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

To make a histogram of the total number of steps taken each day, first calculate the total number of steps taken per day. Then, calculate the mean and median total number of steps taken per day:


```r
day_no_na <- group_by(activity_full,date)
stepsByDay_no_na <- summarize(day_no_na,
                        sum = sum(as.numeric(as.character(steps)), na.rm=F) )
  
  meanMedianStepsByDay_no_na <- summarize(stepsByDay_no_na,
                                          mean = mean(as.numeric(as.character(sum)), na.rm=TRUE) ,
                                          median = median(as.numeric(as.character(sum)), na.rm=TRUE) )
  library(reshape2)
  mean_median_no_na <- melt(meanMedianStepsByDay_no_na, measure.vars=c("mean","median"))
```

Then, make a histogram of the total number of steps taken each day. This plot also shows the calculated mean and median of the total number of steps taken per day:


```r
ggplot(stepsByDay_no_na, aes(x=sum)) + geom_histogram(aes(fill=..count..), binwidth=1000)+ 
    geom_vline(data=mean_median_no_na,aes(xintercept=value, color=variable, linetype=variable), show_guide = TRUE)+
    ggtitle("Total number of steps taken per day without missing values")+
    scale_y_continuous(name="Frequency")+
    scale_x_continuous(name="Total number of steps")  
```

![](figure/histogram_full-1.png) 

Do these values differ from the estimates from the first part of the assignment? 
Here is the calculated mean and median for the dataset with missing values:

```r
meanMedian
```

```
##   variable    value
## 1     mean 10766.19
## 2   median 10765.00
```
Here is the calculated mean and median for the dataset without missing values:

```r
  mean_median_no_na
```

```
##   variable    value
## 1     mean 10766.19
## 2   median 10766.19
```

What is the impact of imputing missing data on the estimates of the total daily number of steps?
The data that was initially with missing values was not depicted in the first histogram. Now that the data has value from the mean of the 5-minute interval, it is added to the plot.
Here is the data of total number of steps by day that was added to the plot:

```r
  day_na <- group_by(activity_na,date)
  stepsByDay_na <- summarize(day_na,
                             sum = sum(as.numeric(as.character(steps)), na.rm=F) )
  stepsByDay_na
```

```
## Source: local data frame [8 x 2]
## 
##         date      sum
## 1 2012-10-01 10766.19
## 2 2012-10-08 10766.19
## 3 2012-11-01 10766.19
## 4 2012-11-04 10766.19
## 5 2012-11-09 10766.19
## 6 2012-11-10 10766.19
## 7 2012-11-14 10766.19
## 8 2012-11-30 10766.19
```

## Are there differences in activity patterns between weekdays and weekends? 

This part of the analysis uses the dataset with the filled-in missing values.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity_full$week <- ifelse(weekdays(as.Date(activity_full$date)) %in% c("Saturday", "Sunday"),"weekend","weekday")
activity_full$week <- factor(activity_full$week)
```

Here is an extract of the dataset with the new variable for “weekday” and “weekend”:


```r
head(activity_full)
```

```
##     steps       date interval    week
## 289     0 2012-10-02        0 weekday
## 290     0 2012-10-02        5 weekday
## 291     0 2012-10-02       10 weekday
## 292     0 2012-10-02       15 weekday
## 293     0 2012-10-02       20 weekday
## 294     0 2012-10-02       25 weekday
```

Calculate the average number of steps taken, averaged across all days.


```r
interval_week_no_na <- group_by(activity_full,interval,week)
averageStepsbyIntervalWeek_no_na <- summarize(interval_week_no_na,
                                              average = mean(as.numeric(as.character(steps)), na.rm=TRUE) )
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
ggplot(averageStepsbyIntervalWeek_no_na, aes(interval, average)) + geom_line() + facet_grid(. ~ week)+
    xlab("5-minute interval") + ylab("Average number of steps")+
    ggtitle("Average number of steps taken per interval across all days")
```

![](figure/panel-1.png) 
