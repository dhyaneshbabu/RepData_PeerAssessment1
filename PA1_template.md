# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(ggplot2)
library(scales)
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```r
# Load the data file into a data frame
activity <- read.csv("activity.csv", as.is = TRUE)

# Remove the NA values and store in a separate structure for future use
good_activity <- activity[complete.cases(activity), ]
```


## What is mean total number of steps taken per day?

```r
steps_Day <- tapply(good_activity$steps, good_activity$date, sum, na.rm=TRUE)
qplot(steps_Day, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


```r
stepsByDayMean <- mean(steps_Day)
stepsByDayMedian <- median(steps_Day)
print(stepsByDayMean)
```

```
## [1] 10766.19
```

```r
print(stepsByDayMedian)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

    1 .Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avg_steps_per_interval <- aggregate(steps ~ interval, good_activity, mean)
plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type='l', col=1, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
interval_idx <- which.max(avg_steps_per_interval$steps)

# Identify the specific interval and the average steps for that interval
print (paste("Interval highest avg steps is ", avg_steps_per_interval[interval_idx, ]$interval, " and the no of steps  ", round(avg_steps_per_interval[interval_idx, ]$steps, digits = 1)))
```

```
## [1] "Interval highest avg steps is  835  and the no of steps   206.2"
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

    Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    Create a new dataset that is equal to the original dataset but with the missing data filled in.
    Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
    

```r
numMissingValues <- length(which(is.na(activity$steps)))
print(numMissingValues)
```

```
## [1] 2304
```

```r
activity_Imputed <- activity
activity_Imputed$steps <- impute(activity$steps, fun=mean)

stepsByDayImputed <- tapply(activity_Imputed$steps, activity_Imputed$date, sum)
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)
print(stepsByDayMeanImputed)
```

```
## [1] 10766.19
```

```r
print(stepsByDayMedianImputed)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

   1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
   2.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
    

```r
activity_Imputed$dateType <-  ifelse(as.POSIXlt(activity_Imputed$date)$wday %in% c(0,6), 'Weekend', 'Weekday')

averagedActivityDataImputed <- aggregate(steps ~ interval + dateType, data=activity_Imputed, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5 minutes interval") + 
    ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
