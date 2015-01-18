# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Load the libraries and load data.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
activity_ori <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity <- tbl_df(activity_ori)
activity <- mutate(activity, date = ymd(date))
```


## What is mean total number of steps taken per day?

Histagram of the steps:

```r
hist(activity$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Mean of the steps is:

```r
mean(activity$steps, na.rm = TRUE)
```

```
## [1] 37.3826
```

Median of the steps is:

```r
median(activity$steps, na.rm = TRUE)
```

```
## [1] 0
```


## What is the average daily activity pattern?

Get average daily activity pattern by grouping the data by interval. And calculate the mean by the groups.


```r
activity_int <- group_by(activity, interval)
y <- summarise(activity_int, value = mean(steps, na.rm = TRUE))
plot(y, type = 'l', ylab = "Mean of steps", xlab = "5-minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Get the interval whcih contains max average steps

```r
maxStepsIndex <- which(y$value == max(y$value))
y$interval[maxStepsIndex]
```

```
## [1] 835
```


## Imputing missing values

The number of missing values are:

```r
sum(is.na(activity))
```

```
## [1] 2304
```

Fill in the missing steps data with mean value of that interval.

```r
## get data contain NA
activity_fill <- activity_ori
size <- dim(activity)[1]
for (i in 1: size) {
  if (is.na(activity_fill[i, 1])) {
    currNAInterval <- activity_fill[i, 3]
    currMeanVal <- y$value[ y$interval == currNAInterval]
    activity_fill[i, 1] <- currMeanVal
  }
}
```

Then plot the histogram of the total number of steps taken each day. We will group the data by date, can calculate the sum steps of all days.


```r
activity_fill_date <- group_by(activity_fill, date)
total_steps_date <- summarise(activity_fill_date, total_steps = sum(steps))
hist(total_steps_date$total_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

The mean of the total number of steps taken each day:


```r
mean(total_steps_date$total_steps)
```

```
## [1] 10766.19
```

The median of the total number of steps taken each day:


```r
median(total_steps_date$total_steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

Add weekday or weekend factor into data, and geoup by interval and weekday type.


```r
activity_week <- mutate(activity, ww = weekdays(date), ww = (ww == "Sunday" | ww =="Saturday"), ww = ifelse(ww, "weekend", "weekday"), ww = as.factor(ww))

activity_week_int <- group_by(activity_week, interval, ww)

weekdata_summ <- summarise(activity_week_int, steps_sum = mean(steps, na.rm = TRUE))
```

Then plot the data by weekday or weekend:

```r
par(mfrow = c(2,1), mar = c(4,4,2,1))
plot(weekdata_summ[weekdata_summ$ww == "weekday", c(1,3)], type = 'l', main = "weekday")
plot(weekdata_summ[weekdata_summ$ww == "weekend", c(1,3)], type = 'l', main = "weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

