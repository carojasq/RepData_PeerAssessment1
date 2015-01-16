# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
steps_by_date <- aggregate(steps ~ date, data = data, FUN = sum)
steps_by_interval <- aggregate(steps ~ interval, data = data, FUN = sum)
```


## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day


```r
barplot(steps_by_date$steps, names.arg = steps_by_date$date, xlab = "Date", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
mdn<- median(steps_by_date$steps)
steps_median<- mean(steps_by_date$steps)
```

Median is : 10765, mean is: 1.0766189\times 10^{4} .

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
plot(steps_by_interval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



```r
max_steps = max(steps_by_interval$steps)
best_interval <- steps_by_interval[steps_by_interval$steps == max_steps,]$interval
```

The interval with more steps was 835.


## Inputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
n <- sum(!complete.cases(data))
```
The number of not complete cases (rows witht NAs) are: 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

In this case we will use the median to fill the NA values

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
new_data <- data
new_data$steps[is.na(new_data$steps)] <- steps_median
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
filled_steps_by_date <- aggregate(steps ~ date, data = new_data, FUN = sum)
mean_filled <- mean(filled_steps_by_date$steps)
median_filled <- median(filled_steps_by_date$steps)
barplot(filled_steps_by_date$steps, names.arg = filled_steps_by_date$date, xlab = "Date", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 
New median is 1.1458\times 10^{4} and mean is 4.1599847\times 10^{5}


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
data$daytype <- as.factor(sapply(data$date, daytype))
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
par(mfrow = c(2, 1))

for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = data, subset = data$daytype == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 
