Reproducible Research
==========================================================================

Peer Review Assignment 1


```r
library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
```
Load and read data into r


```r
activity <- read_csv("activity.csv")
head(activity)
```

**What is mean total number of steps taken per day?**
1.Calculate the total number of steps taken per day


```r
steps_perday<-group_by(activity,date) %>% summarise(totalsteps = sum(steps),mean = mean(steps))
steps_perday
```

```
## # A tibble: 61 x 3
##    date       totalsteps   mean
##    <date>          <dbl>  <dbl>
##  1 2012-10-01         NA NA    
##  2 2012-10-02        126  0.438
##  3 2012-10-03      11352 39.4  
##  4 2012-10-04      12116 42.1  
##  5 2012-10-05      13294 46.2  
##  6 2012-10-06      15420 53.5  
##  7 2012-10-07      11015 38.2  
##  8 2012-10-08         NA NA    
##  9 2012-10-09      12811 44.5  
## 10 2012-10-10       9900 34.4  
## # ... with 51 more rows
```

2.histogram of the total number of steps taken each day


```r
ggplot(steps_perday,aes(totalsteps,color = "fff")) + geom_histogram(bins = 5,show.legend = FALSE)
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk unnamed-chunk-29](figure/unnamed-chunk-29-1.png)

3.Calculate and report the mean and median of the total number of steps taken per day


```r
group_by(activity,date) %>% summarise(mean = mean(steps),median = median(steps,na.rm = TRUE))
```

```
## # A tibble: 61 x 3
##    date         mean median
##    <date>      <dbl>  <dbl>
##  1 2012-10-01 NA         NA
##  2 2012-10-02  0.438      0
##  3 2012-10-03 39.4        0
##  4 2012-10-04 42.1        0
##  5 2012-10-05 46.2        0
##  6 2012-10-06 53.5        0
##  7 2012-10-07 38.2        0
##  8 2012-10-08 NA         NA
##  9 2012-10-09 44.5        0
## 10 2012-10-10 34.4        0
## # ... with 51 more rows
```

**What is the average daily activity pattern?**

1.Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
ggplot(activity,aes(steps,date,type = "l")) + geom_line()
```

```
## Warning: Removed 2304 row(s) containing missing values
## (geom_path).
```

![plot of chunk unnamed-chunk-31](figure/unnamed-chunk-31-1.png)

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
arrange(activity,desc(steps)) %>% head(n =1)
```

```
## # A tibble: 1 x 3
##   steps date       interval
##   <dbl> <date>        <dbl>
## 1   806 2012-11-27      615
```

**Imputing missing values**
1.calculate and report the total number of missing values in the dataset


```r
sum(!complete.cases(activity$steps))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
activity1<-copy(activity)
activity1$steps <- ifelse(is.na(activity1$steps), mean(activity1$steps, na.rm=TRUE), activity1$steps)
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
head(activity1)
```

```
## # A tibble: 6 x 3
##   steps date       interval
##   <dbl> <date>        <dbl>
## 1  37.4 2012-10-01        0
## 2  37.4 2012-10-01        5
## 3  37.4 2012-10-01       10
## 4  37.4 2012-10-01       15
## 5  37.4 2012-10-01       20
## 6  37.4 2012-10-01       25
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
x<-group_by(activity1,date) %>% summarise(total_steps = sum(steps),mean = mean(steps),median = median(steps))
ggplot(x,aes(total_steps,color = "fff")) + geom_histogram(bins = 5,show.legend = FALSE)
```

![plot of chunk unnamed-chunk-36](figure/unnamed-chunk-36-1.png)

**For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.**

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity1$date<-as.Date(activity1$date)
activity1$day<-ifelse(weekdays(activity1$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activity1$day<-as.factor(activity1$day)
```

2.Make a panel plot containing a time series plot type = "l" of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
ggplot(activity1,aes(interval,steps,type = "l")) + geom_line() + facet_grid(day~.)
```

![plot of chunk unnamed-chunk-38](figure/unnamed-chunk-38-1.png)

