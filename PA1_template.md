# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

First, the dataset is unzipped if the csv file is not present.  Then the data
is stored in a variable called fitdata.  The date column is converted from a 
factor to a date class.


```r
if (!file.exists("activity.csv")){
        unzip("activity.zip")
}
fitdata <- read.csv("activity.csv")
fitdata$date <- as.Date(fitdata$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

First totals were calculated of the steps variable by date, ignoring missing 
values, and stored into the sums variable.  A histogram of the totals was 
created.


```r
sums <- aggregate(fitdata$steps, by = list(fitdata$date), sum, na.rm = TRUE)
library(ggplot2)
ggplot(sums, aes(x)) + 
        geom_histogram(bins = 5, col = "steelblue", fill = "turquoise") +
        labs(title = "Total Steps per Day") +
        labs(x = "Total Steps") +
        labs(y = "Number of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Then the mean and medians are calculated.


```r
mean(sums[ ,2])
```

```
## [1] 9354.23
```

```r
median(sums[ ,2])
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
means <- aggregate(fitdata$steps, list(fitdata$interval), mean, na.rm = TRUE)
colnames(means) <- c("Interval", "Steps")
ggplot(means) +
        geom_line(aes(Interval, Steps),col = "indianred") +
        labs(y = "Average Number of Steps") + 
        labs(x = "5-minute Interval") +
        labs(title = "Average Daily Activity")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
