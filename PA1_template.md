Reproducible Research - Peer-graded Assignment: Course Project 1
================================================================

Load the needed libraries
-------------------------

    library(ggplot2)
    library(data.table)
    library(plyr)
    library(timeDate)

    ## Warning: package 'timeDate' was built under R version 3.3.2

(1) Loading and preprocessing the data
--------------------------------------

    activityData <- read.csv("data/activity.csv")

### (2) What is mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day

    totalStepsPerDay <- aggregate(steps ~ date, data=activityData, FUN=sum)

    ggplot(data=totalStepsPerDay, aes(totalStepsPerDay$steps)) + 
        geom_histogram(binwidth = 1000,
                       col="red", 
                       fill="blue", 
                       alpha = .2) + 
        labs(title="Histogram of Total Number of Steps Taken Each Day") +
        labs(x="Steps", y="Count")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

### 2. Calculate and report the mean and median of the total number of steps taken per day

    mean(totalStepsPerDay$steps, na.rm=TRUE) 

    ## [1] 10766.19

    median(totalStepsPerDay$steps, na.rm=TRUE) 

    ## [1] 10765

(3) What is the average daily activity pattern?
-----------------------------------------------

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    avgStepsPerInterval <- aggregate(steps ~ interval, data=activityData, FUN=mean, na.rm = TRUE)

    ggplot(data=avgStepsPerInterval, aes(x=interval, y=steps)) + 
     geom_line(col = "red") +
        labs(title="Average Daily Activity Pattern") +
        labs(x="5-Minute Interval", y="Average Number of Steps Taken")    

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    avgStepsPerInterval[which.max(avgStepsPerInterval$steps),]

    ##     interval    steps
    ## 104      835 206.1698

(4) Imputing missing values
---------------------------

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

    NAData <- is.na(activityData$steps) 

    table(NAData) 

    ## NAData
    ## FALSE  TRUE 
    ## 15264  2304

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc

    imputeMean <- function(steps, interval) {
        if (is.na(steps)) 
            mean(activityData[activityData$interval==interval, "steps"], na.rm = TRUE)
        else
            steps
    }

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

    activityDataFixed <- activityData
    activityDataFixed$steps <- mapply(imputeMean, activityDataFixed$steps, activityDataFixed$interval)

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

    totalStepsPerDay <- aggregate(steps ~ date, data=activityDataFixed, FUN=sum)

    ggplot(data=totalStepsPerDay, aes(totalStepsPerDay$steps)) + 
        geom_histogram(binwidth = 1000,
                       col="red", 
                       fill="blue", 
                       alpha = .2) + 
        labs(title="Histogram of Total Number of Steps Taken Each Day") +
        labs(x="Steps", y="Count")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    mean(totalStepsPerDay$steps, na.rm=TRUE) 

    ## [1] 10766.19

    median(totalStepsPerDay$steps, na.rm=TRUE) 

    ## [1] 10766.19

(5) Are there differences in activity patterns between weekdays and weekends?
-----------------------------------------------------------------------------

### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

    detWDay <- function(date) {
        if (isWeekend(as.Date(date), wday = 1:5)) 'Weekend' else 'Weekday'
    }

    activityDataFixed$Wday <- mapply(detWDay, activityDataFixed$date)

### 2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute

### interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

    avgStepsPerIntervalWday <- aggregate(steps ~ interval + Wday, data=activityDataFixed, FUN=mean)

    ggplot(data=avgStepsPerIntervalWday, aes(x=interval, y=steps)) + 
        geom_line(col = "red") +
        facet_grid(Wday ~ .) + 
        labs(title="Average Daily Activity Pattern") +
        labs(x="5-Minute Interval", y="Average Number of Steps Taken")    

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)
