Introduction
------------

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

The data for this assignment is downloaded from the course web site

The variables included in this dataset are:

-   steps: Number of steps taking in a 5-minute interval (missing values
    are coded as 𝙽𝙰)
-   date: The date on which the measurement was taken in YYYY-MM-DD
    format
-   interval: Identifier for the 5-minute interval in which measurement
    was taken

### Loading the required libraries

Load the required libraries to plot the data

    library(data.table)
    library(ggplot2)

### Code for reading in the dataset and/or processing the data

1.  Reading the data

<!-- -->

    activityData <- read.csv("activity.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")
    str(activityData)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

1.  Processing the data for suitable format

<!-- -->

    activityData$date <- as.Date(activityData$date, "%Y-%m-%d")
    activity <- activityData[complete.cases(activityData), ]
    str(activity)

    ## 'data.frame':    15264 obs. of  3 variables:
    ##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ date    : Date, format: "2012-10-02" "2012-10-02" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

### Histogram of the total number of steps taken each day

1.  To calculate the sum of total number of steps taken per day, we use
    aggregate function.

<!-- -->

    StepsDay <- aggregate(steps~date, activity, FUN = sum)
    head(StepsDay)

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

1.  Creating an histogram for number of steps taken each day using
    `hist` function

<!-- -->

    hist(StepsDay$steps, xlab = "Total number of steps taken per day", col = "green", main = "Histogram of the total number of steps taken each day" )

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

1.  Mean and median number of steps taken each day

Taking mean and median of steps taken per day using `mean` and `median`
function.

    firstMean <- mean(StepsDay$steps)
    firstMedian <- median(StepsDay$steps)
    firstMean

    ## [1] 10766.19

    firstMedian

    ## [1] 10765

### Time series plot of the average number of steps taken

1.  To subset the average number of steps taken in every intervals
    of time.

<!-- -->

    Interval_steps <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)
    colnames(Interval_steps) <- list("intervals", "steps")
    head(Interval_steps)

    ##   intervals     steps
    ## 1         0 1.7169811
    ## 2         5 0.3396226
    ## 3        10 0.1320755
    ## 4        15 0.1509434
    ## 5        20 0.0754717
    ## 6        25 2.0943396

1.  Plotting the average number of steps taken in every interval

<!-- -->

    ggplot(data = Interval_steps, aes(x= intervals, y=steps))+geom_line(col = "red")+xlab("Intervals")+ylab("Number of steps taken")+ggtitle("Time series plot of the average number of steps taken")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

1.  The 5-minute interval that, on average, contains the maximum number
    of steps

<!-- -->

    Interval_steps[Interval_steps$steps == max(Interval_steps$steps), ]

    ##     intervals    steps
    ## 104       835 206.1698

1.  To clear the workspace

<!-- -->

    rm(Interval_steps)

### Code to describe and show a strategy for imputing missing data

    activityData[is.na(activityData$steps), .N]

    ## data frame with 0 columns and 2304 rows

1.  Devise a strategy for filling in all of the missing values in
    the dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

<!-- -->

    library(Hmisc)

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, round.POSIXt, trunc.POSIXt, units

    Dataimputed <- activityData
    Dataimputed$steps <- impute(activityData$steps, fun = mean)
    head(Dataimputed)

    ##     steps       date interval
    ## 1 37.3826 2012-10-01        0
    ## 2 37.3826 2012-10-01        5
    ## 3 37.3826 2012-10-01       10
    ## 4 37.3826 2012-10-01       15
    ## 5 37.3826 2012-10-01       20
    ## 6 37.3826 2012-10-01       25

1.  Make a of total histogram number of steps taken each now

<!-- -->

    StepstakenPerday <- aggregate(Dataimputed$steps, by=list(Dataimputed$date), FUN = sum)
    colnames(StepstakenPerday) <- c("date", "steps")
    hist(StepstakenPerday$steps, col = "blue", main = "Total steps taken each day after imputing missing values in data",xlab = "Number of steps taken each day" )

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)

1.  Mean and median of the total number of steps taken per each day
    after imputing missing values

<!-- -->

    secondMean <- mean(StepstakenPerday$steps)
    secondMedian <- median(StepstakenPerday$steps)
    secondMean

    ## [1] 10766.19

    secondMedian

    ## [1] 10766.19

1.  To calculate the difference between first Mean & median(number of
    steps taken each day/ without NA) and second mean & median(Number of
    steps taken each day after imputing missing values)

<!-- -->

    DifferenceMean = secondMean - firstMean
    DifferenceMedian = secondMedian - firstMedian
    DifferenceMean

    ## [1] 0

    DifferenceMedian

    ## [1] 1.188679

1.  Total difference between steps taken per each day

<!-- -->

    Difference = sum(StepstakenPerday$steps) - sum(StepsDay$steps) 
    # The total difference between imputed data and non imputed data is 
    Difference

    ## [1] 86129.51

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day

<!-- -->

    Dataimputed$date <- as.Date(Dataimputed$date, "%Y-%m-%d")
    Dataimputed <- data.frame(date = Dataimputed$date, weekday = tolower(weekdays(Dataimputed$date)), steps = Dataimputed$steps, interval = Dataimputed$interval)
    Dataimputed <- cbind(Dataimputed, daytype = ifelse(Dataimputed$weekday == "saturday" | Dataimputed$weekday == "sunday", "weekend", "weekday"))
    head(Dataimputed)

    ##         date weekday   steps interval daytype
    ## 1 2012-10-01  monday 37.3826        0 weekday
    ## 2 2012-10-01  monday 37.3826        5 weekday
    ## 3 2012-10-01  monday 37.3826       10 weekday
    ## 4 2012-10-01  monday 37.3826       15 weekday
    ## 5 2012-10-01  monday 37.3826       20 weekday
    ## 6 2012-10-01  monday 37.3826       25 weekday

1.  Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).

<!-- -->

    Dataimputed <- aggregate(Dataimputed$steps, by = list(Dataimputed$weekday, Dataimputed$daytype, Dataimputed$interval), mean)
    colnames(Dataimputed) <- c("weekday", "daytype", "interval", "meanSteps")
    head(Dataimputed)

    ##     weekday daytype interval meanSteps
    ## 1    friday weekday        0  8.307244
    ## 2    monday weekday        0  9.418355
    ## 3  thursday weekday        0  9.375844
    ## 4   tuesday weekday        0  0.000000
    ## 5 wednesday weekday        0  7.931400
    ## 6  saturday weekend        0  4.672825

Plotting the graph

    ggplot(Dataimputed, aes(interval, meanSteps, color = daytype))+geom_line()+facet_grid(daytype ~ .)+xlab("5 min interval")+ylab("Average steps taken")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-19-1.png)

To clear the work space

    rm(firstMedian, firstMean, activity, Dataimputed, StepstakenPerday,StepsDay, Difference, DifferenceMedian, DifferenceMean)
