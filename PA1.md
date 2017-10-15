Loading and preprocessing the data
----------------------------------

1.  Loading the data

<!-- -->

    library(knitr)
    setwd("~/Dropbox/coursera/ReproducibleResearch/Week2")

    active_data <- read.csv("activity.csv")
    str(active_data)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

1.  Converting date from factor -&gt; Date

<!-- -->

    active_data$date <- as.Date(as.character(active_data$date))

What is mean total number of steps taken per day (missing values ignored)?
--------------------------------------------------------------------------

1.  Calculate the total number of steps taken per day

<!-- -->

    tot_step_day <- tapply(active_data$steps, active_data$date, sum, na.rm = TRUE)

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    hist(tot_step_day, breaks =10, xlab = "Total Number of Steps per Day", main = "Histogram of Total Steps per Day")

![](PA1_files/figure-markdown_strict/histogram-1.png)

1.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    mean_steps <- mean(tot_step_day)
    median_steps <- median(tot_step_day)

The mean number of steps is 9354.2295082 and the median is 10395 per day

What is the average daily activity pattern?
-------------------------------------------

1.  Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute
    interval (x-axis) and the average number of steps taken, averaged
    across all days (y-axis)

<!-- -->

    interval_steps <- aggregate(list(steps = active_data$steps), by = list(intervals = active_data$interval), FUN = mean, na.rm = TRUE)

    plot(interval_steps$intervals, interval_steps$steps, type = "l", main = "Mean Number of Steps per Time Interval Across all Days", xlab = "5min Time Intervals", ylab = "Mean No of Steps")

![](PA1_files/figure-markdown_strict/timeseries-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    max_steps <- which.max(interval_steps$steps)
    max_interval <- interval_steps$intervals[max_steps]

The interval with the highest number of steps on average is 835

Imputing missing values
-----------------------

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with 𝙽𝙰s)

<!-- -->

    tot_nas <- sum(is.na(active_data$steps))

The total number of missing values in the dataset is 2304

1.  Devise a strategy for filling in all of the missing values in the
    dataset. Values will be substituted using the mean for that interval

2.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

    library("Hmisc")

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, round.POSIXt, trunc.POSIXt, units

    active_data_new <- active_data
    active_data_new$steps <- impute(active_data$steps, FUN = mean)

1.  Make a histogram of the total number of steps taken each day and
    calculate and report the mean and median total number of steps taken
    per day.

<!-- -->

    hist(active_data_new$steps, breaks =10, xlab = "Total Number of Steps per Day", main = "Histogram of Total Steps per Day (Missing Replaced)")

![](PA1_files/figure-markdown_strict/newhistogram-1.png)

    mean_replace <- mean(active_data_new$steps)
    median_replace <- median(active_data_new$steps)

The mean value of steps per day with the missing values replaced is
32.4799636 and the median is 0

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    active_data_new$weekdays <- as.factor(ifelse(weekdays(active_data_new$date)=="Saturday" | weekdays(active_data_new$date)=="Sunday", "weekend", "weekday" ))

1.  Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).

<!-- -->

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:Hmisc':
    ## 
    ##     combine, src, summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    week_steps <- active_data_new %>%
      group_by(interval, weekdays) %>%
      summarise(new_steps = mean(steps))

    week_plot <- ggplot(week_steps, aes(x = interval, y = new_steps, col = weekdays)) +
                          geom_line() +
                          facet_wrap(~weekdays, ncol = 1, nrow=2)
                          
    print(week_plot)

![](PA1_files/figure-markdown_strict/weekdayplot-1.png)
