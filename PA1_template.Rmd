---
title: "Assignment 1 for Reproducible Research"
output: html_document
---

First of all we're going to read data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Data could be downloaded from the [dataset](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

Reading data into **data** variable.

```{r, echo=TRUE}
library(ggplot2)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
download.file(url, temp)
unzip(temp, "activity.csv")
data <- read.csv("activity.csv", as.is=TRUE)
```

**Display total number of steps taken each day**

1. Remove NA values from the data.
2. Calculate number of steps each day and plot it

```{r, echo=TRUE}
data_na <- na.omit(data)
ggplot(data = data_na, aes(date, steps)) + stat_summary(fun.y = sum, geom="bar") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

**Mean and median number of steps taken each day**

```{r, echo=TRUE}
ggplot(data = data_na, aes(date, steps)) + stat_summary(fun.y = mean, geom="point", col="red") + stat_summary(fun.y = median, geom="point", col="green") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

**Time series plot of the average number of steps taken**
Calculating the nuber of steps done at each interval and then plot it.
```{r, echo=TRUE}
steps_per_interval = aggregate(data_na$steps, by=list(interval = data_na$interval), FUN=mean)
plot(steps_per_interval, type = "l", ylab = "steps", xlab = "interval", col="blue")
```

**The 5-minute interval that, on average, contains the maximum number of steps**
```{r, echo=TRUE}
colnames(steps_per_interval) <- c("interval", "steps")
steps_per_interval[which.max(steps_per_interval$steps), ]
```

**Code to describe and show a strategy for imputing missing data**

Get count of measurements with the missing steps. We going to use for this the original dataset which includes NA values.
```{r, echo=TRUE}
missing<-is.na(data$steps)
sum(missing)
```

Then we going to replace NA values with the average steps count for the same interval. This data is available at **steps_per_interval** variable.
```{r, echo=TRUE}
data_no_miss <- data
data_no_miss$steps<- ifelse(is.na(data_no_miss$steps),steps_per_interval$steps[match(data_no_miss$interval,steps_per_interval$interval)],data_no_miss$steps)
```

**Histogram of the total number of steps taken each day after missing values are imputed**
```{r, echo=TRUE}
ggplot(data = data_no_miss, aes(date, steps)) + stat_summary(fun.y = sum, geom="bar") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

**Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends**

Preprocess the original data.
```{r, echo=TRUE}
data_no_miss$date <- as.Date(data$date, format = "%Y-%m-%d")
data_no_miss$interval <- as.factor(data$interval)
```

Calculate mean value of steps done each day of the week.
```{r, echo=TRUE}
weekdays_steps <- function(data) {
    weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),
                          FUN=mean, na.rm=T)
    weekdays_steps$interval <- 
            as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
    colnames(weekdays_steps) <- c("interval", "steps")
    weekdays_steps
}
```

Below is a function which determine does day below to Weekend or a Weekday. And then apply that information as an additional column.
```{r, echo=TRUE}
data_by_weekdays <- function(data) {
    data$weekday <- 
            as.factor(weekdays(data$date)) 
    weekend <- subset(data, weekday %in% c("Saturday","Sunday"))
    weekday <- subset(data, !weekday %in% c("Saturday","Sunday"))

    weekend_steps <- weekdays_steps(weekend)
    weekday_steps <- weekdays_steps(weekday)

    weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
    weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))

    data_by_weekdays <- rbind(weekend_steps, weekday_steps)
    data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
    data_by_weekdays
}
```

Append weekdays to the dataset without missing values.
```{r, echo=TRUE}
data <- data_by_weekdays(data_no_miss)
```

Plot two graphs to comapre the data collected for Weekend and Weekdays
```{r, echo=TRUE}
ggplot(data, aes(x=interval, y=steps)) +  geom_line(col="blue") + facet_wrap(~ dayofweek, nrow=2, ncol=1) 
```
