---
title: "Reproducible Research Course Project 1"
output: html_document
---
1) Preparation/What is mean total number of steps taken per day?


```
library(knitr)
opts_chunk$set(echo = TRUE)
library(dplyr)
library(lubridate)
library(ggplot2)
data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character", "integer"))
data$date <- ymd(data$date)
str(data)
head(data)

steps <- data%>%
filter(!is.na(steps)) %>%
group_by(date) %>%
summarize(steps = sum(steps)) %>%
print

ggplot(steps, aes(x = steps)) +
geom_histogram(fill = "firebrick", binwidth = 1000) +
labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")

```

2) What is the average daily activity pattern?

```
mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)
mean_steps
median_steps

interval <- data %>%
filter(!is.na(steps)) %>%
group_by(interval) %>%
summarize(steps = mean(steps))

ggplot(interval, aes(x=interval, y=steps)) +
geom_line(color = "firebrick")

```
3) Imputing missing values

```
interval[which.max(interval$steps),]
sum(is.na(data$steps))
data_full <- data
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]
sum(is.na(data_full$steps))

steps_full <- data_full %>%
filter(!is.na(steps)) %>%
group_by(date) %>%
summarize(steps = sum(steps)) %>%
print

ggplot(steps_full, aes(x = steps)) +
geom_histogram(fill = "firebrick", binwidth = 1000) +
labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")

```
4) Are there differences in activity patterns between weekdays and weekends?

```
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)
mean_steps_full
median_steps_full
data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
data_full$weektype <- as.factor(data_full$weektype)
head(data_full)

interval_full <- data_full %>%
group_by(interval, weektype) %>%
summarize(steps = mean(steps))
s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
geom_line() +
facet_wrap(~weektype, ncol = 1, nrow = 2)
print(s)

```