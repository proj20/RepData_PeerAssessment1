## Introduction

# It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.
# This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

# The data for this assignment can be downloaded from the course web site, extracted with the following commands:

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile = "pamdata.zip")
unzip("pamdata.zip", exdir = getwd())



## Loading and preprocessing the data

if(!exists("PAM")){
        PAM <- read.csv("activity.csv", na.strings = "NA")
}

# The variables included in this dataset are:
# - steps: Number of steps taking in a 5-minute interval (missing values are coded as `NA`)
# - date: The date on which the measurement was taken in YYYY-MM-DD format
# - interval: Identifier for the 5-minute interval in which measurement was taken

# It appears that the `date` column is not in the proper data format, so we must convert it to `date`.
str(PAM)
PAM$date <- as.Date(PAM$date,"%Y-%m-%d")

# Now, all the data columns are in the correct format and is suitable for analysis.
str(PAM)


## Data exploration (without missing-value imputation)

# Histogram of the total number of steps taken each day
# Below aggregates `PAM` data into a daily mean steps summary, and plots the histogram.
total_step <- aggregate(steps ~ date, data = PAM, sum, na.rm = T)
total_step
par(mfrow=c(1,1))
hist(total_step$steps, main = "Histogram of Steps per Day", xlab = "total steps per day")

# Calculate the mean and median number of steps taken each day
mean(total_step$steps)
median(total_step$steps)

# Time series plot of the average number of steps taken
mean_step_interval <- aggregate(steps ~ interval, data = PAM, mean, na.rm = T)
par(mfrow=c(1,1))
plot(mean_step_interval$interval, mean_step_interval$steps,
     main = "Daily Average Activity Pattern",
     xlab = "date",
     ylab = "steps average",
     type = "l")

# The 5-minute interval that, on average, contains the maximum number of steps
max_step <- max(mean_step_interval$steps)
mean_step_interval$interval[mean_step_interval$steps==max_step]

## Imputing missing values
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
PAM_complete <- PAM[complete.cases(PAM),]
nrow(PAM) - nrow(PAM_complete)

# Devise a strategy for filling in all of the missing values in the dataset.
# The imputation strategy to be used will be to fill in NA values with the mean for that particular 5-minute interval.
PAM_imputed <- PAM # Copy the original data set
# Use the values from the `mean_step_interval` for filling in the values where the `steps` is `NA` and the `interval` matches
count = 0
for (i in 1:nrow(PAM)) {
        if (is.na(PAM$steps[i])) {
                PAM_imputed$steps[i] <- mean_step_interval[mean_step_interval$interval==PAM$interval[i], 2]
                count = count + 1
        }
}
cat(count, "NA values were successfully filled.\n\r")

# Make a histogram of the total number of steps taken each day (using imputed dataset)
total_step2 <- aggregate(steps ~ date, data = PAM_imputed, sum, na.rm = T)
head(total_step2)
par(mfrow=c(1,1))
hist(total_step2$steps, main = "Histogram of Steps per Day (imputed)", xlab = "total steps per day")

# Calculate the mean and median number of steps taken each day (imputed)
mean(total_step2$steps)
median(total_step2$steps)

# Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
# - mean did not change since the imputed values used were averages
# - median did not change significantly that could impact the actual estimates of location

## Are there differences in activity patterns between weekdays and weekends?
# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
for (i in 1:nrow(PAM_imputed)) {
        if (weekdays(PAM_imputed$date[i])=="Sunday" | weekdays(PAM_imputed$date[i])=="Saturday") {
                PAM_imputed$day_type[i] <- "weekend"
        }
        else {
                PAM_imputed$day_type[i] <- "weekday"
        }
}
PAM_imputed$day_type <- as.factor(PAM_imputed$day_type)

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
steps_day_type <- aggregate(steps ~ interval + day_type, PAM_imputed, mean)
library(lattice)
xyplot(steps ~ interval | factor(day_type), data = steps_day_type, aspect = 1/2, type = "l")
