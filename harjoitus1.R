## Coursera Reproducible Research course assignment 1

## This code requires plyr, lattice packages

## Read the source data
amd <- read.csv("activity.csv", header=TRUE)

## Convert the dates to correct format
amd$date <- as.Date(amd$date)

## Summarise the data with ddply, calculate the sum of steps per day and median and mean per day
sum_amd <- ddply(amd, "date", summarise, amdSum=sum(steps), amdMean=mean(steps), amdMedian=median(steps))

## Make a histogram of total amount of steps taken per day
hist(sum_amd$amdSum, xlab="Sum of steps taken", main="Total amount of steps taken per day")

## Print the sum, mean and median per day
sum_amd

## Calculate the mean number of steps taken per time interval, across all days
mean_interval <- ddply(amd, "interval", summarise, intervallMean=mean(steps, na.rm=TRUE))

## Plot the mean number of steps taken per time interval
plot(mean_interval$interval, mean_interval$intervallMean, type="l", xlab="Time interval", ylab="Mean amount of steps")

## Print the interval where the maximum number of steps occur
mean_interval[which.max(mean_interval$intervallMean),]

## Calculate the amount rows with missing values
sum(is.na(amd))

## Replace the missing values with the correct interval mean
amd[is.na(amd$steps) == "TRUE", 1] <- mean_interval$intervallMean

## Summarise the data with ddply, calculate the sum of steps per day and median and mean per day
sum_amd_new <- ddply(amd, "date", summarise, amdSum=sum(steps), amdMean=mean(steps), amdMedian=median(steps))

## Make a histogram of total amount of steps taken per day
hist(sum_amd_new$amdSum, xlab="Sum of steps taken", main="Total amount of steps taken per day (missing values removed)")

## Print the sum, mean and median per day
sum_amd_new

## Add the new factor variable, isWeekday
amd$isWeekday <- !(weekdays(sum_amd$date)) %in% c('Saturday','Sunday')

## Calculate the mean number of step per interval (use the new data with NAs replaced with correct interval mean)
steps_mean_wday <- ddply(amd, c("interval","isWeekday"), summarise, amd_Wday_mean=mean(steps))

## Plot the results
xyplot(amd_Wday_mean ~ interval | isWeekday  , data=steps_mean_wday, type="b", xlab = "Interval", ylab ="Mean steps per interval", main="Mean number of steps per interval", strip=strip.custom(factor.levels=c("Weekdays", "Weekends")),layout=c(1,2))
